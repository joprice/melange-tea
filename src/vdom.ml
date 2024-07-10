type eventCallback = Dom.event -> unit

type 'msg systemMessage =
  | Render
  | AddRenderMsg of 'msg
  | RemoveRenderMsg of 'msg

type 'msg applicationCallbacks =
  {enqueue: 'msg -> unit; on: 'msg systemMessage -> unit}

type 'msg eventHandler =
  | EventHandlerCallback of string * (Dom.event -> 'msg option)
  | EventHandlerMsg of 'msg

type 'msg eventCache =
  {handler: eventCallback; cb: (Dom.event -> 'msg option) ref}

type 'msg property =
  | NoProp
  | RawProp of string * string
  | Attribute of string * string * string
  | Data of string * string
  | Event of string * 'msg eventHandler * 'msg eventCache option ref
  | Style of (string * string) list

type 'msg properties = 'msg property list

type 'msg t =
  | CommentNode of string
  | Text of string
  | Node of string * string * string * string * 'msg properties * 'msg t list
  | LazyGen of string * (unit -> 'msg t) * 'msg t ref
  | Tagger of
      ('msg applicationCallbacks ref -> 'msg applicationCallbacks ref) * 'msg t

let noNode = ((CommentNode "" : 'msg t) : 'msg t)

let comment (s : string) : 'msg t = CommentNode s

let text (s : string) : 'msg t = Text s

let fullnode (namespace : string) (tagName : string) (key : string)
    (unique : string) (props : 'msg properties) (vdoms : 'msg t list) : 'msg t =
  Node (namespace, tagName, key, unique, props, vdoms)

let node ?(namespace : string = "") (tagName : string) ?(key : string = "")
    ?(unique : string = "") (props : 'msg properties) (vdoms : 'msg t list) :
    'msg t =
  fullnode namespace tagName key unique props vdoms

let lazyGen (key : string) (fn : unit -> 'msg t) : 'msg t =
  LazyGen (key, fn, ref noNode)

let noProp = ((NoProp : 'msg property) : 'msg property)

let prop (key : string) (value : string) : 'msg property = RawProp (key, value)

let onCB (name : string) (key : string) (cb : Dom.event -> 'msg option) :
    'msg property =
  Event (name, EventHandlerCallback (key, cb), ref None)

let onMsg (name : string) (msg : 'msg) : 'msg property =
  Event (name, EventHandlerMsg msg, ref None)

let attribute (namespace : string) (key : string) (value : string) :
    'msg property =
  Attribute (namespace, key, value)

let data (key : string) (value : string) : 'msg property = Data (key, value)

let style (key : string) (value : string) : 'msg property = Style [(key, value)]

let styles s : 'msg property = Style s

let createElementNsOptional namespace tagName =
  let document = Webapi.Dom.document in
  match namespace with
  | "" ->
      Webapi.Dom.Document.createElement tagName document
  | ns ->
      Webapi.Dom.Document.createElementNS ns tagName document

let nodeAt (index : int) (nodes : Dom.nodeList) : Dom.node =
  Webapi.Dom.NodeList.item index nodes |> Option.get

external getProp : Dom.element -> 'key -> 'value = "" [@@mel.get_index]

external setProp : Dom.element -> 'key -> 'value -> unit = "" [@@mel.set_index]

let rec renderToHtmlString =
  ( ( function
      | CommentNode s ->
          "<!-- " ^ s ^ " -->"
      | Text s ->
          s
      | Node (namespace, tagName, _key, _unique, props, vdoms) ->
          let renderProp = function
            | NoProp ->
                ""
            | RawProp (k, v) ->
                String.concat "" [" "; k; "=\""; v; "\""]
            | Attribute (_namespace, k, v) ->
                String.concat "" [" "; k; "=\""; v; "\""]
            | Data (k, v) ->
                String.concat "" [" data-"; k; "=\""; v; "\""]
            | Event (_, _, _) ->
                ""
            | Style s ->
                String.concat ""
                  [ " style=\""
                  ; String.concat ";"
                      (List.map
                         (fun (k, v) -> String.concat "" [k; ":"; v; ";"])
                         s )
                  ; "\"" ]
          in
          String.concat ""
            [ "<"
            ; namespace
            ; (if namespace = "" then "" else ":")
            ; tagName
            ; String.concat "" (List.map (fun p -> renderProp p) props)
            ; ">"
            ; String.concat "" (List.map (fun v -> renderToHtmlString v) vdoms)
            ; "</"
            ; tagName
            ; ">" ]
      | LazyGen (_key, gen, _cache) ->
          let vdom = gen () in
          renderToHtmlString vdom
      | Tagger (_tagger, vdom) ->
          renderToHtmlString vdom
      : 'msg t -> string )
    : 'msg t -> string )

let emptyEventHandler = ((fun [@mel] _ev -> () : eventCallback) : eventCallback)

let emptyEventCB _ev : eventCallback option = None

let eventHandler (callbacks : 'msg applicationCallbacks ref)
    (cb : (Dom.event -> 'msg option) ref) : eventCallback =
 fun [@mel] ev ->
  match !cb ev with None -> () | Some msg -> !callbacks.enqueue msg

let eventHandler_GetCB =
  ( ( function
      | EventHandlerCallback (_, cb) ->
          cb
      | EventHandlerMsg msg ->
          fun _ev -> Some msg
      : 'msg eventHandler -> Dom.event -> 'msg option )
    : 'msg eventHandler -> Dom.event -> 'msg option )

let compareEventHandlerTypes (left : 'msg eventHandler) :
    'msg eventHandler -> bool = function
  | EventHandlerCallback (cb, _) -> (
    match left with
    | EventHandlerCallback (lcb, _) when cb = lcb ->
        true
    | _ ->
        false )
  | EventHandlerMsg msg -> (
    match left with EventHandlerMsg lmsg when msg = lmsg -> true | _ -> false )

let eventHandler_Register (callbacks : 'msg applicationCallbacks ref)
    (elem : Dom.eventTarget) (name : string) (handlerType : 'msg eventHandler) :
    'msg eventCache option =
  let cb = ref (eventHandler_GetCB handlerType) in
  let handler = eventHandler callbacks cb in
  let () =
    Webapi.Dom.Element.addEventListener name handler
      (Webapi.Dom.EventTarget.unsafeAsElement elem)
  in
  Some {handler; cb}

let eventHandler_Unregister (elem : Dom.eventTarget) (name : string) :
    'msg eventCache option -> 'msg eventCache option = function
  | None ->
      None
  | Some cache ->
      let () =
        Webapi.Dom.Element.removeEventListener name cache.handler
          (Webapi.Dom.EventTarget.unsafeAsElement elem)
      in
      None

let eventHandler_Mutate (callbacks : 'msg applicationCallbacks ref)
    (elem : Dom.eventTarget) (oldName : string) (newName : string)
    (oldHandlerType : 'msg eventHandler) (newHandlerType : 'msg eventHandler)
    (oldCache : 'msg eventCache option ref)
    (newCache : 'msg eventCache option ref) : unit =
  match !oldCache with
  | None ->
      newCache := eventHandler_Register callbacks elem newName newHandlerType
  | Some oldcache ->
      if oldName = newName then
        let () = newCache := !oldCache in
        if compareEventHandlerTypes oldHandlerType newHandlerType then ()
        else
          let cb = eventHandler_GetCB newHandlerType in
          let () = oldcache.cb := cb in
          ()
      else
        let () = oldCache := eventHandler_Unregister elem oldName !oldCache in
        let () =
          newCache :=
            eventHandler_Register callbacks elem newName newHandlerType
        in
        ()

let patchVNodesOnElems_PropertiesApply_Add
    (callbacks : 'msg applicationCallbacks ref) (elem : Dom.element) (_idx : int)
    : 'msg property -> unit = function
  | NoProp ->
      ()
  | RawProp (k, v) ->
      setProp elem k v
  | Attribute (namespace, k, v) ->
      Webapi.Dom.Element.setAttributeNS namespace k v elem
  | Data (k, v) ->
      Js.log ("TODO:  Add Data Unhandled", k, v) ;
      failwith "TODO:  Add Data Unhandled"
  | Event (name, handlerType, cache) ->
      cache :=
        eventHandler_Register callbacks
          (Webapi.Dom.Element.asEventTarget elem)
          name handlerType
  | Style s -> (
    match Webapi.Dom.HtmlElement.ofElement elem with
    | Some elem ->
        let elemStyle = Webapi.Dom.HtmlElement.style elem in
        List.fold_left
          (fun () (k, v) ->
            Webapi.Dom.CssStyleDeclaration.setProperty k v "" elemStyle )
          () s
    | None ->
        failwith
          "Expected htmlelement in patchVNodesOnElems_PropertiesApply_Add" )

let patchVNodesOnElems_PropertiesApply_Remove
    (_callbacks : 'msg applicationCallbacks ref) (elem : Dom.element)
    (_idx : int) : 'msg property -> unit = function
  | NoProp ->
      ()
  | RawProp (k, _v) ->
      setProp elem k Js.Undefined.empty
  | Attribute (namespace, k, _v) ->
      Webapi.Dom.Element.removeAttributeNS namespace k elem
  | Data (k, v) ->
      Js.log ("TODO:  Remove Data Unhandled", k, v) ;
      failwith "TODO:  Remove Data Unhandled"
  | Event (name, _, cache) ->
      cache :=
        eventHandler_Unregister
          (Webapi.Dom.Element.asEventTarget elem)
          name !cache
  | Style s -> (
    match Webapi.Dom.HtmlElement.ofElement elem with
    | Some elem ->
        let elemStyle = Webapi.Dom.HtmlElement.style elem in
        List.fold_left
          (fun () (k, _v) ->
            Webapi.Dom.CssStyleDeclaration.setProperty k "" "" elemStyle )
          () s
    | None ->
        failwith
          "Expected htmlelement in patchVNodesOnElems_PropertiesApply_remove" )

let patchVNodesOnElems_PropertiesApply_RemoveAdd
    (callbacks : 'msg applicationCallbacks ref) (elem : Dom.element) (idx : int)
    (oldProp : 'msg property) (newProp : 'msg property) : unit =
  let () =
    patchVNodesOnElems_PropertiesApply_Remove callbacks elem idx oldProp
  in
  let () = patchVNodesOnElems_PropertiesApply_Add callbacks elem idx newProp in
  ()

let patchVNodesOnElems_PropertiesApply_Mutate
    (_callbacks : 'msg applicationCallbacks ref) (elem : Dom.element)
    (_idx : int) (oldProp : 'msg property) : 'msg property -> unit = function
  | NoProp as _newProp ->
      failwith
        "This should never be called as all entries through NoProp are gated."
  | RawProp (k, v) as _newProp ->
      setProp elem k v
  | Attribute (namespace, k, v) as _newProp ->
      Webapi.Dom.Element.setAttributeNS namespace k v elem
  | Data (k, v) as _newProp ->
      Js.log ("TODO:  Mutate Data Unhandled", k, v) ;
      failwith "TODO:  Mutate Data Unhandled"
  | Event (_newName, _newHandlerType, _newCache) as _newProp ->
      failwith "This will never be called because it is gated"
  | Style s as _newProp -> (
    match[@ocaml.warning "-4"] oldProp with
    | Style oldS -> (
      match Webapi.Dom.HtmlElement.ofElement elem with
      | Some elem ->
          List.fold_left2
            (fun () (ok, ov) (nk, nv) ->
              let elemStyle = Webapi.Dom.HtmlElement.style elem in
              if ok = nk then
                if ov = nv then ()
                else
                  Webapi.Dom.CssStyleDeclaration.setProperty nk nv "" elemStyle
              else
                let () =
                  Webapi.Dom.CssStyleDeclaration.setProperty ok "" "" elemStyle
                in
                Webapi.Dom.CssStyleDeclaration.setProperty nk nv "" elemStyle )
            () oldS s
      | None ->
          failwith "Expected htmlelement in patchVNodesOnElems" )
    | _ ->
        failwith
          "Passed a non-Style to a new Style as a Mutations while the old \
           Style is not actually a style!" )

let rec patchVNodesOnElems_PropertiesApply
    (callbacks : 'msg applicationCallbacks ref) (elem : Dom.element) (idx : int)
    (oldProperties : 'msg property list) (newProperties : 'msg property list) :
    bool =
  match[@ocaml.warning "-4"] (oldProperties, newProperties) with
  | [], [] ->
      true
  | [], _newProp :: _newRest ->
      false
  | _oldProp :: _oldRest, [] ->
      false
  | NoProp :: oldRest, NoProp :: newRest ->
      patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
        newRest
  | ( (RawProp (oldK, oldV) as oldProp) :: oldRest
    , (RawProp (newK, newV) as newProp) :: newRest ) ->
      let () =
        if oldK = newK && oldV = newV then ()
        else
          patchVNodesOnElems_PropertiesApply_Mutate callbacks elem idx oldProp
            newProp
      in
      patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
        newRest
  | ( (Attribute (oldNS, oldK, oldV) as oldProp) :: oldRest
    , (Attribute (newNS, newK, newV) as newProp) :: newRest ) ->
      let () =
        if oldNS = newNS && oldK = newK && oldV = newV then ()
        else
          patchVNodesOnElems_PropertiesApply_Mutate callbacks elem idx oldProp
            newProp
      in
      patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
        newRest
  | ( (Data (oldK, oldV) as oldProp) :: oldRest
    , (Data (newK, newV) as newProp) :: newRest ) ->
      let () =
        if oldK = newK && oldV = newV then ()
        else
          patchVNodesOnElems_PropertiesApply_Mutate callbacks elem idx oldProp
            newProp
      in
      patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
        newRest
  | ( (Event (oldName, oldHandlerType, oldCache) as _oldProp) :: oldRest
    , (Event (newName, newHandlerType, newCache) as _newProp) :: newRest ) ->
      let () =
        eventHandler_Mutate callbacks
          (Webapi.Dom.Element.asEventTarget elem)
          oldName newName oldHandlerType newHandlerType oldCache newCache
      in
      patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
        newRest
  | (Style oldS as oldProp) :: oldRest, (Style newS as newProp) :: newRest ->
      let () =
        if oldS = newS then ()
        else
          patchVNodesOnElems_PropertiesApply_Mutate callbacks elem idx oldProp
            newProp
      in
      patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
        newRest
  | oldProp :: oldRest, newProp :: newRest ->
      let () =
        patchVNodesOnElems_PropertiesApply_RemoveAdd callbacks elem idx oldProp
          newProp
      in
      patchVNodesOnElems_PropertiesApply callbacks elem (idx + 1) oldRest
        newRest

let patchVNodesOnElems_Properties (callbacks : 'msg applicationCallbacks ref)
    (elem : Dom.element) (oldProperties : 'msg property list)
    (newProperties : 'msg property list) : bool =
  patchVNodesOnElems_PropertiesApply callbacks elem 0 oldProperties
    newProperties

let genEmptyProps (length : int) : 'msg property list =
  let rec aux lst = function
    | 0 ->
        lst
    | len ->
        aux (noProp :: lst) (len - 1)
  in
  aux [] length

let mapEmptyProps (props : 'msg property list) : 'msg property list =
  List.map (fun _ -> noProp) props

let rec patchVNodesOnElems_ReplaceNode
    (callbacks : 'msg applicationCallbacks ref) (elem : Dom.node)
    (elems : Dom.nodeList) (idx : int) : 'msg t -> unit =
  function[@ocaml.warning "-4"]
  | Node
      (newNamespace, newTagName, _newKey, _newUnique, newProperties, newChildren)
    ->
      let oldChild = nodeAt idx elems in
      let newChild = createElementNsOptional newNamespace newTagName in
      let true =
        patchVNodesOnElems_Properties callbacks newChild
          (mapEmptyProps newProperties)
          newProperties
          [@@ocaml.warning "-8"]
      in
      let newChildNode = Webapi.Dom.Element.asNode newChild in
      let childChildren = Webapi.Dom.Node.childNodes newChildNode in
      let () =
        patchVNodesOnElems callbacks newChildNode childChildren 0 [] newChildren
      in
      let _attachedChild =
        Webapi.Dom.Node.insertBefore newChild oldChild elem
      in
      let _removedChild = Webapi.Dom.Node.removeChild oldChild elem in
      ()
  | _ ->
      failwith
        "Node replacement should never be passed anything but a node itself"

and patchVNodesOnElems_CreateElement (callbacks : 'msg applicationCallbacks ref)
    : 'msg t -> Dom.node = function
  | CommentNode s ->
      Webapi.Dom.Document.createComment s Webapi.Dom.document
      |> Webapi.Dom.Comment.asNode
  | Text text ->
      Webapi.Dom.Document.createTextNode text Webapi.Dom.document
      |> Webapi.Dom.Text.asNode
  | Node (newNamespace, newTagName, _newKey, _unique, newProperties, newChildren)
    ->
      let newChild = createElementNsOptional newNamespace newTagName in
      let true =
        patchVNodesOnElems_Properties callbacks newChild
          (mapEmptyProps newProperties)
          newProperties
          [@@ocaml.warning "-8"]
      in
      let newChildNode = Webapi.Dom.Element.asNode newChild in
      let childChildren = Webapi.Dom.Node.childNodes newChildNode in
      let () =
        patchVNodesOnElems callbacks newChildNode childChildren 0 [] newChildren
      in
      newChildNode
  | LazyGen (_newKey, newGen, newCache) ->
      let vdom = newGen () in
      let () = newCache := vdom in
      patchVNodesOnElems_CreateElement callbacks vdom
  | Tagger (tagger, vdom) ->
      patchVNodesOnElems_CreateElement (tagger callbacks) vdom

and patchVNodesOnElems_MutateNode (callbacks : 'msg applicationCallbacks ref)
    (elem : Dom.node) (elems : Dom.nodeList) (idx : int) (oldNode : 'msg t)
    (newNode : 'msg t) : unit =
  match (oldNode, newNode) with
  | ( ( Node
          ( _oldNamespace
          , oldTagName
          , _oldKey
          , oldUnique
          , oldProperties
          , oldChildren ) as _oldNode )
    , ( Node
          ( _newNamespace
          , newTagName
          , _newKey
          , newUnique
          , newProperties
          , newChildren ) as newNode ) ) -> (
      if oldUnique <> newUnique || oldTagName <> newTagName then
        patchVNodesOnElems_ReplaceNode callbacks elem elems idx newNode
      else
        let child = nodeAt idx elems in
        match Webapi.Dom.Element.ofNode child with
        | None ->
            failwith "Non-element passed to patchVNodesOnElems_MutateNode"
        | Some childElem ->
            let childChildren = Webapi.Dom.Node.childNodes child in
            let () =
              if
                patchVNodesOnElems_Properties callbacks childElem oldProperties
                  newProperties
              then ()
              else
                let () =
                  Js.log
                    "VDom:  Failed swapping properties because the property \
                     list length changed, use `noProp` to swap properties \
                     instead, not by altering the list structure.  This is a \
                     massive inefficiency until this issue is resolved."
                in
                patchVNodesOnElems_ReplaceNode callbacks elem elems idx newNode
            in
            patchVNodesOnElems callbacks child childChildren 0 oldChildren
              newChildren )
  | _ ->
      failwith "Non-node passed to patchVNodesOnElems_MutateNode"

(* NOTE: to be conformant with rescript-tea, `elem` should be a `Dom.node`; however the compiler
   says it can't and I can't understand why yet *)
and patchVNodesOnElems (callbacks : 'msg applicationCallbacks ref)
    (elem : Dom.node) (elems : Dom.nodeList) (idx : int)
    (oldVNodes : 'msg t list) (newVNodes : 'msg t list) : unit =
  match[@ocaml.warning "-4"] (oldVNodes, newVNodes) with
  | Tagger (_oldTagger, oldVdom) :: oldRest, _ ->
      patchVNodesOnElems callbacks elem elems idx (oldVdom :: oldRest) newVNodes
  | oldNode :: oldRest, Tagger (newTagger, newVdom) :: newRest ->
      let () =
        patchVNodesOnElems (newTagger callbacks) elem elems idx [oldNode]
          [newVdom]
      in
      patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
  | [], [] ->
      ()
  | [], newNode :: newRest ->
      let newChild = patchVNodesOnElems_CreateElement callbacks newNode in
      let _attachedChild = Webapi.Dom.Node.appendChild newChild elem in
      patchVNodesOnElems callbacks elem elems (idx + 1) [] newRest
  | _oldVnode :: oldRest, [] ->
      let child = nodeAt idx elems in
      let _removedChild = Webapi.Dom.Node.removeChild child elem in
      patchVNodesOnElems callbacks elem elems idx oldRest []
  | CommentNode oldS :: oldRest, CommentNode newS :: newRest when oldS = newS ->
      patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
  | Text oldText :: oldRest, Text newText :: newRest ->
      let () =
        if oldText = newText then ()
        else
          let child = nodeAt idx elems in
          Webapi.Dom.Node.setNodeValue child (Js.Null.return newText)
      in
      patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
  | ( LazyGen (oldKey, _oldGen, oldCache) :: oldRest
    , LazyGen (newKey, newGen, newCache) :: newRest ) -> (
      if oldKey = newKey then
        let () = newCache := !oldCache in
        patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
      else
        match (oldRest, newRest) with
        | ( LazyGen (olderKey, _olderGen, _olderCache) :: olderRest
          , LazyGen (newerKey, _newerGen, _newerCache) :: newerRest )
          when olderKey = newKey && oldKey = newerKey ->
            let firstChild = nodeAt idx elems in
            let secondChild = nodeAt (idx + 1) elems in
            let _removedChild = Webapi.Dom.Node.removeChild secondChild elem in
            let _attachedChild =
              Webapi.Dom.Node.insertBefore secondChild firstChild elem
            in
            patchVNodesOnElems callbacks elem elems (idx + 2) olderRest
              newerRest
        | LazyGen (olderKey, _olderGen, olderCache) :: olderRest, _
          when olderKey = newKey ->
            let oldChild = nodeAt idx elems in
            let _removedChild = Webapi.Dom.Node.removeChild oldChild elem in
            let oldVdom = !olderCache in
            let () = newCache := oldVdom in
            patchVNodesOnElems callbacks elem elems (idx + 1) olderRest newRest
        | _, LazyGen (newerKey, _newerGen, _newerCache) :: _newerRest
          when newerKey = oldKey ->
            let oldChild = nodeAt idx elems in
            let newVdom = newGen () in
            let () = newCache := newVdom in
            let newChild = patchVNodesOnElems_CreateElement callbacks newVdom in
            let _attachedChild =
              Webapi.Dom.Node.insertBefore newChild oldChild elem
            in
            patchVNodesOnElems callbacks elem elems (idx + 1) oldVNodes newRest
        | _ ->
            let oldVdom = !oldCache in
            let newVdom = newGen () in
            let () = newCache := newVdom in
            patchVNodesOnElems callbacks elem elems idx (oldVdom :: oldRest)
              (newVdom :: newRest) )
  | ( ( Node
          ( oldNamespace
          , oldTagName
          , oldKey
          , _oldUnique
          , _oldProperties
          , _oldChildren ) as oldNode )
      :: oldRest
    , ( Node
          ( newNamespace
          , newTagName
          , newKey
          , _newUnique
          , _newProperties
          , _newChildren ) as newNode )
      :: newRest ) -> (
      if oldKey = newKey && oldKey <> "" then
        patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
      else if oldKey = "" || newKey = "" then
        let () =
          patchVNodesOnElems_MutateNode callbacks elem elems idx oldNode newNode
        in
        patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
      else
        match (oldRest, newRest) with
        | ( Node
              ( olderNamespace
              , olderTagName
              , olderKey
              , _olderUnique
              , _olderProperties
              , _olderChildren )
            :: olderRest
          , Node
              ( newerNamespace
              , newerTagName
              , newerKey
              , _newerUnique
              , _newerProperties
              , _newerChildren )
            :: newerRest )
          when olderNamespace = newNamespace
               && olderTagName = newTagName && olderKey = newKey
               && oldNamespace = newerNamespace
               && oldTagName = newerTagName && oldKey = newerKey ->
            let firstChild = nodeAt idx elems in
            let secondChild = nodeAt (idx + 1) elems in
            let _removedChild = Webapi.Dom.Node.removeChild secondChild elem in
            let _attachedChild =
              Webapi.Dom.Node.insertBefore secondChild firstChild elem
            in
            patchVNodesOnElems callbacks elem elems (idx + 2) olderRest
              newerRest
        | ( Node
              ( olderNamespace
              , olderTagName
              , olderKey
              , _olderUnique
              , _olderProperties
              , _olderChildren )
            :: olderRest
          , _ )
          when olderNamespace = newNamespace
               && olderTagName = newTagName && olderKey = newKey ->
            let oldChild = nodeAt idx elems in
            let _removedChild = Webapi.Dom.Node.removeChild oldChild elem in
            patchVNodesOnElems callbacks elem elems (idx + 1) olderRest newRest
        | ( _
          , Node
              ( newerNamespace
              , newerTagName
              , newerKey
              , _newerUnique
              , _newerProperties
              , _newerChildren )
            :: _newerRest )
          when oldNamespace = newerNamespace
               && oldTagName = newerTagName && oldKey = newerKey ->
            let oldChild = nodeAt idx elems in
            let newChild = patchVNodesOnElems_CreateElement callbacks newNode in
            let _attachedChild =
              Webapi.Dom.Node.insertBefore newChild oldChild elem
            in
            patchVNodesOnElems callbacks elem elems (idx + 1) oldVNodes newRest
        | _ ->
            let () =
              patchVNodesOnElems_MutateNode callbacks elem elems idx oldNode
                newNode
            in
            patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest )
  | _oldVnode :: oldRest, newNode :: newRest ->
      let oldChild = nodeAt idx elems in
      let newChild = patchVNodesOnElems_CreateElement callbacks newNode in
      let _attachedChild =
        Webapi.Dom.Node.insertBefore newChild oldChild elem
      in
      let _removedChild = Webapi.Dom.Node.removeChild oldChild elem in
      patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest

let patchVNodesIntoElement (callbacks : 'msg applicationCallbacks ref)
    (elem : Dom.node) (oldVNodes : 'msg t list) (newVNodes : 'msg t list) :
    'msg t list =
  let elems = Webapi.Dom.Node.childNodes elem in
  let () = patchVNodesOnElems callbacks elem elems 0 oldVNodes newVNodes in
  newVNodes

let patchVNodeIntoElement (callbacks : 'msg applicationCallbacks ref)
    (elem : Dom.node) (oldVNode : 'msg t) (newVNode : 'msg t) : 'msg t list =
  patchVNodesIntoElement callbacks elem [oldVNode] [newVNode]

let wrapCallbacks_On : type a b. (a -> b) -> a systemMessage -> b systemMessage
    =
 fun func -> function
  | Render ->
      Render
  | AddRenderMsg msg ->
      AddRenderMsg (func msg)
  | RemoveRenderMsg msg ->
      RemoveRenderMsg (func msg)

let wrapCallbacks : type a b.
    (a -> b) -> b applicationCallbacks ref -> a applicationCallbacks ref =
 fun func callbacks ->
  Obj.magic ref
    { enqueue=
        (fun (msg : a) ->
          let new_msg = func msg in
          !callbacks.enqueue new_msg )
    ; on=
        (fun smsg ->
          let new_smsg = wrapCallbacks_On func smsg in
          !callbacks.on new_smsg ) }

let map =
  ( ( fun func vdom ->
        let tagger = wrapCallbacks func in
        Tagger (Obj.magic tagger, Obj.magic vdom)
      : ('a -> 'b) -> 'a t -> 'b t )
    : ('a -> 'b) -> 'a t -> 'b t )
