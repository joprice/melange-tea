type unresolved
type xmlHttpRequestUpload
type event_readystatechange = Web_json.t
type event_abort = Web_json.t
type event_error = Web_json.t
type event_load = Web_json.t
type event_loadstart = Web_json.t
type event_progress = Web_json.t
type event_timeout = Web_json.t
type event_loadend = Web_json.t

(* class type _xmlhttprequest = object *)
(*   (* Methods *) *)
(*   method abort : unit -> unit [@@mel.meth] *)
(*   method getAllResponseHeaders : unit -> string Js.null *)
(*   method getResponseHeader : string -> string Js.null *)
(*   method _open : string -> string -> bool -> string -> string -> unit *)
(*   method overrideMimeType : string -> unit *)
(*   method send : unit -> unit *)
(*   method send__string : string Js.null -> unit *)
(*   method send__formdata : Web_formdata.t -> unit *)
(*   method send__document : Web_document.t -> unit *)
(*  *)
(*   (* method send_blob : Web_blob.t -> unit *) *)
(*   (* method send_arrayBufferView : Web_arraybuffer_view.t -> unit *) *)
(*   method setRequestHeader : string -> string -> unit *)
(*  *)
(*   (* Properties *) *)
(*   method onreadystatechange : event_readystatechange -> unit *)
(*   [@@mel.get] [@@mel.set] *)
(*  *)
(*   method readyState : int [@@mel.get] *)
(*   method responseType : string [@@mel.get] [@@mel.set] *)
(*   method response : unresolved Js.null [@@mel.get] *)
(*   method responseText : string [@@mel.get] *)
(*   method responseURL : string [@@mel.get] *)
(*   method responseXML : Web_document.t Js.null [@@mel.get] *)
(*   method status : int [@@mel.get] *)
(*   method statusText : string [@@mel.get] *)
(*   method timeout : float [@@mel.get] [@@mel.set] *)
(*   method upload : xmlHttpRequestUpload [@@mel.get] *)
(*   method withCredentials : bool [@@mel.get] [@@mel.set] *)
(*  *)
(*   (* Base events *) *)
(*   method onabort : event_abort -> unit [@@mel.get] [@@mel.set] *)
(*   method onerror : event_error -> unit [@@mel.get] [@@mel.set] *)
(*   method onload : event_load -> unit [@@mel.get] [@@mel.set] *)
(*   method onloadstart : event_loadstart -> unit [@@mel.get] [@@mel.set] *)
(*   method onprogress : event_loadstart -> unit [@@mel.get] [@@mel.set] *)
(*   method ontimeout : event_timeout -> unit [@@mel.get] [@@mel.set] *)
(*   method onloadend : event_loadend -> unit [@@mel.get] [@@mel.set] *)
(* end[@mel] *)
(*  *)
type t

external create : unit -> t = "XMLHttpRequest" [@@mel.new]

type errors = IncompleteResponse | NetworkError

type body =
  | EmptyBody
  | EmptyStringBody
  | StringBody of string
  | FormDataBody of Webapi.FormData.t
  | FormListBody of (string * string) list
  | DocumentBody of Web_document.t
(* | BlobBody of Web_blob.t *)
(* | ArrayBufferViewBody of Web_arraybuffer_view.t *)

(* Main interface functions *)

module Internal = struct
  external abort : t -> unit = "abort" [@@mel.send]

  external getAllResponseHeaders : t -> string Js.null = "getAllResponseHeaders"
  [@@mel.send]

  external overrideMimeType : string -> unit = "overrideMimeType"
  [@@mel.send.pipe: t]

  external send : t -> unit = "send" [@@mel.send]
  external send_string : string Js.null -> unit = "send" [@@mel.send.pipe: t]

  external send_formdata : Webapi.FormData.t -> unit = "send"
  [@@mel.send.pipe: t]

  external send_document : Web_document.t -> unit = "send" [@@mel.send.pipe: t]

  external setRequestHeader : string -> string -> unit = "setRequestHeader"
  [@@mel.send.pipe: t]

  type onreadystatechange = event_readystatechange -> unit

  external set_onreadystatechange : onreadystatechange -> unit
    = "onreadystatechange"
  [@@mel.send.pipe: t]

  external onreadystatechange : t -> onreadystatechange = "onreadystatechange"
  [@@mel.get]

  external readyState : t -> int = "readyState" [@@mel.get]
  external responseType : t -> string = "responseType" [@@mel.get]

  external set_responseType : string -> unit = "responseType"
  [@@mel.send.pipe: t]

  external response : t -> unresolved Js.null = "response" [@@mel.get]
  external responseText : t -> string = "responseText" [@@mel.get]
  external responseURL : t -> string = "responseURL" [@@mel.get]
  external responseXML : t -> Web_document.t Js.null = "responseXML" [@@mel.get]
  external status : t -> int = "status" [@@mel.get]
  external statusText : t -> string = "statusText" [@@mel.get]
  external set_timeout : float -> unit = "timeout" [@@mel.send.pipe: t]
  external timeout : t -> float = "timeout" [@@mel.get]

  external set_withCredentials : bool -> unit = "withCredentials"
  [@@mel.send.pipe: t]

  external withCredentials : t -> bool = "withCredentials" [@@mel.get]

  type 'a event = 'a -> unit

  external set_onabort : event_abort event -> unit = "onabort"
  [@@mel.send.pipe: t]

  external onabort : t -> event_abort event = "onabort" [@@mel.get]

  external set_onerror : event_error event -> unit = "onerror"
  [@@mel.send.pipe: t]

  external onerror : t -> event_error event = "onerror" [@@mel.get]
  external set_onload : event_load event -> unit = "onload" [@@mel.send.pipe: t]
  external onload : t -> event_load event = "onload" [@@mel.get]

  external set_onloadstart : event_loadstart event -> unit = "onloadstart"
  [@@mel.send.pipe: t]

  external onloadstart : t -> event_loadstart event = "onloadstart" [@@mel.get]

  external set_onprogress : event_progress event -> unit = "onprogress"
  [@@mel.send.pipe: t]

  external onprogress : t -> event_progress event = "onprogress" [@@mel.get]

  external set_ontimeout : event_timeout event -> unit = "ontimeout"
  [@@mel.send.pipe: t]

  external ontimeout : t -> event_timeout event = "ontimeout" [@@mel.get]

  external set_onloadend : event_loadend event -> unit = "onloadend"
  [@@mel.send.pipe: t]

  external onloadend : t -> event_loadend event = "onloadend" [@@mel.get]

  (* external set_onloadend : event_loadend event -> unit = "onloadend" *)
  (* [@@mel.send.pipe: t] *)
  (*  *)
  (* external onloadend : t -> event_loadend event = "onloadend" [@@mel.get] *)

  (*   method onloadstart : event_loadstart -> unit [@@mel.get] [@@mel.set] *)
  (*   method onprogress : event_loadstart -> unit [@@mel.get] [@@mel.set] *)
  (*   method ontimeout : event_timeout -> unit [@@mel.get] [@@mel.set] *)
  (*   method onreadystatechange : event_readystatechange -> unit *)
end

let abort (x : t) : unit = x |> Internal.abort

let getAllResponseHeaders (x : t) : (string, errors) Tea_result.t =
  let open Tea_result in
  match Js.Null.toOption (x |> Internal.getAllResponseHeaders) with
  | None -> Error IncompleteResponse
  | Some "" -> Error NetworkError
  | Some s -> Ok s

let getAllResponseHeadersAsList (x : t) :
    ((string * string) list, errors) Tea_result.t =
  let open Tea_result in
  match getAllResponseHeaders x with
  | Error _ as err -> err
  | Ok s ->
      Ok
        (s
        |> Js.String.split ~sep:"\r\n"
        |> Array.map (Js.String.split ~sep:": " ~limit:2)
        |> Array.to_list
        |> List.filter (fun a -> Array.length a == 2)
        |> List.map (function
             | [| key; value |] -> (key, value)
             | _ -> failwith "Cannot happen, already checked length"))

let getAllResponseHeadersAsDict (x : t) :
    (string Map.Make(String).t, errors) Tea_result.t =
  let module StringMap = Map.Make (String) in
  match getAllResponseHeadersAsList x with
  | Tea_result.Error _ as err -> err
  | Tea_result.Ok l ->
      let insert d (k, v) = StringMap.add k v d in
      Tea_result.Ok (List.fold_left insert StringMap.empty l)

let getResponseHeader key x = Js.Null.toOption (x##getResponse key)

let open_ (method' : string) (url : string) ?(async = true) ?(user = "")
    ?(password = "") x =
  x##_open method' url async user password

let overrideMimeType (mimetype : string) (x : t) : unit =
  x |> Internal.overrideMimeType mimetype

let send (body : body) (x : t) : unit =
  match body with
  | EmptyBody -> x |> Internal.send
  | EmptyStringBody -> x |> Internal.send_string Js.Null.empty
  | StringBody s -> x |> Internal.send_string (Js.Null.return s)
  | FormDataBody f -> x |> Internal.send_formdata f
  | FormListBody l ->
      let form =
        List.fold_left
          (fun f (key, value) ->
            let () = Webapi.FormData.append key value f in
            f)
          (Webapi.FormData.make ()) l
      in
      x |> Internal.send_formdata form
  | DocumentBody d -> x |> Internal.send_document d
(* | BlobBody b -> x##send_blob b *)
(* | ArrayBufferViewBody a -> x##send_arrayBufferView a *)

let setRequestHeader (header : string) (value : string) (x : t) =
  x |> Internal.setRequestHeader header value

(* Properties *)

type state = Unsent | Opened | HeadersReceived | Loading | Done

type responseType =
  | StringResponseType
  | ArrayBufferResponseType
  | BlobResponseType
  | DocumentResponseType
  | JsonResponseType
  | TextResponseType
  | RawResponseType of string

type responseBody =
  | NoResponse
  | StringResponse of string
  | ArrayBufferResponse of unit
  | BlobResponse of unit
  | DocumentResponse of Web_document.t
  | JsonResponse of Web_json.t
  | TextResponse of string
  | RawResponse of string * unit

let set_onreadystatechange (cb : event_readystatechange -> unit) (x : t) : unit
    =
  x |> Internal.set_onreadystatechange cb

let get_onreadystatechange (x : t) : event_readystatechange -> unit =
  x |> Internal.onreadystatechange

let readyState (x : t) : state =
  match x |> Internal.readyState with
  | 0 -> Unsent
  | 1 -> Opened
  | 2 -> HeadersReceived
  | 3 -> Loading
  | 4 -> Done
  | i -> failwith ("Invalid return from 'readystate' of: " ^ string_of_int i)

let set_responseType (typ : responseType) (x : t) : unit =
  match typ with
  | StringResponseType -> x |> Internal.set_responseType ""
  | ArrayBufferResponseType -> x |> Internal.set_responseType "arraybuffer"
  | BlobResponseType -> x |> Internal.set_responseType "blob"
  | DocumentResponseType -> x |> Internal.set_responseType "document"
  | JsonResponseType -> x |> Internal.set_responseType "json"
  | TextResponseType -> x |> Internal.set_responseType "text"
  | RawResponseType s -> x |> Internal.set_responseType s

let get_responseType (x : t) : responseType =
  match x |> Internal.responseType with
  | "" -> StringResponseType
  | "arraybuffer" -> ArrayBufferResponseType
  | "blob" -> BlobResponseType
  | "document" -> DocumentResponseType
  | "json" -> JsonResponseType
  | "text" -> TextResponseType
  | s -> RawResponseType s

let get_response (x : t) : responseBody =
  match Js.Null.toOption (x |> Internal.response) with
  | None -> NoResponse
  | Some resp -> (
      match get_responseType x with
      | StringResponseType -> StringResponse (Obj.magic resp)
      | ArrayBufferResponseType -> ArrayBufferResponse (Obj.magic resp)
      | BlobResponseType -> BlobResponse (Obj.magic resp)
      | DocumentResponseType -> DocumentResponse (Obj.magic resp)
      | JsonResponseType -> JsonResponse (Obj.magic resp)
      | TextResponseType -> TextResponse (Obj.magic resp)
      | RawResponseType s -> RawResponse (s, Obj.magic resp))

let get_responseText (x : t) : string = x |> Internal.responseText
let get_responseURL (x : t) : string = x |> Internal.responseURL

let get_responseXML (x : t) : Web_document.t option =
  Js.Null.toOption (x |> Internal.responseXML)

let get_status (x : t) : int = x |> Internal.status
let get_statusText (x : t) : string = x |> Internal.statusText
let set_timeout (t : float) (x : t) : unit = x |> Internal.set_timeout t
let get_timeout (x : t) : float = x |> Internal.timeout

let set_withCredentials (b : bool) (x : t) : unit =
  x |> Internal.set_withCredentials b

let get_withCredentials (x : t) : bool = x |> Internal.withCredentials

let set_onabort (cb : event_abort -> unit) (x : t) : unit =
  x |> Internal.set_onabort cb

let get_onabort (x : t) : event_abort -> unit = x |> Internal.onabort

let set_onerror (cb : event_error -> unit) (x : t) : unit =
  x |> Internal.set_onerror cb

let get_onerror (x : t) : event_error -> unit = x |> Internal.onerror

let set_onload (cb : event_load -> unit) (x : t) : unit =
  x |> Internal.set_onload cb

let get_onload (x : t) : event_load -> unit = x |> Internal.onload

let set_onloadstart (cb : event_loadstart -> unit) (x : t) : unit =
  x |> Internal.set_onloadstart cb

let get_onloadstart (x : t) : event_loadstart -> unit =
  x |> Internal.onloadstart

let set_onprogress (cb : event_loadstart -> unit) (x : t) : unit =
  x |> Internal.set_onprogress cb

let get_onprogress (x : t) : event_loadstart -> unit = x |> Internal.onprogress

let set_ontimeout (cb : event_timeout -> unit) (x : t) : unit =
  x |> Internal.set_ontimeout cb

let get_ontimeout (x : t) : event_timeout -> unit = x |> Internal.ontimeout

let set_onloadend (cb : event_loadend -> unit) (x : t) : unit =
  x |> Internal.set_onloadend cb

let get_onloadend (x : t) : event_loadend -> unit = x |> Internal.onloadend
