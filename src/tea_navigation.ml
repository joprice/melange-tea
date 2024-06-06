type ('flags, 'model, 'msg) navigationProgram =
  { init: 'flags -> 'model * 'msg Tea_cmd.t
  ; update: 'model -> 'msg -> 'model * 'msg Tea_cmd.t
  ; view: 'model -> 'msg Vdom.t
  ; subscriptions: 'model -> 'msg Tea_sub.t
  ; shutdown: 'model -> 'msg Tea_cmd.t }

let getLocation () = Webapi.Dom.(location)

open struct
  let notifier : (Webapi.Dom.Location.t -> unit) option ref = ref None

  let notifyUrlChange () =
    match !notifier with
    | None ->
        ()
    | Some cb ->
        let location = getLocation () in
        let () = cb location in
        ()

  let subscribe tagger =
    let open Vdom in
    let enableCall callbacks =
      let notifyHandler location = callbacks.enqueue (tagger location) in
      let () = notifier := Some notifyHandler in
      let handler : Vdom.eventCallback =
       fun [@mel] _event -> notifyUrlChange ()
      in
      let () =
        Webapi.Dom.Window.addEventListener "popstate" handler Webapi.Dom.window
      in
      fun () ->
        Webapi.Dom.Window.removeEventListener "popstate" handler
          Webapi.Dom.window
    in
    Tea_sub.registration "navigation" enableCall

  external historyState : 'a Js.Dict.t -> Webapi.Dom.History.state = "%identity"

  let replaceState url =
    let _ =
      Webapi.Dom.history
      |> Webapi.Dom.History.replaceState
           (historyState (Js.Dict.empty ()))
           "" url
    in
    ()

  let pushState url =
    let _ =
      Webapi.Dom.history
      |> Webapi.Dom.History.pushState (historyState (Js.Dict.empty ())) "" url
    in
    ()
end

let modifyUrl url =
  Tea_cmd.call (fun _enqueue ->
      let () = replaceState url in
      let () = notifyUrlChange () in
      () )

let newUrl url =
  Tea_cmd.call (fun _enqueue ->
      let () = pushState url in
      let () = notifyUrlChange () in
      () )

let go step =
  Tea_cmd.call (fun _enqueue ->
      let _ = Webapi.Dom.history |> Webapi.Dom.History.go step in
      let () = notifyUrlChange () in
      () )

let back step = go (-step)

let forward step = go step

let navigationProgram ?(ssr = false) locationToMessage stuff =
  let init flag = stuff.init flag in
  let subscriptions model =
    if ssr then stuff.subscriptions model
    else Tea_sub.batch [subscribe locationToMessage; stuff.subscriptions model]
  in
  let open Tea_app in
  program
    { init
    ; update= stuff.update
    ; view= stuff.view
    ; subscriptions
    ; shutdown= stuff.shutdown }
