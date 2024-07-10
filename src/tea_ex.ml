let render_event ?(key = "") msg =
  let open Vdom in
  let enableCall callbacks =
    let () = callbacks.on (AddRenderMsg msg) in
    fun () -> callbacks.on (RemoveRenderMsg msg)
  in
  Tea_sub.registration key enableCall

module LocalStorage = struct
  open Tea_task
  open Tea_result

  open struct
    let local_storage_fn f =
      nativeBinding (fun cb ->
          match f Webapi.Dom.window with
          | None ->
              cb (Error "localStorage is not available")
          | Some value ->
              cb (Ok value) )
  end

  let length = local_storage_fn Web.Window.LocalStorage.length

  let clear = local_storage_fn Web.Window.LocalStorage.clear

  let clearCmd () = Tea_task.attemptOpt (fun _ -> None) clear

  let key idx = local_storage_fn (Web.Window.LocalStorage.key idx)

  let getItem key = local_storage_fn (Web.Window.LocalStorage.getItem key)

  let removeItem key = local_storage_fn (Web.Window.LocalStorage.removeItem key)

  let removeItemCmd key = Tea_task.attemptOpt (fun _ -> None) (removeItem key)

  let setItem key value =
    local_storage_fn (Web.Window.LocalStorage.setItem key value)

  let setItemCmd key value =
    Tea_task.attemptOpt (fun _ -> None) (setItem key value)
end
