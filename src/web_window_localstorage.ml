open struct
  external local_storage : Webapi.Dom.Window.t -> Dom.Storage.t option
    = "localStorage"

  let local_storage_fn f window =
    match local_storage window with
    | Some localStorage ->
        Some (f localStorage)
    | None ->
        None
end

let length window = local_storage_fn Dom.Storage.length window

let clear window = local_storage_fn Dom.Storage.clear window

let key idx window = local_storage_fn (Dom.Storage.key idx) window

let getItem key window = local_storage_fn (Dom.Storage.getItem key) window

let removeItem key window = local_storage_fn (Dom.Storage.removeItem key) window

let setItem key value window =
  local_storage_fn (Dom.Storage.setItem key value) window
