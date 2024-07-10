module Event = Webapi.Dom.Event
module Node = Webapi.Dom.Node
module Window = Web_window
module Json = Web_json
module XMLHttpRequest = Web_xmlhttprequest
module FormData = Webapi.FormData

let polyfills () =
  let () = Web_node.remove_polyfill () in
  let () = Window.requestAnimationFrame_polyfill () in
  ()
