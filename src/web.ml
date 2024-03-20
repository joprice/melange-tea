module Event = Webapi.Dom.Event
module Node = Webapi.Dom.Node
module Document = Web_document
module Date = Web_date
module Window = Web_window
module Location = Web_location
module Json = Web_json
module XMLHttpRequest = Web_xmlhttprequest
module FormData = Webapi.FormData

let polyfills () =
  let () = Web_node.remove_polyfill () in
  let () = Window.requestAnimationFrame_polyfill () in
  ()
