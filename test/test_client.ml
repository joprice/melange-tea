let counter =
  let open Test_client_counter in
  main

let counter_debug_beginner = Test_client_counter_debug_beginner.main

let counter_debug_standard =
  let open Test_client_counter_debug_standard in
  main

let counter_debug_program =
  let open Test_client_counter_debug_program in
  main

let btn_update_span =
  let open Test_client_btn_update_span in
  main

let attribute_removal =
  let open Test_client_attribute_removal in
  main

let drag =
  let open Test_client_drag in
  main

let on_with_options =
  let open Test_client_on_with_options in
  main

let http_task =
  let open Test_client_http_task in
  main

let navigation = Test_navigation.main
