open Tea
open Html2
open Events

type message = Route of Webapi.Dom.Location.t | Goto of string
[@@deriving accessors]

let init () location = (location, Tea.Cmd.none)

let update model (message : message) =
  match message with
  | Route model ->
      (model, Cmd.none)
  | Goto route ->
      (model, Tea.Navigation.modifyUrl route)

let view model =
  let loc = model |> Webapi.Dom.Location.pathname in
  let link url = div [] [a [onClick (goto url)] [text url]] in
  div []
    [ div [] [text "Current route: "; text loc]
    ; div [] [link "/test-1"; link "/test-2"; link "/test-3"] ]

let subscriptions _model = Sub.none

let shutdown _model = Cmd.none

let main =
  Tea.Debug.navigationProgram
    (fun location -> Route location)
    {init; update; view; subscriptions; shutdown}
    (function Route _ -> "route" | Goto path -> "Goto " ^ path)
