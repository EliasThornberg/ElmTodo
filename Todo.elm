import Html exposing (Html, button, div, text, ul, li, input, span, label, fieldset)
import Html.Attributes exposing (type_, style, checked, id, name, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed

main =
  Html.beginnerProgram { model = model, view = view, update = update }

type alias Todo = { text: String, status: Status, id: Int }
type alias Model = { items: List Todo, text: String, currentId: Int, filterStatus: Status}

type Status
  = Completed
  | Active
  | None

type Msg 
  = AddTodo
  | DeleteTodo Int
  | CompleteTodo Int
  | AddText String
  | AddFilter Status

model : Model
model = { items= [], text = "", currentId = 0, filterStatus = Active}

update : Msg -> Model -> Model
update msg model =
  case msg of
    AddTodo ->
      {model | items = model.items ++ [{text = model.text, id = model.currentId + 1, status= Active}], currentId = model.currentId + 1, text = ""}

    DeleteTodo id ->
      {model | items = List.filter (\m -> m.id /= id) model.items}

    CompleteTodo id ->
      {model | items = List.map (setTodoCompleted id) model.items }
    
    AddText s ->
      {model | text = s}
    
    AddFilter f ->
      {model | filterStatus = f}

-- View
view : Model -> Html Msg
view model =
  div [style divStyle][
    input [ onInput AddText, value model.text][]
    ,button [ onClick AddTodo ][text "Add todo"]
    ,filterContainer
    ,todoListContainer model.items model.filterStatus]

--- TodoList
todoListContainer: List Todo -> Status -> Html Msg
todoListContainer xs f =
  div [][ Keyed.ul [style ulStyle] (
      xs 
      |> List.filter (\m -> m.status == f || f == None)
      |> List.map todoListItem
    )]

todoListItem: Todo -> ( String, Html Msg )
todoListItem t =
  (toString t.id , li []
    [
      input [type_ "Checkbox", onClick (CompleteTodo t.id), checked (t.status == Completed)][]
      ,text t.text
      ,button [onClick (DeleteTodo t.id), style removeButtonStyle][text "X"]
    ])

setTodoCompleted: Int -> Todo -> Todo
setTodoCompleted id todo =
  if todo.id == id then {todo | status = Completed} else todo

-- Filter
filterContainer: Html Msg
filterContainer =
  fieldset []
  [
    label []
      [
        input [type_ "radio", name "filter", onClick (AddFilter Completed)][],
        text "Completed"
      ]
    ,label[]
      [
        input [type_ "radio", name "filter", onClick (AddFilter Active)][],
        text "Active"
      ]
    ,label[]
      [
        input [type_ "radio", name "filter", onClick (AddFilter None)][],
        text "None"
      ]
  ]

-- Style
ulStyle: List(String, String)
ulStyle =
  [
    ("list-style-type", "none")
    ,("align", "left")
  ]

divStyle: List(String, String)
divStyle =
  [
    ("display", "flex")
    ,("align-items", "left")
    ,("justify-content", "left")
  ]

removeButtonStyle: List(String, String)
removeButtonStyle = 
  [
    ("width", "15px")
    ,("text-align", "center")
    ,("padding", "0px")
    ,("margin-left", "15px")
  ]
