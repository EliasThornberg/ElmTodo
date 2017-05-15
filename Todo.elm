import Html exposing (Html, button, div, text, ul, li, input, span)
import Html.Attributes exposing (type_, style, checked, id)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed

main =
  Html.beginnerProgram { model = model, view = view, update = update }

type alias Todo = { text: String, completed: Bool, id: Int }
type alias Model = { items: List Todo, text: String, currentId: Int}

model : Model
model = { items= [], text = "", currentId = 0}

type Msg 
  = AddTodo
  | DeleteTodo Int
  | CompleteTodo Int
  | AddText String

update : Msg -> Model -> Model
update msg model =
  case msg of
    AddTodo ->
      {model | items = model.items ++ [{text = model.text, id = model.currentId + 1, completed=False}], currentId = model.currentId + 1}

    DeleteTodo id ->
      {model | items = List.filter (\m -> m.id /= id) model.items}

    CompleteTodo id ->
      {model | items = List.map (setTodoCompleted id) model.items }
    
    AddText s ->
      {model | text = s}

setTodoCompleted: Int -> Todo -> Todo
setTodoCompleted id todo =
  if todo.id == id then {todo | completed = True} else todo


view : Model -> Html Msg
view model =
  div [style divStyle][
    input [ onInput AddText][]
    ,button [ onClick AddTodo ][text "Add todo"]
    ,buildTodoList (List.filter showActiveTodo model.items)]

showActiveTodo: Todo -> Bool
showActiveTodo t =
  t.completed == False

buildTodoList: List Todo -> Html Msg
buildTodoList list =
    Keyed.ul [style ulStyle] (List.map buildTodoListItem list)

buildTodoListItem: Todo -> ( String, Html Msg )
buildTodoListItem t =
  (toString t.id , li []
    [
      input [type_ "Checkbox", onClick (CompleteTodo t.id), checked t.completed][]
      ,text t.text
      ,button [onClick (DeleteTodo t.id), style removeButtonStyle][text "X"]
    ])


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
