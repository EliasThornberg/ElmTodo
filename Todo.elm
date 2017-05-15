import Html exposing (Html, button, div, text, ul, li, input, span)
import Html.Attributes exposing (type_, style, checked, id)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed

main =
  Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL
type alias Todo = { text: String, completed: Bool, id: Int }
type alias Model = { items: List Todo, text: String, currentId: Int}

model : Model
model = { items= [], text = "", currentId = 0}


-- UPDATE

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
      {model | items = List.map (setCompleted id) model.items }
    
    AddText s ->
      {model | text = s}

setCompleted: Int -> Todo -> Todo
setCompleted id todo =
  if todo.id == id then {todo | completed = True} else todo

view : Model -> Html Msg
view model =
  div [style [("display", "flex"), ("align-items", "left"), ("justify-content", "left")]][
    input [ onInput AddText][]
    ,button [ onClick AddTodo ][text "Lägg till"]
    , buildTodoList (List.filter(\m -> m.completed == False) model.items)]


buildTodoList: List Todo -> Html Msg
buildTodoList list =
    Keyed.ul [style [("list-style-type", "none"), ("align", "left")]] (List.map buildLi list)

buildLi: Todo -> ( String, Html Msg )
buildLi t =
  (toString t.id , li [][input [type_ "Checkbox", onClick (CompleteTodo t.id), checked t.completed][], text t.text])


