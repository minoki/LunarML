infix 1 |>

fun x |> f = f x

structure ExampleModel =
struct
  type model = { name: string, x: real }

  datatype msg = Move of real | SomeOtherEvent 

  fun init () =
    ({ name = "Carl", x = 0.0 }, Cmd.none)

  fun update msg model =
    case msg of
         Move amount =>
         let
           val {name, x} = model
         in
           ({ name, x = x + amount }, Cmd.none)
         end
       | _ => (model, Cmd.none)

  fun view ({x, ...}: model) =
    (Love.Graphics.clear();
     Love.Graphics.rectangle(Love.Graphics.Fill, x, 50.0, 20.0, 20.0))
end

structure App = MakeRuntime(ExampleModel)

fun loveUpdate dt =
  App.dispatch (ExampleModel.Move (50.0 * dt))

val export =
  Love.Config.empty
  |> Love.Config.withLoad (fn () => App.init ())
  |> Love.Config.withUpdate loveUpdate
  |> Love.Config.withDraw App.runView
  |> Love.Config.withKey (fn key =>
       Love.Graphics.print ("key: " ^ key, 10.0, 30.0))
  |> Love.Config.apply
