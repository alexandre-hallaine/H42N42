open Js_of_ocaml
module Html = Dom_html

let width = float_of_int Html.window##.innerWidth
let height = float_of_int Html.window##.innerHeight
let river_height = 50.
let hospital_height = 50.
let amount_of_creatures = 20

type creature_state = Healthy | Sick | Berserk | Mean
type position = { x : float; y : float }
type creature = {
  position : position;
  velocity : position;
  state : creature_state;
  size : float;
}
type state = {
  creatures : creature list;
  dragged_creature : int option;
  game_over : bool;
}

let get_random_position minimum_size =
  let x = Random.float (width -. minimum_size) +. minimum_size in
  let y = Random.float (height -. river_height -. hospital_height -. minimum_size) +. river_height +. minimum_size in
  { x; y }

let get_random_velocity () =
  let angle = Random.float (2. *. Float.pi) in
  { x = cos angle; y = sin angle }

let create_creature _ =
  let size = 20. in
  {
    position = get_random_position size;
    velocity = get_random_velocity ();
    state = Healthy;
    size;
  }

let init_game_state () =
  {
    creatures = List.init amount_of_creatures create_creature;
    dragged_creature = None;
    game_over = false;
  }

let get_creature_color = function
  | Healthy -> "#000000"
  | Sick -> "#FF0000"
  | Berserk -> "#FFA500"
  | Mean -> "#800080"

let draw_rect (ctx : Html.canvasRenderingContext2D Js.t) x y w h color =
  ctx##.fillStyle := Js.string color;
  ctx##fillRect (Js.float x) (Js.float y) (Js.float w) (Js.float h)

let draw_creature (ctx : Html.canvasRenderingContext2D Js.t) creature =
  ctx##beginPath;
  ctx##arc (Js.float creature.position.x) (Js.float creature.position.y) (Js.float creature.size) (Js.float 0.) (Js.float (2. *. Float.pi)) Js._true;
  ctx##.fillStyle := Js.string (get_creature_color creature.state);
  ctx##fill

let draw (ctx : Html.canvasRenderingContext2D Js.t) state =
  ctx##clearRect (Js.float 0.) (Js.float 0.) (Js.float width) (Js.float height);
  draw_rect ctx 0. 0. width river_height "#0000FF";
  draw_rect ctx 0. (height -. hospital_height) width hospital_height "#FF0000";
  List.iter (draw_creature ctx) state.creatures

let update_position creature =
  let new_position = { x = creature.position.x +. creature.velocity.x; y = creature.position.y +. creature.velocity.y } in
  let new_velocity =
    if new_position.x -. creature.size < 0. || new_position.x +. creature.size > width then
      { creature.velocity with x = -.creature.velocity.x }
    else if new_position.y -. creature.size < 0. || new_position.y +. creature.size > height then
      { creature.velocity with y = -.creature.velocity.y }
    else
      creature.velocity
  in
    { creature with position = new_position; velocity = new_velocity }

let distance p1 p2 =
  sqrt ((p1.x -. p2.x) ** 2. +. (p1.y -. p2.y) ** 2.)

let handle_river creature =
  if creature.position.y < river_height && creature.state = Healthy then
    { creature with state = Sick }
  else
    creature

let handle_contamination creatures =
  let sick_creatures = List.filter (fun c -> c.state != Healthy) creatures in
  List.map (fun creature ->
    if creature.state = Healthy then
      let is_contaminated = List.exists (fun sick_creature ->
        distance creature.position sick_creature.position < creature.size +. sick_creature.size && Random.float 1. < 0.02
      ) sick_creatures in
      if is_contaminated then { creature with state = Sick } else creature
    else creature
  ) creatures

let handle_sickness creature =
  if creature.state = Sick then
    let r = Random.float 1. in
    if r < 0.1 then { creature with state = Berserk }
    else if r < 0.2 then { creature with state = Mean }
    else creature
  else creature

let update_creatures creatures =
  let updated_creatures = List.map (fun creature ->
    let creature = update_position creature in
    let creature = handle_river creature in
    handle_sickness creature
  ) creatures in
    handle_contamination updated_creatures

let update state =
  if state.game_over then state
  else
    let creatures = match state.dragged_creature with
      | Some idx ->
          let dragged, others = List.partition (fun c -> c.id = id) state.creatures in
          dragged @ (update_creatures others)
      | None -> update_creatures state.creatures
    in
      let healthy_creatures = List.filter (fun c -> c.state = Healthy) creatures in
      let game_over = List.length healthy_creatures = 0 in
      { state with creatures; game_over }

let get_mouse_position canvas event =
  let rect = canvas##getBoundingClientRect in
  let x = event##.clientX - int_of_float rect##.left in
  let y = event##.clientY - int_of_float rect##.top in
  { x = float x; y = float y }

let handle_mouse_down state position =
  let creature_to_drag = List.find_opt (fun creature ->
    distance creature.position position < creature.size
  ) state.creatures in
  match creature_to_drag with
  | Some creature -> { state with dragged_creature = Some creature.id }
  | None -> state

let handle_mouse_down state position =
  let dragged_creature = List.find_mapi
    (fun i creature -> if distance creature.position position < creature.size then Some i else None)
    state.creatures
  in
  { state with dragged_creature }

let handle_mouse_up state =
  let creatures = List.map (fun creature ->
    if creature.state = Sick && creature.position.y > height -. hospital_height then
      { creature with state = Healthy }
    else
      creature
  ) state.creatures in
    { state with mouse_down = false; dragged_creature = None; creatures }

let handle_mouse_move state position =
  if state.mouse_down then
    match state.dragged_creature with
    | Some id ->
        let creatures = List.map (fun creature ->
          if creature.id = id then { creature with position } else creature
        ) state.creatures in
        { state with creatures }
    | None -> state
  else
    state

let () =
  let canvas = Html.createCanvas Html.window##.document in
  canvas##.width := int_of_float width;
  canvas##.height := int_of_float height;
  Dom.appendChild Html.window##.document##.body canvas;
  let ctx = canvas##getContext Html._2d_ in
  let state = ref (init_game_state ()) in
  let rec loop _ =
    state := update !state;
    draw ctx !state;
    Html.window##requestAnimationFrame (Js.wrap_callback loop) |> ignore
  in
  canvas##.onmousedown := Html.handler (fun event ->
    let position = get_mouse_position canvas event in
    state := handle_mouse_down !state position;
    Js._true
  );
  canvas##.onmouseup := Html.handler (fun _ ->
    state := handle_mouse_up !state;
    Js._true
  );
  canvas##.onmousemove := Html.handler (fun event ->
    let position = get_mouse_position canvas event in
    state := handle_mouse_move !state position;
    Js._true
  );
  loop 0.0
