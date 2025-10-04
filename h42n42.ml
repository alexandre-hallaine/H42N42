open Js_of_ocaml

module Html = Dom_html

let width = 800.
let height = 600.
let river_height = 50.
let hospital_height = 50.
let creet_size = 10.
let num_creets = 20

type creature_state = Healthy | Sick | Berserk | Mean
type position = { x : float; y : float }
type creature = {
  id : int;
  pos : position;
  vel : position;
  state : creature_state;
  size : float;
  invulnerable : bool;
}

type game_state = {
  creets : creature list;
  mouse_down : bool;
  dragged_creet : int option;
  game_over : bool;
}

let get_random_pos () =
  {
    x = Random.float (width -. creet_size) +. creet_size;
    y = Random.float (height -. river_height -. hospital_height -. creet_size) +. river_height +. creet_size;
  }

let get_random_vel () =
  let angle = Random.float (2. *. Float.pi) in
  { x = cos angle; y = sin angle }

let create_creet id =
  {
    id;
    pos = get_random_pos ();
    vel = get_random_vel ();
    state = Healthy;
    size = creet_size;
    invulnerable = false;
  }

let init_game_state () =
  let creets = List.init num_creets create_creet in
  { creets; mouse_down = false; dragged_creet = None; game_over = false }

let draw_rect (ctx : Html.canvasRenderingContext2D Js.t) x y w h color =
  ctx##.fillStyle := Js.string color;
  ctx##fillRect (Js.float x) (Js.float y) (Js.float w) (Js.float h)

let draw_river (ctx : Html.canvasRenderingContext2D Js.t) =
  draw_rect ctx 0. 0. width river_height "#0000FF"

let draw_hospital (ctx : Html.canvasRenderingContext2D Js.t) =
  draw_rect ctx 0. (height -. hospital_height) width hospital_height "#00FF00"

let get_creet_color = function
  | Healthy -> "#000000"
  | Sick -> "#FF0000"
  | Berserk -> "#FFA500"
  | Mean -> "#800080"

let draw_creet (ctx : Html.canvasRenderingContext2D Js.t) creet =
  ctx##beginPath;
  ctx##arc (Js.float creet.pos.x) (Js.float creet.pos.y) (Js.float creet.size) (Js.float 0.) (Js.float (2. *. Float.pi)) Js._true;
  ctx##.fillStyle := Js.string (get_creet_color creet.state);
  ctx##fill

let draw (ctx : Html.canvasRenderingContext2D Js.t) state =
  ctx##clearRect (Js.float 0.) (Js.float 0.) (Js.float width) (Js.float height);
  draw_rect ctx 0. 0. width height "#DDDDDD";
  draw_river ctx;
  draw_hospital ctx;
  List.iter (draw_creet ctx) state.creets;
  if state.game_over then (
    ctx##.font := Js.string "50px Arial";
    ctx##.fillStyle := Js.string "#000000";
    ctx##fillText (Js.string "GAME OVER") (Js.float (width /. 2. -. 150.)) (Js.float (height /. 2.))
  )

let update_pos creet =
  let new_pos = { x = creet.pos.x +. creet.vel.x; y = creet.pos.y +. creet.vel.y } in
  let new_vel =
    if new_pos.x < creet.size || new_pos.x > width -. creet.size then
      { creet.vel with x = -.creet.vel.x }
    else if new_pos.y < creet.size || new_pos.y > height -. creet.size then
      { creet.vel with y = -.creet.vel.y }
    else
      creet.vel
  in
  { creet with pos = new_pos; vel = new_vel }

let handle_river creet =
  if creet.pos.y < river_height then
    { creet with state = Sick }
  else
    creet

let distance p1 p2 =
  sqrt ((p1.x -. p2.x) ** 2. +. (p1.y -. p2.y) ** 2.)

let handle_contamination creets =
  let sick_creets = List.filter (fun c -> c.state = Sick || c.state = Berserk || c.state = Mean) creets in
  List.map (fun creet ->
    if creet.state = Healthy && not creet.invulnerable then
      let is_contaminated = List.exists (fun sick_creet ->
        distance creet.pos sick_creet.pos < creet.size +. sick_creet.size && Random.float 1. < 0.02
      ) sick_creets in
      if is_contaminated then { creet with state = Sick } else creet
    else creet
  ) creets

let handle_sickness creet =
  if creet.state = Sick then
    let r = Random.float 1. in
    if r < 0.1 then { creet with state = Berserk }
    else if r < 0.2 then { creet with state = Mean }
    else creet
  else creet

let update_creets creets =
  let updated_creets = List.map (fun creet ->
    let creet = update_pos creet in
    let creet = handle_river creet in
    handle_sickness creet
  ) creets in
  handle_contamination updated_creets

let update state =
  if state.game_over then state
  else
    let creets = match state.dragged_creet with
      | Some id ->
          let dragged, others = List.partition (fun c -> c.id = id) state.creets in
          dragged @ (update_creets others)
      | None -> update_creets state.creets
    in
    let healthy_creets = List.filter (fun c -> c.state = Healthy) creets in
    let game_over = List.length healthy_creets = 0 in
    { state with creets; game_over }

let get_mouse_pos canvas event =
  let rect = canvas##getBoundingClientRect in
  let x = event##.clientX - int_of_float rect##.left in
  let y = event##.clientY - int_of_float rect##.top in
  { x = float x; y = float y }

let handle_mouse_down state pos =
  let creet_to_drag = List.find_opt (fun creet ->
    distance creet.pos pos < creet.size
  ) state.creets in
  match creet_to_drag with
  | Some creet ->
      let creets = List.map (fun c -> if c.id = creet.id then { c with invulnerable = true } else c) state.creets in
      { state with mouse_down = true; dragged_creet = Some creet.id; creets }
  | None -> state

let handle_mouse_up state =
  let creets = List.map (fun c -> { c with invulnerable = false }) state.creets in
  let creets = List.map (fun creet ->
    if creet.state = Sick && creet.pos.y > height -. hospital_height then
      { creet with state = Healthy }
    else
      creet
  ) creets in
  { state with mouse_down = false; dragged_creet = None; creets }

let handle_mouse_move state pos =
  if state.mouse_down then
    match state.dragged_creet with
    | Some id ->
        let creets = List.map (fun creet ->
          if creet.id = id then { creet with pos } else creet
        ) state.creets in
        { state with creets }
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
    let pos = get_mouse_pos canvas event in
    state := handle_mouse_down !state pos;
    Js._true
  );
  canvas##.onmouseup := Html.handler (fun _ ->
    state := handle_mouse_up !state;
    Js._true
  );
  canvas##.onmousemove := Html.handler (fun event ->
    let pos = get_mouse_pos canvas event in
    state := handle_mouse_move !state pos;
    Js._true
  );
  loop 0.0
