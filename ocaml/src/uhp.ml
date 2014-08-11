open Core

type pos = float Point.t

type state = { pos : pos ; dir : Angle.t }
(* Forward/Backward corresponds to increasing/decreasing angle for
 * Semi_circle and increasing/decreasing y-coordinate for Vertical *)
type orientation = Forward | Backward
type geodesic =
  | Semi_circle of float * float
  | Vertical    of float

let epsilon         = 1e-11
let scale_factor    = 0.05
let angle_scale     = 5.
let width, height   = 1200, 600
let width', height' = float_of_int width, float_of_int height

let arrows = Frp.project_with Jq.arrows (Frp.Stream.ticks 30.) ~f:(fun (ax, ay) _ ->
  Angle.of_degrees (float_of_int ax), float_of_int ay)

let movement_vect {pos = (_, y); dir} =
  Vector.scale (scale_factor *. y) (Angle.cos dir, Angle.sin dir)

let state =
  Frp.scan arrows ~init:{pos = (100., 100.); dir = Angle.of_degrees 0.}
    ~f:(fun ({pos; dir} as s) (ax, ay) -> {
        dir = Angle.(dir + angle_scale * ax);
        pos = Vector.add pos (Vector.scale ay (movement_vect s))
    })

(*
let geodesic_through =
  let on_vertical x = cos x = 0. in
  fun {pos; dir} ->
    let dir_rad = Angle.to_radians dir in
    if on_vertical dir_rad
    then (Vertical (snd pos), if sin dir_rad > 0. then Forward else Backward)
    else Semi_circle

*)
let player = let open Draw in let open Frp.Behavior in
  Draw.circle ~fill:(return Color.black)
    (map state ~f:(fun {pos=(_,y)} -> 0.15 *. y))
    (map state ~f:(fun {pos} -> pos))

let standard_arrow = let open Draw in let open Frp.Behavior in
  let x_offset = 2. /. 3. in
  let h = 1. /. 2. in
  let tri = polygon (return [|(x_offset, -.h/.2.); (x_offset +. 1./.3., 0.) ;(x_offset, h/.2.)|])
    ~fill:(return Color.red)
  in
  pictures [|
    path ~anchor:(return (0., 0.)) ~stroke:(return (Stroke.create Color.red 0.1))
      (return [|Segment.line_to (x_offset, 0.)|]);
    tri
  |]

let draw_arrow src_dst =
  Draw.transform standard_arrow
    (Frp.Behavior.map src_dst ~f:(fun (src, dst) ->
      let ((x, y) as diff) = Vector.(dst - src) in
      let r = Vector.norm diff in
      let t = Angle.atan2 y x in
      Draw.Transform.([|translate src; rotate t; scale r r|])))

let dir_arrow : Draw.t = let open Draw in let open Frp.Behavior in
  let d = 1. in
  transform standard_arrow
    Transform.(map state ~f:(fun {pos; dir} -> let y = snd pos in
      let sine = Angle.sin dir in
      let sf =
        if abs_float sine < epsilon then d *. y
        else 
          let csc = 1. /. sine in
          (exp ((d +. csc *. log y) *. sine) -. y ) *. csc
      in
      [|translate pos; rotate dir; scale sf sf|]))

let flip_button = Option.value_exn (Jq.jq "<button>Flip</button>")

let flipped = Frp.scan (Jq.clicks flip_button) ~init:true ~f:(fun b _ -> not b)

let check_box = Jq.create "input type='checkbox'"
let checked = 
  let check_box_node = Option.value_exn (Jq.to_dom_node check_box) in
  Frp.latest ~init:false (Frp.Stream.map (Jq.clicks check_box) ~f:(fun _ ->
    Js.to_bool (Js.Unsafe.get check_box_node (Js.string "checked"))))

let drawing = let open Draw in
  let flip = [|Transform.rotate (Angle.of_degrees 180.) ~about:(width' /. 2., height' /. 2.)|] in
  let d =
    transform (pictures [|dir_arrow; player|])
      (Frp.Behavior.map flipped ~f:(fun is_flipped ->
        if is_flipped
        then flip
        else [||]))
  in
  dynamic (Frp.Behavior.map checked ~f:(fun is_checked ->
    if is_checked then pictures [|d; transform d (Frp.Behavior.return flip)|]
    else d))

let () =
  let main_container = Option.value_exn (Jq.jq "#container") in
  let (svg, sub) = Draw.render_svg_node ~width ~height drawing in
  Jq.Dom.append (Option.value_exn (Jq.to_dom_node main_container)) svg;
  Jq.insert_after main_container flip_button;
  Jq.insert_after flip_button check_box

