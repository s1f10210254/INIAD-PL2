type location =
LatLng of { la : float; lng : float;}
  | Place of { nam : string };

type way = Walk | Fly | Car | Train

let deg2red deg =
  let pi = 3.141592 in deg *. pi /. 180.0

  
let distance p1 p2 =
  match p1 p2 with
    LatLng(s), LatLng(e) ->
      let a1 = deg2red s.lat
      and r1 = deg2red s.lng
      and a2 = deg2red e.lat
      and r2 = deg2red e.lng in
      6370.0 *. acos(sin a1 *. sin a2 +. cos a1 *. a2 *. cos(r1 - r2))
  | _ -> failwith "error"


let velocity way =
  match way with
    Walk -> 4.0
  | Fly -> 800.0
  | Car -> 50.0
  | Train -> 90.0

let diraction way p1 p2 = 
  distance p1 p2 /. velocity way

let akabane = LatLng{lat = 35.777615; lng = 139.7209868};;
let tokyo = LatLng{lat = 35.681236; lng = 139.767125};;
duration Train akabane tokyo;;