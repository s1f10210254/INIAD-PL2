(*floatの値2つを保存できる独自の型lat_lngを定義せよ。(緯度経度を入れるつもり)*)

type lat_lng = {lat: float; lng:float};;

(*lat_lngと、値が存在しないことの両方を表現できる表現できる
   ヴァリアント型lat_lng_opを定義せよ。
   ただし、lat_lngのラベルはValues、値がないことのラベルはNullとせよ。*)

type lat_lng_op = 
  Values of lat_lng
  | Null;;

(*lat_lng_op型の値1つを引数にとり、INIADより北のときbool option型のtrueを
それ以外の時bool option型のfalseを返す関数north_of_iniadを定義せよ。
ただし、引数がNullのときはbool option型のNoneを返せ。
INIADの緯度、経度は35.780246, 139.7156とする。*)

let north_of_iniad op =
  match op with
    Values(p) -> if p.lat > 35.780246 then Some(true) else Some(false)
    | Null -> None;;

(*north_of_iniadを使って赤羽駅(35.777615, 139.7209868)とINIADを比較せよ。*)
let p = {lat = 35.7777615; lng = 139.7209868; };;
let akabane_sta = Values(p);;
north_of_iniad akabane_sta;;


