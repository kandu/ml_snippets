open Printf

module Dlink = struct
  open Ml_snippets.Dlink

  let dlink0= Elt.init ~f:Fun.id 5
  let elt0= dlink0
    |> Elt.right
    |> Fun.flip Option.bind Elt.right
    |> Option.value ~default:dlink0

  let%expect_test "init & iter"=
    Elt.iter_left ~f:(printf "%d\n") elt0;
    [%expect "
      2
      1
      0"];
    Elt.iter_right ~f:(printf "%d\n") elt0;
    [%expect "
      2
      3
      4"];
    Elt.iter ~f:(printf "%d\n") elt0;
    [%expect "
      2
      1
      0
      3
      4"]

  let%expect_test "map"=
    elt0
      |> Elt.map_left ~f:(( * ) 2)
      |> Elt.iter ~f:(printf "%d\n");
    [%expect "
      4
      2
      0"];
    elt0
      |> Elt.map_right ~f:(( * ) 2)
      |> Elt.iter ~f:(printf "%d\n");
    [%expect "
      4
      6
      8"];
    elt0
      |> Elt.map ~f:(( * ) 2)
      |> Elt.iter ~f:(printf "%d\n");
    [%expect "
      4
      2
      0
      6
      8"];
  ;;

  let%expect_test "fold"=
    elt0
      |> Elt.fold_left ~f:( + ) ~init:10
      |> print_int;
    [%expect "13"];
    elt0
      |> Elt.fold_right ~f:( + ) ~init:10
      |> print_int;
    [%expect "19"];
    elt0
      |> Elt.fold ~f:( + ) ~init:10
      |> print_int;
    [%expect "20"];
  ;;

  let%expect_test "length"=
    elt0 |> Elt.length |> (function
      | Finite l-> print_int l
      | Infinite-> print_string "infinite");
    [%expect "5"];

end

module Dlist = struct
  open Ml_snippets.Dlink

  let dlist0= Dlist.init ~f:Fun.id 5

  let%expect_test "init & iter"=
    Dlist.iter ~f:(printf "%d\n") dlist0;
    [%expect "
      0
      1
      2
      3
      4"]

  let%expect_test "map"=
    let dlist1= Dlist.map ~f:(( * ) 2) dlist0 in
    Dlist.iter ~f:(printf "%d\n") dlist0;
    Dlist.iter ~f:(printf "%d\n") dlist1;
    [%expect "
      0
      1
      2
      3
      4
      0
      2
      4
      6
      8"]

end

module Circle = struct
  open Ml_snippets.Dlink

  let circle0= Circle.init ~f:Fun.id 0
  let circle1= Circle.init ~f:((+)10) 1
  let circle2= Circle.init ~f:Fun.id 5

  let%expect_test "init & iter"=
    circle0 |> Circle.entry |> Option.iter (Circle.iter_right ~f:(printf "%d\n"));
    [%expect ""];
    circle1 |> Circle.entry |> Option.iter (Circle.iter_right ~f:(printf "%d\n"));
    [%expect "10"];
    circle2 |> Circle.entry |> Option.iter (Circle.iter_left ~f:(printf "%d\n"));
    [%expect "
      0
      4
      3
      2
      1"];
  ;;

  let%expect_test "size"=
    circle0 |> Circle.size |> printf "%d\n";
    circle1 |> Circle.size |> printf "%d\n";
    circle2 |> Circle.size |> printf "%d\n";
    [%expect "
      0
      1
      5"];
  ;;

  let circle3= circle2 |> Circle.map ~f:(( * ) 2)

  let%expect_test "map"=
    circle3 |> Circle.entry |> Option.iter (Circle.iter_right ~f:(printf "%d\n"));
    [%expect "
      0
      2
      4
      6
      8"];
  ;;

  let%expect_test "remove"=
    circle2
      |> Circle.entry
      |> Fun.flip Option.bind Circle.right
      |> Fun.flip Option.bind Circle.right
      |> Option.iter Circle.remove;
    circle2 |> Circle.entry |> Option.iter (Circle.iter_right ~f:(printf "%d\n"));
    [%expect "
      0
      1
      3
      4"];
  ;;

  let%expect_test "remove_entry"=
    circle3
      |> Circle.entry
      |> Option.iter Circle.remove;
    circle3 |> Circle.entry |> Option.iter (Circle.iter_right ~f:(printf "%d\n"));
    [%expect "
      2
      4
      6
      8"];
  ;;

  let%expect_test "remove_all"=
    circle3
      |> Circle.entry
      |> Option.iter Circle.remove;
    circle3
      |> Circle.entry
      |> Option.iter Circle.remove;
    circle3
      |> Circle.entry
      |> Option.iter Circle.remove;
    circle3 |> Circle.entry |> Option.iter (Circle.iter_right ~f:(printf "%d\n"));
    [%expect "
      8"];
    circle3
      |> Circle.entry
      |> Option.iter (fun entry-> entry.Circle.right |> Option.iter (fun right-> printf "%b\n" (right == entry)));
    [%expect "
      true"];
    circle3 |> Circle.size |> printf "%d\n";
    [%expect "1"];
    circle3
      |> Circle.entry
      |> Option.iter Circle.remove;
    printf "%b\n" (circle3 |> Circle.entry |> Option.is_none);
    [%expect "true"];
  ;;

  let%expect_test "fold_left"=
    circle2
      |> Circle.fold_left
        ~init:0
        ~f:(fun acc value-> printf "%d\n" value; acc + value)
      |> printf "%d\n";
    [%expect "
      0
      4
      3
      1
      8"];
  ;;

  let%expect_test "fold_right"=
    circle2
      |> Circle.fold_right
        ~init:0
        ~f:(fun acc value-> printf "%d\n" value; acc + value)
      |> printf "%d\n";
    [%expect "
      0
      1
      3
      4
      8"];
  ;;

end

