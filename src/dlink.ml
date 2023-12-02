module Elt = struct
  type length=
    | Finite of int
    | Infinite

  type 'a t= {
    value: 'a;
    mutable left: 'a t option;
    mutable right: 'a t option;
  }

  let left elt=
    elt.left

  let right elt=
    elt.right

  let init ~f n=
    if n < 1 then invalid_arg "length should be at least 1" else
    let init= { value= f 0; left= None; right= None } in
    let prev= ref init in
    for i= 1 to n-1 do
      let next= { value= f i; left= Some !prev; right= None } in
      !prev.right <- Some next;
      prev:= next;
    done;
    init

  let length_left elt=
    let anchor= elt in
    let rec length acc elt=
      match elt.left with
      | Some elt->
        if elt == anchor then
          Infinite
        else
          length (acc+1) elt
      | None-> Finite acc
    in
    length 1 elt

  let length_right elt=
    let anchor= elt in
    let rec length acc elt=
      match elt.right with
      | Some elt->
        if elt == anchor then
          Infinite
        else
          length (acc+1) elt
      | None-> Finite acc
    in
    length 1 elt

  let length elt=
    match (length_left elt, length_right elt) with
    | (Finite left, Finite right)-> Finite (left + right - 1)
    | _-> Infinite

  let insert_left elt value=
    let left= left elt in
    let new_elt= {
      value;
      left= left;
      right= Some elt;
    }
    in
    (match left with
    | Some left-> left.right <- Some new_elt
    | None-> ());
    elt.left <- Some new_elt

  let insert_right elt value=
    let right= right elt in
    let new_elt= {
      value;
      left= Some elt;
      right= right;
    }
    in
    elt.right <- Some new_elt;
    match right with
    | Some right-> right.left <- Some new_elt
    | None-> ()

  let remove elt=
    let left= elt.left
    and right= elt.right in
    (match left with
    | Some left-> left.right <- right
    | None-> ());
    (match right with
    | Some right-> right.left <- left
    | None-> ())

  let rec iter_left ~f elt=
    f elt.value;
    Option.iter (iter_left ~f) elt.left

  let rec iter_right ~f elt=
    f elt.value;
    Option.iter (iter_right ~f) elt.right

  let iter ~f elt=
    iter_left ~f elt;
    Option.iter (iter_right ~f) elt.right

  let map_left ~f elt=
    let rec map_left ~f elt_right elt_from=
      let elt_new= {
        value= f elt_from.value;
        left= None;
        right= Some elt_right;
      }
      in
      elt_right.left <- Some elt_new;
      Option.iter (map_left ~f elt_new) elt_from.left
    in
    let init= {
      value= f elt.value;
      left= None;
      right= None;
    } in
    Option.iter (map_left ~f init) elt.left;
    init

  let map_right ~f elt=
    let rec map_right ~f elt_left elt_from=
      let elt_new= {
        value= f elt_from.value;
        left= Some elt_left;
        right= None;
      }
      in
      elt_left.right <- Some elt_new;
      Option.iter (map_right ~f elt_new) elt_from.right
    in
    let init= {
      value= f elt.value;
      left= None;
      right= None;
    } in
    Option.iter (map_right ~f init) elt.right;
    init

  let map ~f elt=
    let elt_left= map_left ~f elt in
    let elt_right= Option.map (map_right ~f) elt.right in
    elt_left.right <- elt_right;
    Option.iter (fun elt_right-> elt_right.left <- Some elt_left) elt_right;
    elt_left

  let rec fold_left ~f ~init elt=
    let init= f init elt.value in
    match elt.left with
    | Some elt-> fold_left ~f ~init elt
    | None-> init

  let rec fold_right ~f ~init elt=
    let init= f init elt.value in
    match elt.right with
    | Some elt-> fold_right ~f ~init elt
    | None-> init

  let fold ~f ~init elt=
    let acc= fold_left ~f ~init elt in
    elt.right
      |> Option.map (fold_right ~f ~init:acc)
      |> Option.value ~default:acc
end

module Dlist = struct
  type 'a t= {
    mutable head: 'a Elt.t option;
    mutable tail: 'a Elt.t option;
    mutable length: int;
  }

  let head t= t.head
  let tail t= t.tail

  let init ~f n=
    let open Elt in
    if n < 0 then
      invalid_arg "length should be at least 0"
    else if n = 0 then
      {
        head= None;
        tail= None;
        length= n;
      }
    else
      let init= { value= f 0; left= None; right= None } in
      let prev= ref init in
      for i= 1 to n-1 do
        let next= { value= f i; left= Some !prev; right= None } in
        !prev.right <- Some next;
        prev:= next;
      done;
      {
        head= Some init;
        tail= Some !prev;
        length= n;
      }

  let length t= t.length

  let insert_elt_left t elt value=
    Elt.insert_left elt value;
    Option.iter
      (fun head-> if head == elt then t.head <- head.left)
      t.head

  let insert_elt_right t elt value=
    Elt.insert_right elt value;
    Option.iter
      (fun tail-> if tail == elt then t.tail <- tail.right)
      t.head

  let remove t elt=
    if t.length > 0 then begin
      Option.iter
        (fun head->
          if head == elt then
            t.head <- head.Elt.right)
        t.head;
      Option.iter
        (fun tail->
          if tail == elt then
            t.tail <- tail.Elt.left)
        t.tail;
      Elt.remove elt;
    end
    
  let iter ~f t=
    Option.iter (Elt.iter_right ~f) t.head

  let iter_rev ~f t=
    Option.iter (Elt.iter_left ~f) t.tail

  let map ~f t=
    let rec map_right ~f elt_left elt_from=
      let elt_new= Elt.{
        value= f elt_from.value;
        left= Some elt_left;
        right= None;
      }
      in
      elt_left.right <- Some elt_new;
      match elt_from.right with
      | Some right-> (map_right ~f elt_new right)
      | None-> elt_new
    in
    match t.head with
    | None->
      {
        length=0;
        head= None;
        tail= None;
      }
    | Some elt->
      let init= Elt.{
        value= f elt.value;
        left= None;
        right= None;
      } in
      let tail= Option.map (map_right ~f init) elt.right in
      {
        length= t.length;
        head= Some init;
        tail;
      }

  let fold ~f ~init t=
    match t.head with
    | Some head-> Elt.fold_right ~f ~init head
    | None-> init
end

module Circle = struct
  type 'a t= {
    mutable size: int;
    mutable entry: 'a elt option;
  }
  and 'a elt= {
    value: 'a;
    circle: 'a t;
    mutable left: 'a elt;
    mutable right: 'a elt;
  }

  let left elt= elt.left

  let right elt= elt.right

  let size t= t.size

  let entry t= t.entry

  let set_entry t elt=
    if elt.circle == t then
      (t.entry <- Some elt;
      Ok ())
    else
      Error (Invalid_argument "the element does not belong to the circle")

  let init ~f n=
    if n < 0 then invalid_arg "length should be at least 0"
    else if n = 0 then
      {
        size= 0;
        entry= None;
      }
    else
    let rec circle= {
      size= n;
      entry= Some entry;
    }
    and entry= { value= f 0; circle; left= entry; right= entry } in
    let prev= ref entry in
    for i= 1 to n-1 do
      let rec next= { value= f i; circle; left= !prev; right= next } in
      !prev.right <- next;
      prev:= next;
    done;
    !prev.right <- entry;
    entry.left <- !prev;
    circle

  let insert_left elt value=
    let left= elt.left in
    let new_elt= {
      value;
      circle= elt.circle;
      left= elt.left;
      right= elt;
    }
    in
    left.right <- new_elt;
    elt.left <- new_elt;
    elt.circle.size <- elt.circle.size + 1

  let insert_right elt value=
    let right= elt.right in
    let new_elt= {
      value;
      circle= elt.circle;
      left= elt;
      right= elt.right;
    }
    in
    right.left <- new_elt;
    elt.right <- new_elt;
    elt.circle.size <- elt.circle.size + 1

  let remove elt=
    let circle= elt.circle in
    if circle.size = 1 then (
      circle.entry <- None;
      circle.size <- 0;
    ) else if circle.size > 1 then (
      let left= elt.left
      and right= elt.right in
      left.right <- right;
      right.left <- left;
      circle.entry
        |> Option.iter (fun entry->
          if entry == elt then
            circle.entry <- Some right);
      circle.size <- circle.size - 1;
    )

  let iter_left ~f elt=
    let entry= elt in
    let rec iter_left ~f elt=
      f elt.value;
      if elt.left != entry then iter_left ~f elt.left
    in
    iter_left ~f elt

  let iter_right ~f elt=
    let entry= elt in
    let rec iter_right ~f elt=
      f elt.value;
      if elt.right != entry then iter_right ~f elt.right
    in
    iter_right ~f elt

  let map ~f circle=
    match circle.entry with
    | Some elt->
      let entry_original= elt in
      let rec map ~f circle elt_left elt_from=
        if elt_from == entry_original then
          elt_left
        else
          let rec elt_new= {
            value= f elt_from.value;
            circle;
            left= elt_left;
            right= elt_new;
          }
          in
          elt_left.right <- elt_new;
          map ~f circle elt_new elt_from.right
      in
        let rec circle= {
          entry= Some entry;
          size= elt.circle.size;
        }
        and entry= {
          value= f elt.value;
          circle;
          left= entry;
          right= entry;
        } in
        let last= elt.right |> map ~f circle entry in
        entry.left <- last;
        last.right <- entry;
        circle
    | None->
      {
        entry= None;
        size= 0;
      }

  let fold_left ~f ~init t=
    match t.entry with
    | Some entry->
      let rec fold_left ~f ~acc elt=
        let acc= f acc elt.value in
        if elt.left == entry then
          acc
        else
          fold_left ~f ~acc elt.left
      in
      fold_left ~f ~acc:init entry
    | None-> init

  let fold_right ~f ~init t=
    match t.entry with
    | Some entry->
      let rec fold_right ~f ~acc elt=
        let acc= f acc elt.value in
        if elt.right == entry then
          acc
        else
          fold_right ~f ~acc elt.right
      in
      fold_right ~f ~acc:init entry
    | None-> init
end

