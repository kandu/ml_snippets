module type OrderedType = Set.OrderedType

module Make(Ord: OrderedType) =
struct
  module Bundle = struct

    type t = Ord.t list

    let length= List.length

    let singleton e= [e]

    let add t e=
      match t with
      | []-> [e]
      | hd::_->
        if Ord.compare hd e = 0 then
          e::t
        else
          invalid_arg "incompatible"

    let compare b1 b2=
      match (b1, b2) with
      | [], _
      | _, []->
        invalid_arg "incompatible"
      | h1::_, h2::_ -> Ord.compare h1 h2
  end

  module Set0 = struct
    include Set.Make(Bundle)
    let update e1 e2 s=
      s |> remove e1 |> add e2
  end

  let empty= Set0.empty
  let is_empty= Set0.is_empty

  let add e s=
    match Set0.find_opt [e] s with
    | None-> Set0.add [e] s
    | Some b-> Set0.update b (Bundle.add b e) s

  let pop_min s=
    match Set0.min_elt_opt s with
    | Some (hd::[])->
      Some hd, Set0.remove [hd] s
    | Some (hd::tl)-> Some hd, Set0.update (hd::tl) tl s
    | Some []-> assert false
    | None-> None, s

  let pop_max s=
    match Set0.max_elt_opt s with
    | Some (hd::[])->
      Some hd, Set0.remove [hd] s
    | Some (hd::tl)-> Some hd, Set0.update (hd::tl) tl s
    | Some []-> assert false
    | None-> None, s

end

