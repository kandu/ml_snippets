let string_of_float f=
  let str= string_of_float f in
  if str.[String.length str - 1] == '.' then str ^ "0" else str

let string_of_chars char_list=
  String.concat ""
    (List.map (String.make 1) char_list)

type nl=
  | N
  | R
  | RN
  | All

let line_break_count ?(nl=All) str=
  let len= String.length str in
  let rec find count pos=
    match len - pos with
    | n when n <=0 -> count
    | 1-> step count pos
    | _->
      match nl with
      | RN | All ->
        if (str.[pos], str.[pos+1]) = ('\r','\n') then
          find (count+1) (pos+2)
        else
          step count pos
      | _-> step count pos
  and step count pos=
    let next= pos+1 in
    match str.[pos] with
    | '\r'->
      (match nl with
      | R | All -> find (count+1) next
      | N | RN -> find count next)
    | '\n'->
      (match nl with
      | N | All -> find (count+1) next
      | R | RN -> find count next)
    | _-> find count next
  in
  find 0 0

let to_lines ?(nl=All) str=
  let len= String.length str in
  let rec build acc prev pos=
    match len - pos with
    | n when n <=0 -> String.sub str prev (pos-prev) :: acc
    | 1-> step acc prev pos
    | _->
      match nl with
      | RN | All ->
        if (str.[pos], str.[pos+1]) = ('\r','\n') then
          let acc= String.sub str prev (pos-prev) :: acc in
          build acc (pos+2) (pos+2)
        else
          step acc prev pos
      | _-> step acc prev pos
  and step acc prev pos=
    let next= pos+1 in
    match str.[pos] with
    | '\r'->
      (match nl with
      | R | All ->
        let acc= String.sub str prev (pos-prev) :: acc in
        build acc next next
      | N | RN ->
        build acc prev next)
    | '\n'->
      (match nl with
      | N | All ->
        let acc= String.sub str prev (pos-prev) :: acc in
        build acc next next
      | R | RN ->
        build acc prev next)
    | _-> build acc prev next in
  build [] 0 0 |> List.rev

let rec find_last_bol ?(nl=All) str=
  match nl with
  | N->
    (try Some (String.rindex str '\n' + 1) with _-> None)
  | R->
    (try Some(String.rindex str '\r' + 1) with _-> None)
  | RN->
    (try
      let pos= String.rindex str '\n' in
      if str.[pos-1] = '\r' then
        Some (pos+1)
       else
         None
    with _-> None)
  | All->
    let pn= find_last_bol ~nl:N str
    and pr= find_last_bol ~nl:R str in
    match pn, pr with
    | (Some pn, Some pr)-> Some (max pn pr)
    | Some pn, None-> Some pn
    | None, Some pr-> Some pr
    | None, None-> None

