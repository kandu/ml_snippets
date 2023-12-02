open Printf

type pos= {
  cnum: int;
  line: int;
  bol: int;
}

type state= {
  data: string;
  maxlen: int;
  pos: pos
}

let initState data= {
  data;
  maxlen= String.length data;
  pos= {
    cnum= 0;
    line= 1;
    bol= 0;
    };
}

type error= pos * string

type 'a reply= (('a * state), error) result

type 'a parser= state -> 'a reply
type 'a t= 'a parser


let string_of_pos pos= sprintf "line %d, characters %d"
  pos.line (pos.cnum - pos.bol)

let string_of_pos_full pos= sprintf "offset %d, line %d, characters %d"
  pos.cnum pos.line (pos.cnum - pos.bol)

(* parser generator *)

let any= fun state->
  let pos= state.pos in
  if pos.cnum < state.maxlen then
    let found= String.get state.data state.pos.cnum in
    let pos= { pos with cnum= pos.cnum + 1 } in
    (Ok (found, { state with pos }))
  else
    (Error (state.pos, "out of bounds"))

let char c= fun state->
  let pos= state.pos in
  if pos.cnum < state.maxlen then
    let found= String.get state.data pos.cnum in
    if found = c then
      let pos= { pos with cnum= pos.cnum + 1 } in
      (Ok (found, { state with pos }))
    else
      Error (
        state.pos,
        sprintf "\"%c\" expected but \"%c\" found" c found)
  else
    (Error (state.pos, "out of bounds"))

let string str= fun state->
  let pos= state.pos in
  let len= String.length str in
  if state.maxlen - pos.cnum >= len then
    let found= String.sub state.data pos.cnum len in
    if found = str then
      let pos= { pos with cnum= pos.cnum + len } in
      (Ok (found, { state with pos }))
    else
      Error (
        state.pos,
        sprintf "\"%s\" expected but \"%s\" found" str found)
  else
    (Error (state.pos, "out of bounds"))


let satisfy test= fun state->
  let pos= state.pos in
  if pos.cnum < state.maxlen then
    let found= String.get state.data pos.cnum in
    if test found then
      let pos= { pos with cnum= pos.cnum + 1 } in
      (Ok (found, { state with pos }))
    else
      Error (
        state.pos,
        sprintf "\"%c\" isn't satisfied" found)
  else
    (Error (state.pos, "out of bounds"))

(* combinator *)
let fail msg= fun state-> Error (state.pos, msg)

let return v= fun state-> Ok (v, state)

let bind (p: 'a parser) (f: 'a -> 'b parser)= fun state->
  let result= p state in
  match result with
  | Error e-> Error e
  | Ok (v,state)-> f v state

let (>>=)= bind
let (>>) p1 p2= p1 >>= fun _ -> p2
let (<<) p1 p2= p1 >>= fun x-> p2 >> return x
let (|>>) p f= p >>= fun v-> return (f v)
let (>>$) p v= p >> return v

let (<|>) (p1:'a parser) (p2:'a parser)= fun state->
  let result= p1 state in
  match result with
  | Error _-> p2 state
  | Ok _-> result

let between left right p= left >> p << right

let many p=
  let rec parser s=
    (((p |>> fun v-> Some v) <|> return None) >>= (function
      | Some v-> parser |>> (fun r-> v :: r)
      | None-> return []))
      s
  in parser

let many1 p=
  p >>= fun v-> many p |>> fun l-> v :: l

let rec times num p s=
  if num > 0 then
    (p >>= (fun v-> times (num-1) p |>> (fun r-> v::r))) s
  else
    (return []) s

let sepStartBy sep p= many (sep >> p)

let sepStartBy1 sep p= many1 (sep >> p)

let sepEndBy sep p= many (p << sep)

let sepEndBy1 sep p= many1 (p << sep)

let sepBy1 sep p=
  p >>= fun head->
  sepStartBy sep p >>= fun body->
  return (head :: body)

let sepBy sep p= sepBy1 sep p <|> return []

let opt default p=
  p <|> return default

let option p= p |>> (fun v-> Some v) <|> return None

let lookAhead p= fun state->
  let reply= p state in
  match reply with
  | Ok (r, newState)-> Ok (r, state)
  | Error _-> reply
  [@@ocaml.warning "-27"]

let followedBy p msg= fun state->
  let reply= p state in
  match reply with
  | Ok _-> Ok ((), state)
  | Error _-> Error (state.pos, msg)

let notFollowedBy p msg= fun state->
  let reply= p state in
  match reply with
  | Ok _-> Error (state.pos, msg)
  | Error _-> Ok ((), state)

(* parser *)
let eof state=
  if state.pos.cnum >= state.maxlen
  then Ok ((), state)
  else Error (state.pos, "not eof")

let newline_lf state=
  let pos= state.pos in
  if pos.cnum < state.maxlen then
    let found= String.get state.data pos.cnum in
    if found = '\n' then
      let cnum= pos.cnum + 1
      and line= pos.line + 1 in
      let bol= cnum in
      let pos= { cnum; line; bol } in
      (Ok (String.make 1 found, { state with pos }))
    else
      Error (
        state.pos,
        sprintf "newline-lf expected but \"%c\" found" found)
  else
    (Error (state.pos, "out of bounds"))

let newline_cr state=
  let pos= state.pos in
  if pos.cnum < state.maxlen then
    let found= String.get state.data pos.cnum in
    if found = '\r' then
      let cnum= pos.cnum + 1
      and line= pos.line + 1 in
      let bol= cnum in
      let pos= { cnum; line; bol } in
      (Ok (String.make 1 found, { state with pos }))
    else
      Error (
        state.pos,
        sprintf "newline-cr expected but \"%c\" found" found)
  else
    (Error (state.pos, "out of bounds"))

let newline_crlf state=
  let pos= state.pos in
  if pos.cnum + 2 <= state.maxlen then
    let found= String.sub state.data pos.cnum 2 in
    if found = "\r\n" then
      let cnum= pos.cnum + 1
      and line= pos.line + 1 in
      let bol= cnum in
      let pos= { cnum; line; bol } in
      (Ok (found, { state with pos }))
    else
      Error (
        state.pos,
        sprintf "newline-crlf expected but \"%s\" found" found)
  else
    (Error (state.pos, "out of bounds"))

let newline_lfcr state=
  let pos= state.pos in
  if pos.cnum + 2 <= state.maxlen then
    let found= String.sub state.data pos.cnum 2 in
    if found = "\n\r" then
      let cnum= pos.cnum + 1
      and line= pos.line + 1 in
      let bol= cnum in
      let pos= { cnum; line; bol } in
      (Ok (found, { state with pos }))
    else
      Error (
        state.pos,
        sprintf "newline-lfcr expected but \"%s\" found" found)
  else
    (Error (state.pos, "out of bounds"))

let newline= newline_crlf <|> newline_lfcr
  <|> newline_lf <|> newline_cr


let int8= any |>> int_of_char

let int16= any >>= fun l-> any
  |>> fun h-> int_of_char h lsl 8 + int_of_char l
let int16_net= any >>= fun h-> any
  |>> fun l-> int_of_char h lsl 8 + int_of_char l

let int32= int16 >>= fun l-> int16
  |>> fun h-> Int32.(add (shift_left (of_int h) 16) (of_int l))
let int32_net= int16_net >>= fun h-> int16_net
  |>> fun l-> Int32.(add (shift_left (of_int h) 16) (of_int l))

let int64= int32 >>= fun l-> int32
  |>> fun h-> Int64.(add (shift_left (of_int32 h) 32) (of_int32 l))
let int64_net= int32_net >>= fun h-> int32_net
  |>> fun l-> Int64.(add (shift_left (of_int32 h) 32) (of_int32 l))

let num_dec= satisfy (fun c->
  '0' <= c && c <= '9')

let num_bin= satisfy (fun c->
  c = '0' || c = '1')

let num_oct= satisfy (fun c->
  '0' <= c && c <= '7')

let num_hex= satisfy (fun c->
  '0' <= c && c <= '9'
  || 'a' <= c && c <= 'f'
  || 'A' <= c && c <= 'F')

let lowercase= satisfy (fun c->
  'a' <= c && c <= 'z')

let uppercase= satisfy (fun c->
  'A' <= c && c <= 'Z')

(* start parsing *)
let parse_string parser str= parser (initState str)

