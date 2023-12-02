open MiniParsec
open Utils

let ( let* )= bind

let space= char ' ' <|> char '\t' <|> (newline |>> (fun _-> '\n'))

let spaces= many space

let float=
  let* neg= option (char '-') in
  let* integer= many1 num_dec << option (char '.') in
  let* fractional= option (many1 num_dec) in
  let neg= Option.is_some neg in
  let integer= integer |> string_of_chars |> float_of_string in
  let fractional= match fractional with
    | None-> 0.
    | Some chars-> "." ^ (string_of_chars chars) |> float_of_string
  in
  let num= integer +. fractional in
  let result= if neg then -. num else num in
  return result

