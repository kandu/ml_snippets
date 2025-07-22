open MiniParsec
open Utils

module Make(NL: NL) = struct
  let ( let* )= bind

  include Make(NL)

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
end
