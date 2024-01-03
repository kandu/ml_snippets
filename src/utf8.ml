let byte str i = Char.code (String.unsafe_get str i)

let fail str pos msg = failwith (Printf.sprintf "at position %d of %s: %s" pos str msg)

let next str ofs =
  let ch = String.unsafe_get str ofs in
  match ch with
    | '\x00' .. '\x7f' ->
      ofs + 1
    | '\xc0' .. '\xdf' ->
      if ofs + 2 > String.length str then
        fail str ofs "unterminated UTF-8 sequence"
      else
        ofs + 2
    | '\xe0' .. '\xef' ->
      if ofs + 3 > String.length str then
        fail str ofs "unterminated UTF-8 sequence"
      else
        ofs + 3
    | '\xf0' .. '\xf7' ->
      if ofs + 4 > String.length str then
        fail str ofs "unterminated UTF-8 sequence"
      else
        ofs + 4
    | _ ->
      fail str ofs "invalid start of UTF-8 sequence"

let unsafe_extract_next str ofs =
  let ch = String.unsafe_get str ofs in
  match ch with
    | '\x00' .. '\x7f' ->
      (Uchar.of_char ch, ofs + 1)
    | '\xc0' .. '\xdf' ->
      if ofs + 2 > String.length str then
        fail str ofs "unterminated UTF-8 sequence"
      else
        (Uchar.of_int (((Char.code ch land 0x1f) lsl 6) lor (byte str (ofs + 1) land 0x3f)), ofs + 2)
    | '\xe0' .. '\xef' ->
      if ofs + 3 > String.length str then
        fail str ofs "unterminated UTF-8 sequence"
      else
        (Uchar.of_int (
          ((Char.code ch land 0x0f) lsl 12) lor
          ((byte str (ofs + 1) land 0x3f) lsl 6)lor
          (byte str (ofs + 2) land 0x3f))
        , ofs + 3)
    | '\xf0' .. '\xf7' ->
      if ofs + 4 > String.length str then
        fail str ofs "unterminated UTF-8 sequence"
      else
        (Uchar.of_int (
          ((Char.code ch land 0x07) lsl 18) lor
          ((byte str (ofs + 1) land 0x3f) lsl 12) lor
          ((byte str (ofs + 2) land 0x3f) lsl 6) lor
          (byte str (ofs + 3) land 0x3f))
        , ofs + 4)
    | _ ->
      fail str ofs "invalid start of UTF-8 sequence"

let code_point_of_utf8 str=
  let len= String.length str in
  let core, next= unsafe_extract_next str 0 in
  if next >= len then
    (Uchar.to_int core, 0)
  else
    let variation, _= unsafe_extract_next str next in
    (Uchar.to_int core, Uchar.to_int variation)

