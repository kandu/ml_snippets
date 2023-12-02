let string_of_float f=
  let str= string_of_float f in
  if str.[String.length str - 1] == '.' then str ^ "0" else str

let string_of_chars char_list=
  String.concat ""
    (List.map (String.make 1) char_list)

