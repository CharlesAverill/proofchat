let bytes_of_byte_list (l : char list) : bytes =
  Bytes.concat Bytes.empty (List.map (fun c -> Bytes.make 1 c) l)

let int63_to_bytes n =
  List.map (fun i -> Char.chr ((n lsr (i * 8)) land 0xFF)) [0;1;2;3;4;5;6;7]

let bytes_to_int63 bytes =
  List.fold_left (fun a b -> a + (int_of_char b)) 0 bytes
