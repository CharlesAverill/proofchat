let bytes_of_byte_list (l : char list) : bytes =
  Bytes.concat Bytes.empty (List.map (fun c -> Bytes.make 1 c) l)

let int63_to_bytes n =
  let bytes = Bytes.create 8 in
  for i = 0 to 7 do
    Bytes.set bytes i (Char.chr ((n lsr (i * 8)) land 0xFF))
  done;
  bytes

let bytes_to_int63 bytes =
  let n = ref 0 in
  for i = 0 to 7 do
    n := !n lor ((Char.code (Bytes.get bytes i)) lsl (i * 8))
  done;
  !n
