open Unix

let bytes_of_char_list (l : char list) : bytes =
  Bytes.concat Bytes.empty (List.map (fun c -> Bytes.make 1 c) l)

let char_list_of_bytes (b : bytes) : char list =
  let l = ref [] in
  for i = 0 to Bytes.length b - 1 do
    l := Bytes.get b i :: !l
  done;
  List.rev !l

let int63_to_bytes (n : int) : char list =
  List.map (fun i -> Char.chr ((n lsr (i * 8)) land 0xFF)) [0;1;2;3;4;5;6;7]

let bytes_to_int63 (b : char list) : int =
  if List.length b != 8 then
    raise (Invalid_argument 
      ("bytes_to_int63 expects 8 bytes, got " ^ (string_of_int (List.length b))))
  else
  List.fold_left (fun a b -> a + (int_of_char b)) 0 b

let functional_read (fd : file_descr) (pos : int) (len : int) (flags : msg_flag list) 
    : int * char list =
  let b = Bytes.create len in
  let num_read = recv fd b pos len flags in
  (num_read, char_list_of_bytes b)
