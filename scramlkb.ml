open Unix
open Printf
open Buffer
open String

let rec _unbiased_modulo ~(f:unit -> char) (element_count:char) : char=
  (** returns a char -1 < c < element_count **)
  let r = f () in
    if 256 mod int_of_char element_count = 0 then
      char_of_int (int_of_char r mod int_of_char element_count)
    else begin
      if r < element_count then r (* the number is in our keyspace *)
      else _unbiased_modulo f element_count (* try again *)
    end

let _random_unbiased_modulo () : (char -> char)=
  (** opens an entropy channel and returns a reader **)
  let random_channel = open_in_bin "/dev/random" in
  let f () = (input_char random_channel) in
  _unbiased_modulo ~f

let get1char () : (char)=
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH
      { termio with
        Unix.c_icanon = false; (* turn off line-editing and input parsing *)
        Unix.c_echo = false (* turn off local echo *)
      } in 
    let res = input_char (in_channel_of_descr stdin) in
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
    res

let print_layout () =
  let linux_cls     = "\x1b[2J"     (* tput clear *)
  and linux_save    = "\x1b[?1049h" (* tput smcup *)
  and linux_restore = "\x1b[?1049l" (* tput rmcup *)
  and linux_movehome= "\x1b[H"      (* move cursor home *)
  and alphabet      = "~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>?`1234567890-=qwertyuiop[]\\asdfghjkl;'zxcvbnm,./"
  and shift_offset  = 47
  in

  ()

let retrieve_key_entry () =
  let rec get_char (output : string) =
    let c = get1char () in
    match c with
    | '\n' | '\r' -> (* enter/return - flush buffer and exit *)
      output
    | '\x7f' -> (* backspace - remove last char in buffer *)
      get_char (String.sub output 0 ((max (String.length output) 1)-1))
    | _ as c -> let ret = begin match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | _ ->
        (* add dictionary lookup here *)
        get_char (output ^ (String.make 1 c))
      end in ret
  in
    Printf.fprintf
      (Unix.out_channel_of_descr Unix.stdout)
      "%s" (get_char (String.make 0 'x'))

let () =
  let random_char = _random_unbiased_modulo () in
  if Unix.isatty Unix.stdout then begin
      Printf.printf "You didn't redirect stdout. Error!\n";
      Pervasives.exit 2
  end else
    let () = retrieve_key_entry () in ()

