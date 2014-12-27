open Unix
open Printf
open String
open Bytes

let rec _unbiased_modulo ~(f:unit -> char) (element_count:int) : int=
  (** returns a char -1 < c < element_count **)
  let r = int_of_char( f () ) in
    if 256 mod element_count = 0 then
      r mod element_count
    else begin
      if r < element_count then r (* the number is in our keyspace *)
      else _unbiased_modulo f element_count (* try again *)
    end

let _random_unbiased_modulo () : (int -> int)=
  (** opens an entropy channel and returns a reader **)
  let random_channel = open_in_bin "/dev/random" in
  let f () = (input_char random_channel) in
  _unbiased_modulo ~f

let get1char () : (char)=
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH
	      { (Unix.tcgetattr Unix.stdin) with
        Unix.c_icanon = false; (* turn off line-editing and input parsing *)
        (* turn off local echo: *)
        Unix.c_echo = false;
        Unix.c_echoe = false;
        Unix.c_echok = false;
        Unix.c_echonl = false
      } in
    let res = input_char (in_channel_of_descr stdin) in
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
    res

let display_layout ~display ~alphabet ~remapped_alphabet =
  let linux_movehome= "\x1b[H\x1b[2J"      (* move cursor home + clear *)
  and kbd_color     = "\x1b[1;33m"  (* bold; yellow *)
  and key_color     = "\x1b[2;31m"  (* dim; red *)
  and clear_color   = "\x1b[39;49;21;22;24m" (* reset fg, bg, bold, dim, underline *)
  and qwerty_color  = "\x1b[4;1;33m" in (* underline; bold; yellow *)
  let kbd_sep_upper =  kbd_color ^ "|" ^ key_color
  and kbd_sep_lower  = kbd_color ^ "|" ^ qwerty_color
    (* US qwerty keyboard by design: SHIFT + key is == ASCII[key]-47 *)
  and shift_offset  = 47
  in if (String.length alphabet) <> (String.length remapped_alphabet) then
      display "Alphabet remapping is broken in scramlkb. Please fix!"
    else
    display (sprintf "%s%s\n%s%!"
      linux_movehome
      (* Top line of kbd UI *)
      (sprintf "%s%s" kbd_color " ____ ____ ____ ____ ____ ____ ____ ____ ____ ____ ____ ____ ____ __________")
      ( let rec kbd_line ~offset ~acc ~sep ~color ~numkeys ~acc_end ~kbdmap = function
        | x when x = numkeys ->
          acc ^ color ^ sep ^ acc_end ^ color ^ sep ^ clear_color ^ "\n"
        | i ->
          kbd_line ~acc:(
            sprintf "%s%s %c%c "
              ( if i=0 && acc <> ""
                then color ^ sep ^ acc
                else acc )
              sep
              kbdmap.[offset + i + shift_offset] (* key with no modifiers *)
              kbdmap.[offset + i] (* shift + key *)
          ) ~offset ~kbdmap ~numkeys ~sep ~color ~acc_end (i+1)
        in let display_kbd_line ~offset ~acc ~numkeys ~acc_end =
          (kbd_line ~offset ~acc ~numkeys ~acc_end ~kbdmap:remapped_alphabet ~sep:kbd_sep_upper ~color:kbd_color 0)
          ^(kbd_line ~offset ~acc ~numkeys ~acc_end ~kbdmap:alphabet ~sep:kbd_sep_lower ~color:qwerty_color 0)
        in
      (* Keyboard numberic row *)
          display_kbd_line ~offset:0 ~numkeys:13 ~acc:"" ~acc_end:"   <===   "
      (* Row of QWERTY keys *)
        ^ display_kbd_line ~offset:13 ~numkeys:13 ~acc:" TAB " ~acc_end:" <- "
      (* Row of ASDFG keys *)
        ^ display_kbd_line ~offset:(13+13) ~numkeys:11 ~acc:"CapsLck" ~acc_end:"            "
      (* Row of ZXCVB keys *)
        ^ display_kbd_line ~offset:(13+13+11) ~numkeys:10 ~acc:" SHIFT " ~acc_end:"   SHIFT   "
      )
    )

let swap arr i j =
  let temp = arr.[i] in
    Bytes.set arr i arr.[j];
    Bytes.set arr j temp
 
let fisher_yates_shuffle ~random_char remapped =
  let l = Bytes.length remapped in
    for i = (l-1) downto 1 do
      let r = random_char (i+1) in
        swap remapped i r
    done
    ; remapped

let retrieve_key_entry ~random_char ~display ~output =
  let alphabet      = "~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>?`1234567890-=qwertyuiop[]\\asdfghjkl;'zxcvbnm,./" in
  let rec get_char (output : string) =
    let remapped_alphabet =
      Bytes.to_string
      (fisher_yates_shuffle ~random_char (Bytes.of_string alphabet))
    in let () = display_layout ~display ~alphabet ~remapped_alphabet
    and     c = get1char ()
    in match c with
    | '\n' | '\r' -> (* enter/return - flush buffer and exit *)
      output ^ "\n"
    | '\x7f' -> (* backspace - remove last char in buffer *)
      get_char (String.sub output 0 ((max (String.length output) 1)-1))
    | c when String.contains alphabet c ->
      get_char (output ^ (String.make 1
      (* Look up real character in remapped alphabet *)
        alphabet.[(String.index remapped_alphabet c)]  ))
    | c ->
      get_char (output ^ (String.make 1 c))
  in
      output (get_char "")

let () =
  let random_char = _random_unbiased_modulo () in
  if Unix.isatty Unix.stdout then begin
    Printf.printf "You didn't redirect stdout. Error!\nUsage: (%s [mode, mode, ..]) where mode is either 'c'|'u'|'p' for unscrambled lines or an integer (count of scrambled lines)\nExample: (scramlkb c c 2 c) -> reads two plaintext lines, two scrambled lines, then one unscrambled line\n"
      Sys.argv.(0)
  ;  Pervasives.exit 2
  end else
    let output  s = Printf.fprintf (Unix.out_channel_of_descr Unix.stdout) "%s%!" s
    and display s = Printf.fprintf (Unix.out_channel_of_descr Unix.stderr) "%s%!" s
    and linux_cls     = "\x1b[2J"     (* clear screen:   tput clear *)
    and linux_save    = "\x1b[?1049h" (* save screen:    tput smcup *)
    and linux_restore = "\x1b[?1049l\x1b[0m" (* restore screen: rmcup *)
    in
    let rec parse_queue acc = function
      | hd::tl ->
        let mode =
          ( match hd with
            | "p" | "plain" | "c" | "clear" | "u" | "unscrambled"
                -> `Plaintext
            | x -> `Scrambled (int_of_string x)
          )
        in parse_queue (mode::acc) tl
      | [] -> List.rev acc
    in let queue =
      let argv =
        if 1 = Array.length Sys.argv then
          [ "1" ] (* default is to ask for one passphrase *)
        else
          List.map (Bytes.to_string)
            (List.tl (Array.to_list Sys.argv))
      in parse_queue [] argv
    in
    let rec process_queue = function
    | hd::tl ->
      let () =
      begin match hd with
      | `Plaintext ->
        output ((input_line (in_channel_of_descr stdin)) ^ "\n")
      | `Scrambled times ->
        let () = display (linux_save ^ linux_cls) in
        for i = 1 to times do 
          retrieve_key_entry ~random_char ~display ~output
        ; display (linux_cls ^ linux_restore)
        done
      end
      in process_queue tl
    | [] -> ()
    in
      process_queue queue

