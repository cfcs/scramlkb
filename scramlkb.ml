open Unix
open Printf
open String
open Bytes

type systemd_askpassword_msg =
  {message : string ; socket : string; error : bool}

type scramlkb_operation =
| Systemdwatch of (Inotify.event list)
| Plaintext of ((string -> unit) * string)
| Scrambled of ((string -> unit) * string)

let random_char =
  (** opens an entropy channel and returns a reader **)
  let random_channel = open_in_bin "/dev/urandom" in
  let f () = (input_char random_channel) in
  let rec _unbiased_modulo ~(f:unit -> char) (element_count:int) : int=
    (** returns a char -1 < c < element_count **)
    let r = int_of_char( f () ) in
      if 256 mod element_count = 0 then
        r mod element_count
      else begin
        if r < element_count then r (* the number is in our keyspace *)
        else _unbiased_modulo f element_count (* try again *)
      end
  in
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

let display_layout ~display ~alphabet ~remapped_alphabet ~prompt =
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
    display (sprintf "%s%s%s\n%s\n%s%!"
      linux_movehome
      kbd_color
      prompt
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

let fisher_yates_shuffle ~random_char remapped =
  let swap arr i j =
    let temp = arr.[i] in
      Bytes.set arr i arr.[j];
      Bytes.set arr j temp
  in
  let l = Bytes.length remapped in
    for i = (l-1) downto 1 do
      let r = random_char (i+1) in
        swap remapped i r
    done
    ; remapped

let display s = Printf.fprintf (Unix.out_channel_of_descr Unix.stderr) "%s%!" s

let retrieve_key_entry ~prompt =
  let linux_cls     = "\x1b[2J"     (* clear screen:   tput clear *)
      and linux_save    = "\x1b[?1049h" (* save screen:    tput smcup *)
      and linux_restore = "\x1b[?1049l\x1b[0m" in (* restore screen: rmcup *)
  let () = display (linux_save ^ linux_cls) in
  let alphabet      = "~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>?`1234567890-=qwertyuiop[]\\asdfghjkl;'zxcvbnm,./" in
  let rec get_char (output : string) =
    let remapped_alphabet =
      Bytes.to_string
      (fisher_yates_shuffle ~random_char (Bytes.of_string alphabet))
    in
    let () = display_layout ~display ~alphabet ~remapped_alphabet ~prompt
    and  c = get1char ()
    in match c with
    | '\n' | '\r' -> (* enter/return - flush buffer and exit *)
      output
    | '\x7f' -> (* backspace - remove last char in buffer *)
      get_char (String.sub output 0 ((max (String.length output) 1)-1))
    | c when String.contains alphabet c ->
      get_char (output ^ (String.make 1
      (* Look up real character in remapped alphabet *)
        alphabet.[(String.index remapped_alphabet c)]  ))
    | c ->
      get_char (output ^ (String.make 1 c))
  in
  let output = get_char "" in
  let () = display (linux_cls ^ linux_restore) in
  output

let systemd_askpassword ~prompt reply_socket =
  let passphrase = retrieve_key_entry ~prompt in
  (* debug: let () = display ("got passphrase: '"^ passphrase ^"' -- "^ reply_socket) in *)
  let sock = Unix.(socket PF_UNIX SOCK_DGRAM 0) in
  (* TODO: this may fail due to permissions. solution? *)
  let () = Unix.connect sock (Unix.ADDR_UNIX reply_socket) in
  (* TODO should check that PID matches *)
  let passphrase = "+" ^ passphrase in (* systemd: "+" means "entry was a success" *)
  let _ = Unix.send sock passphrase 0 (String.length passphrase) [] in
  Unix.(shutdown sock SHUTDOWN_ALL)

let systemd_watch ~event_list =
  let open Inotify in
  let inotify_fd = Inotify.create () in
  let askpassword_dir = "/run/systemd/ask-password/" in
  let _ = Inotify.add_watch inotify_fd askpassword_dir [ S_Moved_to ; S_Close_write ] in
  let rec inotify_read () = 
    begin
      try Inotify.read inotify_fd
      with _ -> inotify_read ()
    end
  in
  let rec get_password_request : 'a list -> unit = function
  | []     -> get_password_request (inotify_read ()) (* get new events *)
  | current_event :: event_tl ->
    begin match current_event with
    | (_, _, _, Some file_name) when
      (Str.first_chars file_name 4) = "ask."
      ->
       let blank_msg = {message = ""; socket = ""; error = true } in
       begin try
         let msg =
           let fh = open_in (askpassword_dir ^ file_name) in
           let rec readall acc =
             let line = begin try input_line fh with e -> "e-o-f" end in
             if line = "e-o-f" then acc else (* EOF *)
             let hd, tl = let open Str in
             let pair = Str.bounded_split (Str.regexp "=") line 2 in
             (List.hd pair, List.tl pair)
             in
             let acc = begin match lowercase_ascii hd with
             | "message" -> {acc with message = List.hd tl}
             | "socket"  -> {acc with socket  = List.hd tl}
             | _ -> acc
             end
             in readall acc
           in
             readall blank_msg
         in
           systemd_askpassword ~prompt:msg.message msg.socket
       with
       | Sys_error _ -> () (* TODO currently failing silently *)
       end
    | _ ->
      get_password_request event_tl (* event read error *)
    end
  in get_password_request []

let handle_operation = function
  | Plaintext (callback, prompt) ->
      let () = display prompt in
      callback ((input_line (in_channel_of_descr stdin)))
  | Scrambled (callback, prompt) ->
      let passphrase = retrieve_key_entry ~prompt in
      callback passphrase
  | Systemdwatch event_list ->
      systemd_watch ~event_list

let () =
  let output  s = Printf.fprintf (Unix.out_channel_of_descr Unix.stdout) "%s%!" s in
  let output_nl s = output (s ^ "\n") in
  let rec parse_cmdline_queue (acc : scramlkb_operation list) = function
      | (hd::tl) ->
        let mode : scramlkb_operation =
          ( match hd with
            | "p" | "plain" | "c" | "clear" | "u" | "unscrambled"
                -> Plaintext (output_nl , "Enter passphrase: ")
            | "--watch"
                -> Systemdwatch []
            | x -> Scrambled (output , "Enter passphrase: ")
          )
        in begin match mode with
        | Plaintext _ | Scrambled _ ->
          parse_cmdline_queue (mode::acc) tl
        | Systemdwatch _ ->
          [mode]
        end
     | [] -> List.rev acc
    in
    let operation_queue =
      let argv =
        if 1 = Array.length Sys.argv then
          [ "1" ] (* default when no arguments is to ask for one passphrase *)
        else
          List.map (Bytes.to_string)
            (List.tl (Array.to_list Sys.argv))
      in parse_cmdline_queue [] argv
    in
    if operation_queue = [Systemdwatch []] then
       handle_operation List.(hd operation_queue)
    else begin
        let rec process_queue op_lst : unit =
          begin match op_lst with
          | hd::tl ->
              let () = handle_operation hd in process_queue tl
          | [] -> ()
          end
        in
        if Unix.isatty Unix.stdout
        then begin
        (* display a warning if we're not using systemd and we're not redirecting the output *)
          let () =
          Printf.eprintf "You didn't redirect stdout. Error!\nUsage: (%s [mode, mode, ..]) where mode is either 'c'|'u'|'p' for unscrambled lines or an integer (count of scrambled lines)\nExample: (scramlkb c c 2 c) -> reads two plaintext lines, two scrambled lines, then one unscrambled line\n" Sys.argv.(0) in
          Pervasives.exit 2
        end else process_queue operation_queue
    end
