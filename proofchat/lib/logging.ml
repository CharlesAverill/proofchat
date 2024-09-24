(** logging.ml - Custom logging and error messages *)

(*
  The two key functions here are _log and fatal, which do what they sound like
*)

(** Represents the severity of a log statement *)
type log_type =
  | Log_None
  | Log_Debug
  | Log_Info
  | Log_Warning
  | Log_Error
  | Log_Critical

let _GLOBAL_LOG_LEVEL = Log_Info

(** Follows the order in the type definition, \[0:5\]*)
let int_of_log = function
  | Log_Debug ->
      1
  | Log_Info ->
      2
  | Log_Warning ->
      3
  | Log_Error ->
      4
  | Log_Critical ->
      5
  | Log_None ->
      0

(** For exits, their appropriate return code and the message type *)
type return_code = int * string

let rc_Ok = (0, "OK")

and rc_Error = (1, "ERROR")

(** ANSI encoding for bold text *)
let ansi_bold = "\x1b[1m"

(** ANSI encoding for red text *)
let ansi_red = "\x1b[38:5:196m"

(** ANSI encoding for orange text *)
let ansi_orange = "\x1b[38:5:208m"

(** ANSI encoding for yellow text *)
let ansi_yellow = "\x1b[38:5:178m"

(** ANSI encoding for plain text *)
let ansi_reset = "\x1b[0m"

(** ANSI encoding for bold red text *)
let error_red = ansi_bold ^ ansi_red

(** ANSI encoding for bold orange text *)
let error_orange = ansi_bold ^ ansi_orange

(** ANSI encoding for bold yellow text *)
let error_yellow = ansi_bold ^ ansi_yellow

(** Gets the string representation of a {!log_type}*)
let string_of_log = function
  | Log_Debug ->
      ansi_bold ^ "[DEBUG]"
  | Log_Info ->
      ansi_bold ^ "[INFO]"
  | Log_Warning ->
      ansi_yellow ^ "[WARNING]"
  | Log_Error ->
      ansi_orange ^ "[ERROR]"
  | Log_Critical ->
      ansi_red ^ "[CRITICAL]"
  | Log_None ->
      ansi_reset ^ "[NONE]"

(** A fatal log statement that immediately exits the program *)
let fatal rc message =
  Printf.fprintf stderr
    "%s[%s] - %s%s\n----------------------------------------\n" error_red
    (snd rc) ansi_reset message ;
  flush stderr ;
  exit (fst rc)

(** Prints log statements to stdout/stderr *)
let _log log_level message =
  if log_level = Log_None || int_of_log _GLOBAL_LOG_LEVEL > int_of_log log_level
  then ()
  else
    let stream =
      if log_level = Log_Debug || log_level = Log_Info then stdout else stderr
    in
    Printf.fprintf stream "LOG:%s%s - %s\n" (string_of_log log_level) ansi_reset
      message ;
    flush stream
