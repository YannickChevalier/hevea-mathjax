# 12 "get.mll"
 
open Printf
open Misc
open Lexing
open Latexmacros
open Lexstate
open MyStack
open Length

(* Compute functions *)

exception Error of string

let sbool = function
  | true -> "true"
  | false -> "false"

let get_this = ref (fun _ -> assert false)
and get_fun = ref (fun _f _lexbuf -> assert false)
and open_env = ref (fun _ -> ())
and close_env = ref (fun _ -> ())
and get_csname = ref (fun _ -> assert false)
and main = ref (fun _ -> assert false)
;;

let bool_out = ref false
and int_out = ref false

let int_stack = MyStack.create "int_stack"
and bool_stack = MyStack.create "bool_stack"
and group_stack = MyStack.create "group_stack"
and just_opened = ref false

type saved =
  bool * bool MyStack.saved *
  bool * int MyStack.saved * 
  (unit -> unit) MyStack.saved * bool

let check () =
  !bool_out, MyStack.save bool_stack,
  !int_out, MyStack.save int_stack,
  MyStack.save group_stack,
  !just_opened

and hot (b,bs,i,is,gs,j) =
  bool_out := b ; MyStack.restore bool_stack bs ;
  int_out := i ; MyStack.restore int_stack is ;
  MyStack.restore group_stack gs ;
  just_opened := j

let push_int x =
  if !verbose > 2 then
    prerr_endline ("PUSH INT: "^string_of_int x) ;
  just_opened := false ;
  push int_stack x

let open_ngroups n =
  let rec open_ngroups_rec  = function
    | 0 ->()
    | n -> push group_stack (fun () -> ()) ; open_ngroups_rec (n-1) in
  if !verbose > 2 then
    prerr_endline ("OPEN NGROUPS: "^string_of_int n) ;
  if n > 0 then begin
    just_opened := true ;
    open_ngroups_rec n
  end

let close_ngroups n =
  let rec close_ngroups_rec  = function
    | 0 -> ()
    | n ->
        let f = pop group_stack in
        f() ; close_ngroups_rec (n-1) in
  if !verbose > 2 then
    prerr_endline ("CLOSE NGROUPS: "^string_of_int n);
  close_ngroups_rec n

let open_aftergroup f s =
  if !verbose > 2 then
    prerr_endline ("OPEN AFTER: "^s) ;
  just_opened := true ;
  push group_stack f


# 87 "get.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\239\255\240\255\062\000\014\000\243\255\244\255\245\255\
    \246\255\247\255\000\000\000\000\250\255\137\000\024\000\032\000\
    \001\000\002\000\255\255\003\000\055\000\163\000\000\000\000\000\
    \249\255\003\000\000\000\011\000\248\255\242\255\241\255\002\000\
    \224\000\001\000\011\000\026\001\254\255\012\000\255\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\015\000\015\000\255\255\255\255\255\255\
    \255\255\255\255\015\000\015\000\255\255\015\000\015\000\002\000\
    \001\000\015\000\255\255\255\255\003\000\004\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\014\000\
    \014\000\002\000\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_default = 
   "\002\000\000\000\000\000\030\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\019\000\000\000\019\000\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\000\000\000\000\000\000\255\255\
    \255\255\034\000\255\255\037\000\000\000\255\255\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\016\000\016\000\018\000\018\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \016\000\016\000\013\000\004\000\000\000\017\000\000\000\014\000\
    \006\000\005\000\008\000\009\000\030\000\009\000\000\000\008\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\000\000\000\000\007\000\007\000\007\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\000\000\000\000\003\000\035\000\000\000\000\000\
    \012\000\025\000\000\000\000\000\000\000\024\000\010\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\026\000\
    \028\000\000\000\022\000\027\000\011\000\023\000\000\000\000\000\
    \000\000\000\000\000\000\006\000\000\000\005\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\000\000\031\000\000\000\000\000\000\000\000\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \000\000\000\000\000\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\255\255\255\255\255\255\021\000\021\000\021\000\021\000\
    \021\000\021\000\030\000\036\000\038\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\000\000\000\000\000\000\255\255\000\000\
    \000\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\036\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\016\000\017\000\019\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\016\000\000\000\000\000\255\255\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\031\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\255\255\000\000\000\000\000\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\255\255\255\255\000\000\033\000\255\255\255\255\
    \000\000\010\000\255\255\255\255\255\255\023\000\000\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\025\000\
    \027\000\255\255\011\000\026\000\000\000\022\000\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\255\255\003\000\255\255\255\255\255\255\255\255\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\013\000\013\000\013\000\013\000\013\000\013\000\
    \255\255\255\255\255\255\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\021\000\021\000\021\000\021\000\
    \021\000\021\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\033\000\017\000\019\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\032\000\034\000\037\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\255\255\255\255\255\255\003\000\255\255\
    \255\255\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\035\000";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec result lexbuf =
    __ocaml_lex_result_rec lexbuf 0
and __ocaml_lex_result_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 101 "get.mll"
                      (result lexbuf)
# 266 "get.ml"

  | 1 ->
# 102 "get.mll"
                      (result lexbuf)
# 271 "get.ml"

  | 2 ->
# 105 "get.mll"
    (let lxm = Lexing.lexeme lexbuf in
    push_int (int_of_string lxm) ;
    result lexbuf)
# 278 "get.ml"

  | 3 ->
# 109 "get.mll"
    (let lxm = lexeme lexbuf in
    push_int
      (int_of_string ("0o"^String.sub lxm 1 (String.length lxm-1))) ;
    result lexbuf)
# 286 "get.ml"

  | 4 ->
# 114 "get.mll"
    (let lxm = lexeme lexbuf in
    push_int
      (int_of_string ("0x"^String.sub lxm 1 (String.length lxm-1))) ;
    result lexbuf)
# 294 "get.ml"

  | 5 ->
# 119 "get.mll"
    (let token = !get_csname lexbuf in
    after_quote (MyLexing.from_string token) ;
    result lexbuf)
# 301 "get.ml"

  | 6 ->
# 123 "get.mll"
    (push bool_stack true ;
    result lexbuf)
# 307 "get.ml"

  | 7 ->
# 126 "get.mll"
    (push bool_stack false ;
    result lexbuf)
# 313 "get.ml"

  | 8 ->
let
# 129 "get.mll"
               lxm
# 319 "get.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 130 "get.mll"
    (let unary = !just_opened in
    if unary then begin
      let f = pop group_stack in
      open_aftergroup
        (fun () ->
          if !verbose > 2 then begin
            prerr_endline ("UNARY: "^String.make 1 lxm) ;
            MyStack.pretty string_of_int int_stack
          end ;
          let x1 = pop int_stack in
          let r = match lxm with
          | '+' -> x1
          | '-' -> 0 - x1
          | _   -> assert false in
          push_int r ; f()) "UNARY"
    end else begin
      close_ngroups 2 ;
      open_aftergroup
        (fun () ->
          if !verbose > 2 then begin
            prerr_endline ("OPPADD: "^String.make 1 lxm) ;
            MyStack.pretty string_of_int int_stack
          end ;
          let x2 = pop int_stack in
          let x1 = pop int_stack in
          let r = match lxm with
          | '+' -> x1 + x2
          | '-' -> x1 - x2
          | _   -> assert false in
          push_int r) "ADD";
      open_ngroups 1 ;
    end ;
    result lexbuf)
# 355 "get.ml"

  | 9 ->
# 164 "get.mll"
    (let lxm = lexeme_char lexbuf 0 in
    close_ngroups 1 ;
    open_aftergroup
        (fun () ->
          if !verbose > 2 then begin
            prerr_endline ("MULTOP"^String.make 1 lxm) ;
            MyStack.pretty string_of_int int_stack
          end ;
          let x2 = pop int_stack in
          let x1 = pop int_stack in
          let r = match lxm with
          | '*' -> x1 * x2
          | '/' -> x1 / x2
          | _   -> assert false in
          push_int r) "MULT";
    result lexbuf)
# 375 "get.ml"

  | 10 ->
# 182 "get.mll"
    (let lxm = Lexing.lexeme_char lexbuf 0 in
    close_ngroups 3 ;
    open_aftergroup
      (fun () ->
        if !verbose > 2 then begin
          prerr_endline ("COMP: "^String.make 1 lxm) ;
          MyStack.pretty string_of_int int_stack
        end ;
        let x2 = pop int_stack in
        let x1 = pop int_stack in              
        push bool_stack
          (match lxm with
          | '<' -> x1 < x2
          | '>' -> x1 > x2
          | '=' -> x1 = x2
          | _   -> assert false) ;
          if !verbose > 2 then
            MyStack.pretty sbool bool_stack) "COMP" ;
    open_ngroups 2 ;
    result lexbuf)
# 399 "get.ml"

  | 11 ->
# 205 "get.mll"
    (open_ngroups 2 ;
    result lexbuf)
# 405 "get.ml"

  | 12 ->
# 208 "get.mll"
    (close_ngroups 2 ;
    result lexbuf)
# 411 "get.ml"

  | 13 ->
# 212 "get.mll"
    (let lxm = lexeme lexbuf in
    let i = Char.code (lxm.[1]) - Char.code '1' in
    scan_arg (scan_this_arg_list result) i ;
    result lexbuf)
# 419 "get.ml"

  | 14 ->
let
# 216 "get.mll"
                  lxm
# 425 "get.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 217 "get.mll"
    (let pat,body = Latexmacros.find lxm in
    let args = make_stack lxm pat lexbuf in
    scan_body
      (function
        | Subst body -> scan_this_list result body
        | Toks l ->
            List.iter
              (scan_this result)
              (List.rev l)
        | CamlCode f ->
            let rs = !get_fun f lexbuf in
            scan_this result rs)
          body args ;
    result lexbuf)
# 442 "get.ml"

  | 15 ->
# 231 "get.mll"
      (raise (Error ("Bad character in Get.result: ``"^lexeme lexbuf^"''")))
# 447 "get.ml"

  | 16 ->
# 232 "get.mll"
      (())
# 452 "get.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_result_rec lexbuf __ocaml_lex_state

and after_quote lexbuf =
    __ocaml_lex_after_quote_rec lexbuf 33
and __ocaml_lex_after_quote_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 236 "get.mll"
    (let lxm = lexeme lexbuf in
    push_int (Char.code lxm.[1]);
    result lexbuf)
# 465 "get.ml"

  | 1 ->
# 240 "get.mll"
    (let lxm = lexeme lexbuf in
    push_int (Char.code lxm.[0]);
    result lexbuf)
# 472 "get.ml"

  | 2 ->
# 244 "get.mll"
    (Misc.fatal "Cannot understand `-like numerical argument")
# 477 "get.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_after_quote_rec lexbuf __ocaml_lex_state

;;

# 245 "get.mll"
 
let init latexget latexgetfun latexopenenv latexcloseenv latexcsname
    latexmain =
  get_this := latexget ;
  get_fun := latexgetfun ;
  open_env := latexopenenv ;
  close_env := latexcloseenv ;
  get_csname := latexcsname ;
  main := latexmain
;;

let def_loc  name f = 
  Latexmacros.def name zero_pat (CamlCode f) ;
;;

let def_commands l =
  List.map
    (fun (name,f) ->
      name,Latexmacros.replace name (Some (zero_pat,CamlCode f)))
    l

let def_commands_int () =
  def_commands
    ["\\value",
      (fun lexbuf ->
        let name = !get_this (save_arg lexbuf) in
        push_int (Counter.value_counter name)) ;
      "\\@lengthtonchar",
      (fun lexbuf ->
        let length =
          Length.main
            (MyLexing.from_string
               (!get_this (save_arg lexbuf))) in        
        let r = match length with
        | Length.Char x -> x
        | Length.Pixel x -> pixel_to_char x
        | _ -> 2 in
        push_int r) ;
      "\\pushint",
      (fun lexbuf ->
        let s = !get_this (save_arg lexbuf) in
        scan_this result s)]

let def_commands_bool () =  
  let old_ints = def_commands_int () in
  let old_commands =
    def_commands
      ["\\(", (fun _ -> open_ngroups 7) ;
        "\\)",  (fun _ -> close_ngroups 7) ;
        "\\@fileexists",
        (fun lexbuf ->
          let name = !get_this (save_arg lexbuf) in
          push bool_stack
            (try
              let _,chan = Myfiles.open_tex name in
              begin try close_in chan with Sys_error _ -> () end ;
              true
            with Myfiles.Except | Myfiles.Error _ -> false)) ;
        "\\@commandexists",
        (fun lexbuf ->
          let name = !get_csname lexbuf in
(*          Printf.eprintf "EXISTS? '%s'\n" name ; *)
          push bool_stack (Latexmacros.exists name)) ;
        "\\or",
        (fun _ ->
          close_ngroups 7 ;
          open_aftergroup
            (fun () ->
              if !verbose > 2 then begin
                prerr_endline "OR" ;
                MyStack.pretty sbool bool_stack
              end ;
              let b1 = pop bool_stack in
              let b2 = pop bool_stack in
              push bool_stack (b1 || b2)) "OR";
          open_ngroups 6) ;
        "\\and",
        (fun _ ->
          close_ngroups 6 ;
          open_aftergroup
            (fun () ->
              if !verbose > 2 then begin
                prerr_endline "AND" ;
                MyStack.pretty sbool bool_stack
              end ;
              let b1 = pop bool_stack in
              let b2 = pop bool_stack in
              push bool_stack (b1 && b2)) "AND";            
          open_ngroups 5) ;
        "\\not",
        (fun _ ->
          close_ngroups 4 ;
          open_aftergroup
            (fun () ->
              if !verbose > 2 then begin
                prerr_endline "NOT" ;
                MyStack.pretty sbool bool_stack
              end ;
              let b1 = pop bool_stack in
              push bool_stack (not b1)) "NOT";
          open_ngroups 3) ;
        "\\boolean",
          (fun lexbuf ->
            let name = !get_this (save_arg lexbuf) in
            let b = try
              let r = !get_this
                  (string_to_arg ("\\if"^name^" true\\else false\\fi")) in
              match r with
              | "true" -> true
              | "false" -> false
              | _ -> raise (Misc.Fatal ("boolean value: "^r))
            with
              Latexmacros.Failed -> true  in
            push bool_stack b) ;
        "\\isodd",
        (fun _lexbuf ->
          close_ngroups 3 ;
          open_aftergroup
            (fun () ->
              if !verbose > 2 then begin
                prerr_endline ("ISODD") ;
                MyStack.pretty string_of_int int_stack
              end ;
              let x = pop int_stack in
              push bool_stack (x mod 2 = 1) ;
              if !verbose > 2 then
                MyStack.pretty sbool bool_stack) "ISODD" ;
          open_ngroups 2) ] in
  let old_equal =
    try Some (Latexmacros.find_fail "\\equal") with Failed -> None in
  
  def_loc "\\equal"
    (fun lexbuf ->
      let arg1 = save_arg lexbuf in
      let arg2 = save_arg lexbuf in
      scan_this !main "\\begin{@norefs}" ;
      let again = List.map (fun (name,x) -> name,Latexmacros.replace name x)
          ((("\\equal",old_equal)::old_ints)@old_commands) in
      push bool_stack (!get_this arg1 = !get_this arg2) ;
      let _ =
        List.map (fun (name,x) -> Latexmacros.replace name x) again in
      scan_this !main "\\end{@norefs}")



type 'a funs =
  { pp : out_channel -> 'a -> unit ;
    to_string : 'a -> string ;
    scan_this : (Lexing.lexbuf -> unit) -> 'a -> unit; }

let string_funs =
  { pp = output_string ;
    to_string = (fun x -> x) ;
    scan_this = scan_this; }
let list_funs =
  { pp = pretty_body ;
    to_string = String.concat "";
    scan_this = scan_this_list; }

let do_get_int f {arg=expr ; subst=subst} =
  if !verbose > 1 then  eprintf "get_int : '%a'\n%!" f.pp expr ;
  let r =
    let old_int = !int_out in
    int_out := true ;
    start_normal subst ;
    !open_env "*int*" ;
    let _ = def_commands_int () in
    open_ngroups 2 ;
    begin try f.scan_this result expr with
    | x ->
        begin
          eprintf
            "Error while scanning '%a' for integer result\n%!"
            f.pp expr ;
          raise x
        end
    end ;
    close_ngroups 2 ;
    !close_env "*int*" ;
    end_normal () ;
    if MyStack.empty int_stack then
      raise
        (Error
           (sprintf "'%s'' has no value as an integer" (f.to_string expr))) ;
    let r = pop int_stack in
    int_out := old_int ;
    r in
  if !verbose > 1 then eprintf "get_int: '%a' -> %i\n%!" f.pp expr r ;
  r
  
let get_int_string a = do_get_int string_funs a
let get_int a = do_get_int list_funs a

let get_bool {arg=expr ; subst=subst} =
  if !verbose > 1 then
    prerr_endline ("get_bool : "^expr) ;
  let old_bool = !bool_out in
  bool_out := true ;
  start_normal subst ;
  !open_env "*bool*" ;
  def_commands_bool () ;
  open_ngroups 7 ;
  begin try scan_this result expr with
  | x ->
      begin
        prerr_endline
          ("Error while scanning ``"^expr^"'' for boolean result");
        raise x
      end
  end ;
  close_ngroups 7 ;
  !close_env "*bool*" ;
  end_normal () ;
  if MyStack.empty bool_stack then
    raise (Error ("``"^expr^"'' has no value as a boolean"));
  let r = pop bool_stack in
  if !verbose > 1 then
    prerr_endline ("get_bool: "^expr^" = "^sbool r);
  bool_out := old_bool ;
  r

let get_length arg =
  if !verbose > 1 then
    prerr_endline ("get_length : "^arg) ;
  let r = Length.main (MyLexing.from_string arg) in
  if !verbose > 2 then begin
    prerr_string ("get_length : "^arg^" -> ") ;
    prerr_endline (Length.pretty r)
  end ;
  r

# 715 "get.ml"
