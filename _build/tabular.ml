# 13 "tabular.mll"
 
open Misc
open Lexing
open Table
open Lexstate
open Subst

exception Error of string
;;

type align =
    {hor : string ; mutable vert : string ; wrap : bool ;
      mutable pre : string ; mutable post : string ; width : Length.t}

let make_hor = function
    'c' -> "center"
  | 'l' -> "left"
  | 'r' -> "right"
  | 'p'|'m'|'b' -> "left"
  | _ -> raise (Misc.Fatal "make_hor")

and make_vert = function
  | 'c'|'l'|'r' -> ""
  | 'p' -> "top"
  | 'm' -> "middle"
  | 'b' -> "bottom"
  | _ -> raise (Misc.Fatal "make_vert")

type format =
  Align of align
| Inside of string
| Border of string
;;

(* Patch vertical alignment (for HTML) *)
let check_vert f =
  try
    for i = 0 to Array.length f-1 do
      match f.(i) with
      | Align {vert=s} when s <> "" -> raise Exit
      | _ -> ()
    done ;
    f
  with Exit -> begin
    for i = 0 to Array.length f-1 do
      match f.(i) with
      | Align ({vert=""} as f) ->
          f.vert <- "top"
      | _ -> ()
    done ;
    f
  end

(* Compute missing length (for text) *)
and check_length f =
  for i = 0 to Array.length f - 1 do
    match f.(i) with
    | Align ({wrap=true ; width=Length.No _} as r) ->
        f.(i) <-
           Align
             {r with
              width =
              Length.Percent
                (truncate (100.0 /. float (Array.length f)))}
    | _ -> ()
  done

let border = ref false



let out_table = Table.create (Inside "")

let pretty_format = function
  |   Align {vert = v ; hor = h ; pre = pre ; post = post ; wrap = b ; width = w}
      ->
        "[>{"^pre^"}"^
        ", h="^h^", v="^v^
        ", <{"^post^"}"^(if b then ", wrap" else "")^
        ", w="^Length.pretty w^"]"
  | Inside s -> "@{"^s^"}"
  | Border s -> s

let pretty_formats f =
  Array.iter (fun f -> prerr_string (pretty_format f) ; prerr_string "; ") f

(* For some reason pre/post-ludes are executed right to left *)
let concat_pre_post x y = match x, y with
| "", _ -> y
| _, "" -> x
| _,_   -> y ^ "{}" ^ x

# 95 "tabular.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\254\255\255\255\005\000\249\255\250\255\001\000\000\000\
    \253\255\254\255\255\255\252\255\251\255\002\000\011\000\253\255\
    \254\255\255\255\026\000\250\255\023\000\252\255\253\255\254\255\
    \255\255\251\255";
  Lexing.lex_backtrk = 
   "\002\000\255\255\255\255\007\000\255\255\255\255\005\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\003\000\255\255\
    \255\255\255\255\006\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_default = 
   "\255\255\000\000\000\000\005\000\000\000\000\000\013\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\013\000\255\255\000\000\
    \000\000\000\000\255\255\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\002\000\002\000\012\000\012\000\002\000\010\000\010\000\
    \000\000\000\000\010\000\000\000\017\000\017\000\000\000\000\000\
    \017\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\000\000\000\000\024\000\024\000\010\000\255\255\024\000\
    \007\000\000\000\006\000\017\000\000\000\000\000\000\000\000\000\
    \000\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\024\000\021\000\000\000\020\000\001\000\000\000\
    \000\000\255\255\000\000\255\255\023\000\255\255\000\000\016\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\021\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\008\000\
    \009\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\009\000\008\000\000\000\000\000\008\000\000\000\009\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\022\000\000\000\
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
    \000\000\255\255\255\255\000\000\000\000\004\000\000\000\000\000\
    \000\000\000\000\000\000\015\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\019\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\006\000\013\000\000\000\003\000\003\000\
    \255\255\255\255\003\000\255\255\014\000\014\000\255\255\255\255\
    \014\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\018\000\018\000\003\000\003\000\018\000\
    \003\000\255\255\003\000\014\000\255\255\255\255\255\255\255\255\
    \255\255\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\018\000\018\000\255\255\018\000\000\000\255\255\
    \255\255\003\000\255\255\003\000\018\000\003\000\255\255\014\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\018\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\003\000\
    \003\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\003\000\003\000\255\255\255\255\003\000\255\255\003\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\003\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\018\000\255\255\
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
    \255\255\006\000\013\000\255\255\255\255\003\000\255\255\255\255\
    \255\255\255\255\255\255\014\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\018\000";
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

let rec tfone lexbuf =
    __ocaml_lex_tfone_rec lexbuf 0
and __ocaml_lex_tfone_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 107 "tabular.mll"
                    (tfone lexbuf)
# 207 "tabular.ml"

  | 1 ->
# 109 "tabular.mll"
    (let pre = subst_arg lexbuf in
    tfone lexbuf ;
    try
      apply out_table (function
        |  Align a ->
            a.pre <- concat_pre_post pre a.pre ;
        | _ -> raise (Error "Bad syntax in array argument (>)"))
    with Table.Empty ->
      raise (Error "Bad syntax in array argument (>)"))
# 220 "tabular.ml"

  | 2 ->
# 118 "tabular.mll"
     (tfmiddle lexbuf)
# 225 "tabular.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_tfone_rec lexbuf __ocaml_lex_state

and tfmiddle lexbuf =
    __ocaml_lex_tfmiddle_rec lexbuf 3
and __ocaml_lex_tfmiddle_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 121 "tabular.mll"
                    (tfmiddle lexbuf)
# 236 "tabular.ml"

  | 1 ->
# 123 "tabular.mll"
  (let f = Lexing.lexeme_char lexbuf 0 in
  let post = tfpostlude lexbuf in
  emit out_table
    (Align {hor = make_hor f ; vert = make_vert f ; wrap = false ;
        pre = "" ;   post = post ; width = Length.Default}))
# 245 "tabular.ml"

  | 2 ->
# 129 "tabular.mll"
  (let f = Lexing.lexeme_char lexbuf 0 in
  let width = subst_arg lexbuf in
  let my_width = Length.main (MyLexing.from_string width) in
  let post = tfpostlude lexbuf in
  emit out_table
    (Align {hor = make_hor f ; vert = make_vert f ; wrap = true ;
          pre = "" ;   post = post ; width = my_width}))
# 256 "tabular.ml"

  | 3 ->
# 137 "tabular.mll"
    (let lxm = lexeme lexbuf in
    let i = Char.code (lxm.[1]) - Char.code '1' in
    Lexstate.scan_arg (scan_this_arg_list tfmiddle) i)
# 263 "tabular.ml"

  | 4 ->
# 141 "tabular.mll"
    (tfmiddle lexbuf)
# 268 "tabular.ml"

  | 5 ->
# 143 "tabular.mll"
    (let lxm = lexeme lexbuf in
    let name = column_to_command lxm in
    let pat,body = Latexmacros.find name in
    let args = Lexstate.make_stack name pat lexbuf in
    let cur_subst = get_subst () in
    Lexstate.scan_body
      (function
        | Lexstate.Subst body ->
            scan_this_list_may_cont
              lexformat lexbuf  cur_subst (string_to_arg body) ;            
        | _ -> assert false)
      body args ;
    let post = tfpostlude lexbuf in
    if post <> "" then
      try
        Table.apply out_table
          (function
            | Align f -> f.post <- post
            | _ -> Misc.warning ("``<'' after ``@'' in tabular arg scanning"))
      with
      | Table.Empty ->
          raise (Error ("``<'' cannot start tabular arg")))
# 294 "tabular.ml"

  | 6 ->
# 165 "tabular.mll"
      (())
# 299 "tabular.ml"

  | 7 ->
# 167 "tabular.mll"
  (let rest =
    Bytes.sub_string lexbuf.lex_buffer lexbuf.lex_curr_pos
      (lexbuf.lex_buffer_len - lexbuf.lex_curr_pos) in
  raise (Error ("Syntax of array format near: "^rest)))
# 307 "tabular.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_tfmiddle_rec lexbuf __ocaml_lex_state

and tfpostlude lexbuf =
    __ocaml_lex_tfpostlude_rec lexbuf 14
and __ocaml_lex_tfpostlude_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 173 "tabular.mll"
                    (tfpostlude lexbuf)
# 318 "tabular.ml"

  | 1 ->
# 175 "tabular.mll"
    (let one = subst_arg lexbuf in
    let rest = tfpostlude lexbuf in
    let r = concat_pre_post one rest in
    r)
# 326 "tabular.ml"

  | 2 ->
# 180 "tabular.mll"
    (if MyStack.empty stack_lexbuf then
      ""
    else
      let lexbuf = previous_lexbuf () in
      tfpostlude lexbuf)
# 335 "tabular.ml"

  | 3 ->
# 185 "tabular.mll"
      ("")
# 340 "tabular.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_tfpostlude_rec lexbuf __ocaml_lex_state

and lexformat lexbuf =
    __ocaml_lex_lexformat_rec lexbuf 18
and __ocaml_lex_lexformat_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 189 "tabular.mll"
                    (lexformat lexbuf)
# 351 "tabular.ml"

  | 1 ->
# 191 "tabular.mll"
   (let ntimes = save_arg lexbuf in
   let what = save_arg lexbuf in
   let rec do_rec = function
     0 -> lexformat lexbuf
   | i ->
      scan_this_arg lexformat what ; do_rec (i-1) in
   do_rec (Get.get_int_string ntimes))
# 362 "tabular.ml"

  | 2 ->
# 198 "tabular.mll"
      (border := true ; emit out_table (Border "|") ; lexformat lexbuf)
# 367 "tabular.ml"

  | 3 ->
# 200 "tabular.mll"
    (let lxm = Lexing.lexeme_char lexbuf 0 in
    let inside = subst_arg lexbuf in
    if lxm = '!' || inside <> "" then emit out_table (Inside inside) ;
    lexformat lexbuf)
# 375 "tabular.ml"

  | 4 ->
# 205 "tabular.mll"
    (let lxm = lexeme lexbuf in
    let i = Char.code (lxm.[1]) - Char.code '1' in
    Lexstate.scan_arg (scan_this_arg_list lexformat) i ;
    lexformat lexbuf)
# 383 "tabular.ml"

  | 5 ->
# 210 "tabular.mll"
    (if MyStack.empty stack_lexbuf then
      ()
    else
      let lexbuf = previous_lexbuf () in
      lexformat lexbuf)
# 392 "tabular.ml"

  | 6 ->
# 215 "tabular.mll"
     (tfone lexbuf ; lexformat lexbuf)
# 397 "tabular.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_lexformat_rec lexbuf __ocaml_lex_state

;;

# 219 "tabular.mll"
 
open Parse_opts

let main {arg=s ; subst=env} =
  if !verbose > 1 then prerr_endline ("Table format: "^s);
  let lexbuf =
    if String.length s > 0 && s.[0] = '\\' then
      match Latexmacros.find s with
      | _, Lexstate.Subst s -> MyLexing.from_list s
      | _,_ -> MyLexing.from_string s
    else
      MyLexing.from_string s in
  start_normal env ;
  lexformat lexbuf ;
  end_normal () ;
  let r = check_vert (trim out_table) in
  begin match !destination with
  | (Text | Info) -> check_length r
  | Html -> ()
  end ;
  if !verbose > 1 then begin
    prerr_string "Format parsed: " ;
    pretty_formats r ;
    prerr_endline ""
  end ;
  r

# 431 "tabular.ml"
