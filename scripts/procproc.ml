#!/usr/bin/env -S ocaml unix.cma

(*
  2022-12-18 T. Bourke

  This script is public domain. Feel free to reuse and adapt it with no
  constraints.
 *)

type name = {
  last : string;
  first : string;
}

type article = {
  title      : string;
  authors    : name list;
  doi        : string option;
  url        : string option;
  conference : string;
  session    : string;
}

type year = int

type conference = {
  acronym   : string;
  year      : year;
  title     : string;
  chair     : name;
  cochair   : name option;
  where     : string;
  published : string;
  articles  : article list;
}

let conferences = ref ([] : conference list)

let by_year { year = y1; _ } { year = y2; _ } = Int.compare y1 y2
let by_year_rev { year = y1; _ } { year = y2; _ } = Int.compare y2 y1

(* Poor man's removal of unicode accents *)
let unimap = [
  ("Å", 'a');
  ("Á", 'a');
  ("À", 'a');
  ("Â", 'a');
  ("Ä", 'a');
  ("Ă", 'a');
  ("É", 'e');
  ("È", 'e');
  ("Ê", 'e');
  ("Ë", 'e');
  ("Í", 'i');
  ("Ì", 'i');
  ("Î", 'i');
  ("Ï", 'i');
  ("ı", 'i');
  ("Ó", 'o');
  ("Ò", 'o');
  ("Ô", 'o');
  ("Ö", 'o');
  ("Ú", 'u');
  ("Ù", 'u');
  ("Û", 'u');
  ("Ü", 'u');
  ("Ç", 'c');
  ("Č", 'c');
  ("Ș", 's');
  ("Ț", 't');
  ("ẞ", 's');
  ]

(* Not available under OCaml 4.11.1 *)
let starts_with ~prefix s =
  let len_s = String.length s in
  let len_p = String.length prefix in
  len_p <= len_s && (prefix = String.sub s 0 len_p)

(* Not available under OCaml 4.11.1 *)
let ends_with ~suffix s =
  let len_s = String.length s in
  let len_p = String.length suffix in
  len_p <= len_s && (suffix = String.sub s (len_s - len_p) len_p)

let convert_first s =
  let rec f = function
    | [] -> Char.lowercase_ascii s.[0]
    | (m, c) :: mcs when starts_with ~prefix:m s -> c
    | _ :: mcs -> f mcs
  in
  f unimap

let to_output f filename =
  let fout = open_out filename in
  try f fout; close_out fout
  with e -> (close_out fout; raise e)

let conference_fileroot { acronym; year; _ } =
  Printf.sprintf "%s%d" (String.lowercase_ascii acronym) year

(* Reverse lookups on name *)

type name_info = Info of {
  articles : article list;
  pcs : year list;
  chair : year list;
  cochair : year list;
}

let empty_name_info = Info { articles = []; pcs = [];
                             chair = []; cochair = [] }

let name_hash = ((Hashtbl.create 1000) : (name, name_info) Hashtbl.t)

let name_map f name =
  match Hashtbl.find_opt name_hash name with
  | None -> Hashtbl.add name_hash name (f empty_name_info)
  | Some info -> Hashtbl.replace name_hash name (f info)

let add_article article =
  name_map (fun (Info ({ articles; _ } as info)) ->
              (Info { info with articles = article :: articles }))

let add_pc pc =
  name_map (fun (Info ({ pcs; _ } as info)) ->
              (Info { info with pcs = pc :: pcs }))

let add_chair year =
  name_map (fun (Info ({ chair; _ } as info)) ->
              (Info { info with chair = year :: chair }))

let add_cochair year =
  name_map (fun (Info ({ cochair; _ } as info)) ->
              (Info { info with cochair = year :: cochair }))

let name_compare { last = l1; first = f1 } { last = l2; first = f2 } =
  match String.compare l1 l2 with 0 -> String.compare f1 f2 | n -> n

let all_names () =
  List.sort name_compare
    (Hashtbl.fold (fun name _ names -> name :: names) name_hash [])

let get_name_info = Hashtbl.find name_hash

let first_letter_of_name { last; _ } = String.make 1 (convert_first last)

(* Printing *)

(* UTF-8 Byte Order Mark *)
let output_bom out =
  output_char out '\xEF';
  output_char out '\xBB';
  output_char out '\xBF'

let rec output_list output_item out xs =
  match xs with
  | []    -> ()
  | [x]   -> output_item out x; output_char out '\n'
  | x::xs ->
      output_item out x;
      output_string out " | ";
      output_list output_item out xs

let string_of_name { last; first } = last ^ ", " ^ first
let string_of_name' { last; first } = first ^ " " ^ last

let output_name out name = output_string out (string_of_name name)

let output_name_endline out name =
  output_name out name;
  output_char out '\n'

let rec output_names = output_list output_name

let output_heading out n s =
  for i = 1 to n do output_char out '#' done;
  output_char out ' ';
  output_string out s;
  output_char out '\n'

let output_year out y = output_string out (string_of_int y)

let output_field out n v =
  output_string out "* ";
  output_string out n;
  output_string out ": ";
  output_string out v;
  output_char out '\n'

let output_opt_field n out v =
  match v with None -> () | Some v -> output_field n out v

let check_article project out previous article =
  let current = project article in
  if String.equal previous current
  then previous
  else (output_heading out 2 current; output_char out '\n'; current)

let check_article_conference = check_article (fun { conference = c; _ } -> c)
let check_article_session = check_article (fun { session = s; _ } -> s)

let output_article check_current out current
    ({ title; authors; doi; url; _ } as article) =
  let current = check_current out current article in
  output_heading out 3 title;
  output_names out authors;
  output_opt_field out "DOI" doi;
  output_opt_field out "URL" url;
  output_char out '\n';
  current

let output_conference out
    { title; chair; cochair; where; published; articles; _} =
  output_heading out 1 title;
  output_name out chair;
  (match cochair with
   | None -> ()
   | Some cochair -> output_string out " | "; output_name out cochair);
  output_char out '\n';
  output_field out "At" where;
  output_field out "Published" published;
  output_char out '\n';
  ignore (List.fold_left (output_article check_article_session out) "" articles)

let output_pc_year was_chair was_cochair out year =
  output_year out year;
  if was_chair year then output_string out " (chair)"
  else if was_cochair year then output_string out " (cochair)"

let output_summary name (Info { articles; pcs; chair; cochair }) out =
  output_heading out 1 (string_of_name name);
  output_char out '\n';
  (if pcs <> [] then begin
    output_string out "* PCs: ";
    output_list
      (output_pc_year (fun y -> List.mem y chair)
                      (fun y -> List.mem y cochair)) out pcs;
    output_char out '\n'
  end);
  ignore (List.fold_left
            (output_article check_article_conference out) "" articles)

(* HTML generation *)

module Html = struct

  let fprintf = Printf.fprintf

  let linked_name_base to_string ~rel out ({ last; first } as name) =
    let path = (if rel = "" then Fun.id else Filename.concat rel) "participants"
    in
    fprintf out {|<a href="%s">%s</a>|}
      (Filename.concat path (string_of_name name ^ ".html"))
      (to_string name)

  let linked_name ~rel out name = linked_name_base string_of_name ~rel out name
  let linked_name' ~rel out name = linked_name_base string_of_name' ~rel out name

  let list_names ln out names =
    fprintf out "<ul>";
    List.iter (fprintf out "<li>%a</li>" ln) names;
    fprintf out "</ul>"

  (* TODO: handle markdown links ; both []() and <> *)
  let with_markdown_link out text = fprintf out "TODO: %s" text

  let template title make out =
    fprintf out {|
        <html>
          <head>
            <meta charset="UTF-8" />
            <title>%s</title>
          </head>
          <body>
          %t
          </body>
        </html>
     |} title make

  let cochair_opt ~rel out =
    Option.iter (fun cc ->
      output_string out " and ";
      linked_name' ~rel out cc)

  let index_entry out ({ title; where; published; chair; cochair; _ } as conf) =
    fprintf out {|
      <dt><a href="%s">%s</a></dt>
      <dd>%s; %a; chairs: %a%a<dd>
     |} (Filename.concat "confs" (conference_fileroot conf ^ ".html"))
        title
        where
        with_markdown_link published
        (linked_name' ~rel:"") chair
        (cochair_opt ~rel:"") cochair

  let index site_title =
    to_output (template site_title (fun out ->
      fprintf out "<h1>%s</h1><dl>%a</dl>"
        site_title
        (fun out -> List.iter (index_entry out))
        (List.(sort by_year_rev !conferences))
    ))

  let check_article project out previous article =
    let current = project article in
    if String.equal previous current
    then previous
    else (fprintf out "<h2>%s</h2>" current; current)

  let check_article_conference = check_article (fun { conference = c; _ } -> c)
  let check_article_session = check_article (fun { session = s; _ } -> s)

  let output_opt_li out = function
    | None -> ()
    | Some v -> fprintf out {|<li>%a</li>|} with_markdown_link v

  let output_article check_current out current
      ({ title; authors; doi; url; _ } as article) =
    let current = check_current out current article in
    fprintf out {|
        <div class="article">
          <h3>%s</h3>
          <div class="authors">%a</div>
          <div class="links"><ul>%a%a</ul></div>
        </div>
      |} title
         (list_names (linked_name' ~rel:"..")) authors
         output_opt_li doi
         output_opt_li url;
    current

  let article_list out articles = ignore
    (List.fold_left (output_article check_article_session out) "" articles)

  let conf site_title path
           ({ title; chair; cochair; where; published; articles; _ } as conf) =
    let opt_cochair out =
      Option.iter
        (fprintf out "<dt>cochair</dt><dd>%a</dd>" (linked_name' ~rel:".."))
    in
    to_output (template (site_title ^ ": " ^ title) (fun out ->
      fprintf out {|
          <h1>%s</h1>
          <div class="conf-info">
            <dl>
              <dt>chair</dt><dd>%a</dd>%a
              <dt>where</dt><dd>%s</dd>
              <dt>published</dt><dd>%s</dd>
            </dl>
          </div>
          <div class="conf-articles">%a</div>
        |} title
           (linked_name' ~rel:"..") chair
           opt_cochair cochair
           where published article_list articles
    )) (Filename.concat path (conference_fileroot conf ^ ".html"))

  let participant_lists out groups =
    let show_group (c, group) =
      fprintf out {|<h2 id="section-%s">%s</h2>|} c c;
      list_names (linked_name ~rel:"") out group
    in
    List.iter show_group groups

  let index_links out groups =
    let show_group (c, _) =
      fprintf out {|<li><a href="#section-%s">%s</a></li>|} c c
    in
    List.iter show_group groups

  let group_by_first_letter names =
    let f groups name =
      let cn = String.uppercase_ascii (first_letter_of_name name) in
      match groups with
      | [] -> [(cn, [name])]
      | (cg, group) :: groups' when cn = cg -> (cg, name :: group) :: groups'
      | groups' -> (cn, [name]) :: groups'
    in
    List.fold_left f [] names
    |> List.rev_map (fun (cg, group) -> (cg, List.rev group))

  let participant_index site_title names =
    to_output (template (site_title ^ ": participant index") (fun out ->
      let groups = group_by_first_letter names in
      fprintf out {|
        <h1>%s: participant index</h1>
        <div class="links"><ul>%a</ul><div>
        <div class="participants">%a</div>
       |} site_title index_links groups participant_lists groups
    ))

  let participant site_title path name =
    let first_last = string_of_name' name in
    let Info { articles; _ } = get_name_info name in
    to_output (template (site_title ^ ": " ^ first_last) (fun out ->
      fprintf out {|
          <h1>%s</h1>
        |} first_last;
      ignore (List.fold_left (output_article check_article_conference out)
                             "" articles)
    )) (Filename.concat path (string_of_name name ^ ".html"))
    (* TODO: show PCs, cochairs, chairs *)

  let make site_title path =
    index site_title Filename.(concat path "index.html");
    (* conferences *)
    let confs_path = Filename.(concat path "confs") in
    Sys.mkdir confs_path 0o777;
    List.(iter (conf site_title confs_path) !conferences);
    (* participants *)
    let names = all_names () in
    participant_index site_title names Filename.(concat path "participants.html");
    let participants_path = Filename.(concat path "participants") in
    Sys.mkdir participants_path 0o777;
    List.iter (participant site_title participants_path) names

end

(* Parsing *)

let filename = ref "stdin"
let line = ref 0

let reset_filename s =
  filename := s;
  line := 0

exception Parse_error

type line =
  | Heading of int * string
  | Field of string * string
  | Item of string
  | Text of string

let print_line = function
  | Heading (n, s) -> print_endline ("heading " ^ Int.to_string n ^ ": " ^ s)
  | Field (n, v)   -> print_endline ("field: " ^ n ^ "=" ^ v)
  | Item s         -> print_endline ("item: " ^ s)
  | Text s         -> print_endline ("text: " ^ s)

let error fmt =
  let pr s = prerr_endline s; raise Parse_error in
  Format.(kasprintf (kasprintf pr "%s, line %d: %s" !filename !line) fmt)

let read_line fin =
  let s = input_line fin in
  incr line;
  s

let rec read_next_line fin =
  let s = String.trim (read_line fin) in
  if s = "" then read_next_line fin
  else s

let count_leading_chars c s =
  let rec f i = if s.[i] = c then f (i + 1) else i in
  f 0

let split_on_first_char c s =
  match String.index_from_opt s 0 c with
  | None -> error "no %c in '%s'" c (String.trim s)
  | Some i -> String.(sub s 0 i, sub s (i + 1) (length s - i - 1))

let parse_line fin =
  let s = read_next_line fin in
  if String.length s = 0 then Text s
  else if s.[0] = '#'
  then let h = count_leading_chars '#' s in
        Heading (h, String.(trim (sub s h (length s - h))))
  else if s.[0] = '*'
  then begin
    if String.contains s ':' then
      let s1, s2 = split_on_first_char ':' s in
      Field (String.(trim (sub s1 1 (length s1 - 1))), String.trim s2)
    else Item (String.(trim (sub s 1 (length s - 1))))
  end
  else Text s

let rec make_seq fin =
  let rec f () =
    match parse_line fin with
    | Text "" -> f ()
    | v -> Seq.Cons (v, f)
    | exception End_of_file -> Seq.Nil
  in f

let parse_name s =
  let last, first = split_on_first_char ',' s in
  String.{ last = trim last; first = trim first }

let parse_names s = List.map parse_name String.(split_on_char '|' s)

let drop_prefix prefix s=
  if starts_with ~prefix s
  then let l = String.length prefix in
       String.(sub s l (length s - l))
  else error "expected prefix '" ^ prefix ^ "': " ^ s

let parse_conf_acronym_year s =
  let conf, _ = split_on_first_char ':' s in
  let acronym, year = split_on_first_char ' ' conf in
  String.trim acronym, int_of_string year

let expect_names seq =
  match seq () with
  | Seq.Cons (Text s, seq') -> (parse_names s, seq')
  | _ -> error "expected a list of names (separated by '|'s"

let try_heading n seq =
  match seq () with
  | Seq.Cons (Heading (m, s), seq') when n = m -> Some s, seq'
  | v -> None, (fun () -> v)

let expect_heading n seq =
  match try_heading n seq with
  | None, _ -> error "expected heading level %d (leading '%s')" n (String.make n '#')
  | Some s, seq' -> s, seq'

let try_item seq =
  match seq () with
  | Seq.Cons (Item s, seq') -> Some s, seq'
  | v -> None, (fun () -> v)

let try_field n seq =
  match seq () with
  | Seq.Cons (Field (n', v), seq') when n = n' -> Some v, seq'
  | v -> None, (fun () -> v)

let expect_field n seq =
  match try_field n seq with
  | None, _ -> error "expected field '%s'" n
  | Some v, seq' -> v, seq'

let expect_end seq =
  match seq () with
  | Seq.Nil -> ()
  | _ -> error "expected end of file"

let try_session session seq =
  match try_heading 2 seq with
  | None, seq -> session, seq
  | Some session, seq -> session, seq

let try_article (conference, session) seq =
  let session, seq = try_session session seq in
  match try_heading 3 seq with
  | None, seq -> None, seq
  | Some title, seq ->
      let authors, seq = expect_names seq in
      let doi, seq = try_field "DOI" seq in
      let url, seq = try_field "URL" seq in
      let article = { title; authors; doi; url; conference; session } in
      List.iter (add_article article) authors;
      Some (article, (conference, session)), seq

let rec try_to_list tryf =
  let rec f acc state seq =
    match tryf state seq with
    | None, seq' -> List.rev acc, seq'
    | Some (x, state), seq' -> f (x::acc) state seq'
  in
  f []

let rec try_to_list' tryf =
  let rec f acc seq =
    match tryf seq with
    | None, seq' -> List.rev acc, seq'
    | Some x, seq' -> f (x::acc) seq'
  in
  f []

let expect_articles data seq =
  try_to_list try_article data seq

let expect_chairs seq =
  match expect_names seq with
  | [ chair; cochair ], seq' -> chair, Some cochair, seq'
  | [ chair ], seq' -> chair, None, seq'
  | _, _ -> error "expected one or two names (chair and cochair)"

let read_conference fin =
  let seq = make_seq fin in
  let title, seq = expect_heading 1 seq in
  let acronym, year = parse_conf_acronym_year title in
  let chair, cochair, seq = expect_chairs seq in
  add_chair year chair;
  Option.iter (add_cochair year) cochair;
  let where, seq = expect_field "At" seq in
  let published, seq = expect_field "Published" seq in
  let session, seq = expect_heading 2 seq in
  let articles, seq = expect_articles (title, session) seq in
  expect_end seq;
  { title; acronym; year; chair; cochair; where; published; articles }

let read_pc fin =
  let seq = make_seq fin in
  let title, seq = expect_heading 1 seq in
  let _, year = parse_conf_acronym_year title in
  let names, seq = try_to_list' try_item seq in
  List.(iter (add_pc year) (map parse_name names));
  expect_end seq

(* Algorithms *)

let load_file filename =
  let fin = open_in filename in
  reset_filename filename;
  if ends_with ~suffix:"-pc.md" filename
  then read_pc fin
  else conferences := read_conference fin :: !conferences;
  close_in fin

let output_conferences outc =
  List.(iter (output_conference outc) (rev !conferences))

let output_conferences_to_file filename =
  let fout = open_out filename in
  output_conferences fout;
  close_out fout

let print_authors outc =
  List.(iter (fun { articles; _ } ->
                iter (fun { authors; _ } ->
                  iter (output_name_endline outc) authors) articles)
        (rev !conferences))

let make_name_summaries path =
  let make_path name =
    let path = Filename.concat path (first_letter_of_name name) in
    (try Unix.mkdir path 0o777 with Unix.(Unix_error (EEXIST, _, _)) -> ());
    Filename.concat path (string_of_name name ^ ".md")
  in
  let summarize name =
    let Info ({ articles; pcs; _ } as info) = get_name_info name in
    to_output (output_summary name
                  (Info { info with articles = List.rev articles;
                                    pcs = List.rev pcs }))
      (make_path name)
  in
  List.iter summarize (all_names ())

let output_author_pcs confs out { last; first } (Info { pcs; _ }) =
  if pcs <> [] then begin
    output_string out last;
    output_char out ',';
    output_string out first;
    List.iter (fun { year = y; _ } ->
                output_string out (if List.mem y pcs then ",1" else ",0"))
      confs;
    output_char out '\n'
  end

let output_author_confs confs out { last; first } (Info { articles; _ }) =
  output_string out last;
  output_char out ',';
  output_string out first;
  let at { title; _ } { conference; _ } = String.equal title conference in
  List.iter (fun conf ->
      output_string out (if List.exists (at conf) articles then ",1" else ",0"))
    confs;
  output_char out '\n'

let output_conf_headings out =
  List.iter (fun { year; _ } -> output_char out ','; output_year out year)

let make_pc_csv out =
  output_bom out;
  let confs = List.(sort by_year !conferences) in
  output_string out "last,name";
  output_conf_headings out confs;
  output_char out '\n';
  Hashtbl.iter (output_author_pcs confs out) name_hash

let make_author_csv out =
  output_bom out;
  let confs = List.(sort by_year !conferences) in
  output_string out "last,name";
  output_conf_headings out confs;
  output_char out '\n';
  Hashtbl.iter (output_author_confs confs out) name_hash

let _ = Arg.parse [
    ("--print", Arg.Unit (fun () -> output_conferences stdout),
     "print conferences to stdout");
    ("--output", Arg.String (to_output output_conferences),
     "write conferences to file");
    ("--print-authors", Arg.Unit (fun () -> print_authors stdout),
     "print authors to stdout");
    ("--summarize-by-name", Arg.String make_name_summaries,
     "create summary pages indexed by last name");
    ("--pc-csv", Arg.String (to_output make_pc_csv),
     "write pc summary to csv file");
    ("--author-csv", Arg.String (to_output make_author_csv),
     "write author summary to csv file");
    ("--html", Arg.String (Html.make "testing"), (* TODO *)
     "write html files to the given path");
  ]
  load_file
  "procproc: process article files"

