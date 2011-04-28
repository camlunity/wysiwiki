open Lwt
open Lwt_chan
open Eliom_parameters

open HTML5.M
open Eliom_output.Html5
open Eliom_output
open Eliom_services
open Eliom_parameters
open Eliom_state

{client{
(*  Dom_html.window##alert (Js.string "asdf")  *)
}}

module My_appl =
  Eliom_output.Eliom_appl (
    struct
      let application_name = "miniwiki"
      let params = (*Eliom_output.default_appl_params *)
	let open Eliom_output in
	{ ap_title = "miniwiki";
	  ap_container = None;
	  ap_body_attributes = None;
	  ap_headers_before = [link ~rel:[`Stylesheet] ~href:"style.css" () ];
	  ap_headers_after = []
	}
    end)

let wiki_view_page = 
  let open Eliom_parameters in
  Eliom_services.service [] (suffix (string "p")) ()

let wiki_start = 
  Redirection.register_service [] Eliom_parameters.unit
  (fun _ _ ->        
    Lwt.return (Eliom_services.preapply wiki_view_page "WikiStart"))    

let wiki_edit_page = Eliom_services.service ["edit"] (Eliom_parameters.string "p") ()

let menu_html content = 
  [div ~a:[a_id "navbar"] [
    div ~a:[a_id "akmenu"] [p [
      span ~a:[a_class ["nwikilogo"]] [pcdata "MiniWiki"];	    
      Eliom_output.Html5.a ~a:[a_accesskey 'h'; a_class ["ak"]] ~absolute:true
	[pcdata "Home"]  ~service:wiki_view_page "WikiStart"; 
      Eliom_output.Html5.a ~a:[a_accesskey 'e'; a_class ["ak"]] ~absolute:true 
	[pcdata "Edit page"]  ~service:wiki_edit_page "edit"; 
      br ()]]];
  div ~a:[a_id "content"] content]

let wiki_page_menu_html page  = menu_html
  

let finally_ handler f x =
  catch
    (fun () -> f x)
    (fun e -> handler() >>= fun () -> fail e)
  >>= fun r ->
  handler () >>= fun () ->
  return r

let fold_read_lines f accum inchnl =
  let line () =
    catch
      (fun () -> Lwt_chan.input_line inchnl >>= fun line -> return (Some line))
      (function End_of_file -> return None | e -> fail e)
  in
  let rec loop accum =
    line () >>= fun l ->
    match l with
    | Some e -> loop (f accum e)
    | None -> return accum
  in
  loop accum


let with_open_out fname f =
  Lwt_chan.open_out fname >>= fun oc ->
  finally_
    (fun () -> Lwt_chan.flush oc >>= (fun () -> Lwt_chan.close_out oc))
    f oc

let with_open_in fname f =
  Lwt_chan.open_in fname >>= fun ic ->
  finally_
    (fun () -> Lwt_chan.close_in ic)
    f ic

let wiki_file_dir =
  let rec find_wikidata = function
      [Simplexmlparser.Element ("wikidata", [("dir", s)],_)] -> s
    | _ -> raise (Ocsigen_extensions.Error_in_config_file ("Unexpected content inside Miniwiki config"))
  in
  let c = Eliom_config.get_config () in
  find_wikidata c

let wiki_page_filename page =
  wiki_file_dir ^ "/" ^ page ^ ".wiki"

let wiki_page_exists page =
  Sys.file_exists (wiki_page_filename page)

let save_wiki_page page text =
  with_open_out
    (wiki_page_filename page)
    (fun chnl -> output_string chnl text)

let load_wiki_page page =
  with_open_in
    (wiki_page_filename page)
    (fun chnl ->
      fold_read_lines (fun acc line -> line::acc) [] chnl >>= fun l ->
      return (List.rev l))

let view_page page = 
  return (html 
     (head (title (pcdata "")) [])
     ( body [h1 [pcdata "view_page_content"]] )
  )

let take_while pred lines =
  let rec loop acc = function
      (x::xs) as lst ->
        if pred x then
          loop (x::acc) xs
        else
          (lst, List.rev acc)
    | [] ->
        ([], List.rev acc) in
  loop [] lines


let comp_re = Pcre.regexp ~flags:[`ANCHORED]

let accepted_chars_ = "a-zA-Z\128-\2550-9_!\"§°#%&/\\(\\)=\\?\\+\\.,;:{}'@\\$\\^\\*`´<>"
let accepted_chars_sans_ws = "["^accepted_chars_^"-]+"
let accepted_chars = "["^accepted_chars_^" -]+"

let text_re = comp_re ("("^accepted_chars_sans_ws^")")
let wikilink_re = comp_re "([A-Z][a-z]+([A-Z][a-z]+)+)"

let wikilinkanum_re =
  comp_re
    ("(\\[(wiki|file|http):("^accepted_chars_sans_ws^")[ ]+("^accepted_chars^")\\])")

let wikilinkanum_no_text_re =
  comp_re ("(\\[(wiki|file|http):("^accepted_chars_sans_ws^")\\])")


let h1_re = Pcre.regexp "^=(.*)=([ \n\r]*)?$"
let h2_re = Pcre.regexp "^==(.*)==([ \n\r]*)?$"
let h3_re = Pcre.regexp "^===(.*)===([ \n\r]*)?$"
let list_re = Pcre.regexp "^[ ]?([*]+) (.*)([ \n\r]*)?$"

let match_pcre_option rex s =
  try Some (Pcre.extract ~rex s) with Not_found -> None

let is_list s =
  match_pcre_option list_re s

let open_pre_re = Pcre.regexp "^(<pre>|{{{)[ \n\r]+$"
let close_pre_re = Pcre.regexp "^(</pre>|}}})[ \n\r]+$"


let translate_list items  =
  [ul ~a:[] [] ]
(*  let add_ul t lst = 
    lst in
(*  t @ [ul (List.hd lst) (List.tl lst)] in *)

  let rec loop = function
      ((nesting1,text1)::(nesting2,text2)::xs) as lst ->
        if nesting1 = nesting2 then
          (li text1)::loop (List.tl lst)
        else if nesting1 < nesting2 then (* enter *)
          let (next_same_level,same_or_higher) =
            take_while (fun (n,_) -> n >= nesting2) (List.tl lst) in
          (li (add_ul text1 (loop same_or_higher)))::loop next_same_level
        else (* leave *)
          loop (List.tl lst)
    | (nesting,text)::[] ->
        [(li text)]
    | [] -> [] in
  let list_items = loop items in
  ul (List.hd list_items) (List.tl list_items)
*)
let parse_lines lines = 

  let wikilink scheme page text  =
    if scheme = "wiki" || scheme = "" then
      let t = if text = "" then page else text in
      if wiki_page_exists page then
        a  ~service:wiki_view_page [pcdata t] page
      else
        a ~a:[a_class ["missing_page"]] ~service:wiki_view_page [pcdata t]
          page
    else (* External link *)
      let url = scheme^":"^page in
      let t = if text = "" then url else text in
      HTML5.M.a ~a:[a_href (Uri.uri_of_string url)] [pcdata t]
  in

  let rec pcre_first_match str pos =
    let rec loop = function
        (rex,f)::xs ->
          (try Some (Pcre.extract ~rex ~pos str, f) with Not_found -> loop xs)
      | [] -> None in
    loop in

  (* Parse a line of text *)
  let rec parse_text acc s =

    let len = String.length s in
    let add_html html_acc html =
      html::html_acc in

    let parse_wikilink acc r charpos =
      (add_html acc (wikilink "" r.(1) r.(1)), charpos+(String.length r.(0))) in

    let parse_wikilinkanum acc r charpos =
      let scheme = r.(2) in
      let page = r.(3) in
      let text = r.(4) in
      let fm_len = String.length r.(0) in
      (add_html acc (wikilink scheme page text), charpos+fm_len) in

    let parse_wikilinkanum_no_text acc r charpos =
      let scheme = r.(2) in
      let page = r.(3) in
      let text = "" in
      let fm_len = String.length r.(0) in
      (add_html acc (wikilink scheme page text), charpos+fm_len) in

    let parse_text acc r charpos =
      (add_html acc (pcdata r.(1)), charpos+(String.length r.(0))) in

    let text_patterns =
      [(wikilink_re, parse_wikilink);
       (wikilinkanum_re, parse_wikilinkanum);
       (wikilinkanum_no_text_re, parse_wikilinkanum_no_text);
       (text_re, parse_text)] in

    let rec loop acc charpos =
      if charpos >= len then
        acc
      else
        if s.[charpos] = '\t' then
          let m = "\t" in
          loop (add_html acc (pcdata m)) (charpos+1)
        else if s.[charpos] = ' ' then
          let m = " " in
          loop (add_html acc (pcdata m)) (charpos+1)
        else if s.[charpos] = '\r' || s.[charpos] = '\n' then
          acc
        else
          begin
            match pcre_first_match s charpos text_patterns with
              Some (r,f) ->
                let (acc',charpos') = f acc r charpos in
                loop acc' charpos'
            | None ->
                let s = (String.sub s charpos ((String.length s)-charpos)) in
                add_html acc
                  (span
                     [span ~a:[a_class ["error"]]
                        [pcdata "WIKI SYNTAX ERROR IN INPUT: "];
                      pcdata s])
          end
    in
    List.rev (loop acc 0) in

  (* Line-by-line wiki parser *)
  let rec loop acc = function
      (x::xs) as lst ->
        let parse_list r =
          (* Grab all lines starting with '*': *)
          let (after_bullets,bullets) =
            take_while (fun e -> is_list e <> None) lst in
          let list_items =
            List.map
              (fun e ->
                 match is_list e with
                   Some r ->
                     let n_stars = String.length r.(1) in
                     (n_stars, parse_text [] r.(2))
                 | None -> assert false) bullets in
          loop ((translate_list list_items)::acc) after_bullets in

        let parse_verbatim r =
          (* Handle <pre>..</pre>, {{{..}}} *)
          let (after_pre,contents) =
            take_while
              (fun x -> match_pcre_option close_pre_re x = None)
              lst in
          let p  =
            (pre [pcdata (String.concat "\n" (List.tl contents))]) in
          loop (acc) (List.tl after_pre) in

(*        let wiki_pats =
          [(h3_re, (fun r -> loop ((ul ~a:[] [])::acc) xs));
           (h2_re, (fun r -> loop ((ul ~a:[] [pcdata r.(1)])::acc) xs));
           (h1_re, (fun r -> loop ((ul ~a:[] [pcdata r.(1)])::acc) xs));
           (list_re, parse_list);
           (open_pre_re, parse_verbatim)] in
        begin
          match pcre_first_match x 0 wiki_pats with
            Some (res, action) -> action res
            | None -> *)
              loop (acc) xs
  (*      end *)
    | [] -> List.rev acc in

  return (loop [] lines) 

let wikiml_to_html page =
  if wiki_page_exists page then
    load_wiki_page page >>= parse_lines
  else
    return []

let html_stub body_html = 
  return     
    (html
       (head (title (pcdata ""))
	  [link ~rel:[`Stylesheet] ~href:"style.css" ()]
       )
       (body body_html))

(* Save page as a result of /edit?p=Page *)
let service_save_page_post =
  Eliom_output.Html5.register_post_service
    ~fallback:wiki_view_page
    ~post_params:(string "value")
    (fun (page:string) (value:string) ->
      print_endline ("save value: "^value); 
      html_stub []
    )

let wiki_page_contents_html page ?(content=[]) () =
  print_endline " wiki_page_contents_html call";
  print_endline ("page = " ^ page);
  wikiml_to_html page >>= fun p ->
  return (wiki_page_menu_html page ([div ~a:[] []]))

let () = My_appl.register wiki_edit_page 
  (fun page () -> 
    let open Eliom_output.Html5 in
    (if wiki_page_exists page then
	load_wiki_page page >>= fun s -> return (String.concat "\n" s)
     else 
	return "")
    >>= fun wikitext ->
    let f =
      print_endline "f";
      Eliom_output.Html5.post_form service_save_page_post
        (fun chain ->
	  print_endline "post_form";
          [(p [string_input ~input_type:`Submit ~value:"Save" (); br ();
               textarea ~name:chain ~rows:30 ~cols:80
                 ~value:wikitext ()])
	  ])
        page
    in    
    (wiki_page_contents_html page ~content:[f] () >>= fun c ->
     (return [div c] )
    )
  )

let () = My_appl.register wiki_view_page 
  (fun page () -> 
    (if wiki_page_exists page then (
      let open Lwt_io in
      Lwt_stream.to_list (lines_of_file (wiki_page_filename page)) 
     ) else
      return [""]) >>=
    (fun (s:string list) ->
      let inner = List.map (fun s -> h1 [pcdata s]) s in
      Lwt.return 
	(menu_html 
	   ((h1 [pcdata "view_page"]) :: inner
	    
	   ))
    )
  )

