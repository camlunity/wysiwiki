{shared{
open Lwt
let ( >> ) x f = f x
}}
open Lwt_chan
open Eliom_parameters

open HTML5.M
open Eliom_output.Html5
open Eliom_output
open Eliom_services
open Eliom_parameters

module My_appl =
  Eliom_output.Eliom_appl (
    struct
      let application_name = "miniwiki"
      let params = 
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

let menu_html ~edit:page content = 
  [div ~a:[a_id "navbar"] [
    div ~a:[a_id "akmenu"] [p [
      span ~a:[a_class ["nwikilogo"]] [pcdata "MiniWiki"];	    
      Eliom_output.Html5.a ~a:[a_accesskey 'h'; a_class ["ak"]] ~absolute:true
	[pcdata "Home"]  ~service:wiki_view_page "WikiStart"; 
      Eliom_output.Html5.a ~a:[a_accesskey 'e'; a_class ["ak"]] ~absolute:true 
	[pcdata "Edit page"]  ~service:wiki_edit_page page; 
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

let html_stub body_html = 
  return     
    (html
       (head (title (pcdata ""))
	  [link ~rel:[`Stylesheet] ~href:"style.css" ()]
       )
       (body body_html))

let service_save_page_post = Eliom_services.post_service
    ~fallback:wiki_view_page
    ~post_params:(string "value") 
    ()

{client{
  (* TODO: understanf Gregoire's letter *)
  let find_element name = 
    let e1 = Dom_html.document##getElementById (Js.string name) in
    let e2 = Js.Opt.get e1 (fun () -> assert false) in
    (Js.Unsafe.coerce e2)

  let find_iframe name : Dom_html.iFrameElement Js.t =  find_element name

  let replace_child p n =
    Js.Opt.iter (p##firstChild) (fun c -> 
      Firebug.console##log (c);
      Dom.removeChild p c
    );
    Dom.appendChild p n
}}

let view_content page () =
  (if wiki_page_exists page then 
      (load_wiki_page page) >>= fun s -> return (String.concat "\n" s)
   else
      return "") 
  >>= (fun s ->
    let d = div ~a:[a_id "qqq"] [] in
    Eliom_services.onload {{
      let d = find_element "qqq" in
      let rendered = Wiki_syntax.xml_of_wiki %s Dom_html.document in
      replace_child d rendered	
    }};

    Lwt.return 
      (menu_html ~edit:page [d])
  )  

let () = My_appl.register wiki_view_page view_content

let () = My_appl.register service_save_page_post
    (fun page value ->
      (save_wiki_page page value) >> view_content page ()
    )

open Eliom_services

let () = My_appl.register wiki_edit_page 
  (fun page () -> 
    let open Eliom_output.Html5 in
    (if wiki_page_exists page then
	load_wiki_page page >>= fun s -> return (String.concat "\n" s)
     else 
	return "")
    >>= fun wikitext ->
    let frame = iframe ~a:[a_id "main_iframe" (*; a_seamless `Seamless *)] [] in
    Eliom_services.onload {{
      ignore ((Lwt_js.sleep 0.1) >>= (fun _ -> 
	
	let fr = find_iframe "main_iframe" in
	fr##src <- Js.string "#";
	let doc = Js.Opt.get (fr##contentDocument) (fun _ -> assert false) in
	doc##open_ ();
	doc##write (Js.string "<html><body><div id=\"qqq\"/></body></html>");
	doc##close ();
	doc##designMode <- Js.string "On";
	let rendered = Wiki_syntax.xml_of_wiki %wikitext doc in
	let qqq = doc##getElementById (Js.string "qqq") in
	Js.Opt.case qqq (fun () ->  () )
	  (fun x -> replace_child x rendered);

	let preview: Dom_html.textAreaElement Js.t = find_element "preview_area" in
	Html2wiki.button_adder fr (find_element "buttons_block");

(*	Dom_html.window##onload <- Dom_html.handler (fun _ ->
	  let rec dyn_preview old_text n =
	    let text = Js.to_string doc##body##innerHTML in
	    let n =
              if text <> old_text then begin
		begin try
			preview##value <- Js.string text;
		  with _ -> () end;
		20
              end else
		max 0 (n - 1)
	    in
	    Lwt_js.sleep (if n = 0 then 0.5 else 0.1) >>= fun () ->
	    dyn_preview text n
	  in
	  ignore (dyn_preview "" 0);
	  Js._false
	); *)

	return ()		   
      ) )  
    }};
	
    Lwt.return (menu_html ~edit:page [
      div ~a:[] [
	frame; br ();
	button ~a:[a_id "submit_button";
		   a_onclick {{
		     let fr = find_iframe "main_iframe" in
		     let doc = Js.Opt.get (fr##contentDocument) (fun _ -> assert false) in
		     let ans = Html2wiki.html2wiki (doc##body :> Dom.node Js.t) in
		     Eliom_client.change_page %service_save_page_post %page ans 
		       (* TODO: carring in line above wil not be reported as warning *)		       
		   }}
		  ] ~button_type:`Button [i ~a:[] [pcdata "Save"] ];
	br ();
	div ~a:[a_id "buttons_block"] []
(*
	;
	br ();
	label ~a:[] [pcdata "tags below are used for debugging only"];
	br ();
	button ~a:[a_id "asdfasdfasdf"; a_onclick {{
	  let fr:  Dom_html.iFrameElement Js.t = find_element "main_iframe" in
	  let doc = Js.Opt.get (fr##contentDocument) (fun () -> assert false) in
	  let area: Dom_html.textAreaElement Js.t = find_element "preview_area" in
	  area##value <- doc##body##innerHTML
	}}] ~button_type:`Button [i ~a:[] [pcdata "generate"] ];
	br ();
	HTML5.M.textarea ~a:[a_id "preview_area"; a_cols 50; a_rows 20] (pcdata "")
*)
      ]
    ])
  )
