{shared{
  open Eliom_pervasives
  open HTML5.M
  let width = 700
  let height = 400
}}

{client{
(*  Dom_html.window##alert (Js.string "asdf")  *)
}}
(*
{server{ 
let wiki_view_page = service [] (suffix (string "p")) ()
let wiki_edit_page = service ["edit"] (string "p") ()
let wiki_start = Redirection.register_service [] unit
    (fun _ _ ->
       Lwt.return (Eliom_services.preapply wiki_view_page "WikiStart"))

}}
*)
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
  Eliom_services.service [""] (suffix (string "p")) ()
(*
let wiki_start = Eliom_output.Redirection.register_service [] Eliom_parameters.unit
    (fun _ _ ->
       Lwt.return (Eliom_services.preapply wiki_view_page "WikiStart"))
*)
(*
let wiki_start = 
  let open Eliom_output in
  Redirection.register_service [] Eliom_parameters.unit
  (fun _ _ ->        
    Lwt.return (Eliom_services.preapply wiki_view_page "WikiStart"))
*)

let wiki_edit_page = Eliom_services.service ["edit"] (Eliom_parameters.string "p") ()

let menu_html content = 
  [div ~a:[a_id "navbar"] [
    div ~a:[a_id "akmenu"] [p [
      span ~a:[a_class ["nwikilogo"]] [pcdata "MiniWiki"];	    
(*            a ~service:wiki_view_page
              ~a:[a_accesskey 'h'; a_class ["ak"]]
              [pcdata "Home"] "WikiStart";
	    a   [pcdata "Edit page"]; *)
      Eliom_output.Html5.a ~a:[a_accesskey 'h'; a_class ["ak"]] 
	[pcdata "Home"]  ~service:wiki_view_page "WikiStart"; 
      Eliom_output.Html5.a ~a:[a_accesskey 'e'; a_class ["ak"]] 
	[pcdata "Edit page"]  ~service:wiki_edit_page "edit"; 
      br ()]]];
  div ~a:[a_id "content"] content]

let () = My_appl.register wiki_edit_page 
  (fun s () -> Lwt.return 
      (menu_html [h1 [pcdata "edit_page"]])
  )

let () = My_appl.register wiki_view_page 
  (fun s () -> Lwt.return 
      (menu_html [h1 [pcdata "view_page"]])
  )

(*
let wiki_edit_page =
  let open Eliom_parameters in
  (* TODO: understand how to create service with 2 parameters *)
  My_appl.register_service ~path:["edit"] ~get_params:(string "p")
    (fun s () -> Lwt.return [
      menu_html [h1 [pcdata "edit_page"]]
    ])
*)

let main_service =
  My_appl.register_service ~path:[""] ~get_params:Eliom_parameters.unit
    (fun () () ->
      let a  = Eliom_output.Html5.a in
      Lwt.return (menu_html [h1 [pcdata "main_service"]]))


