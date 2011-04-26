{shared{
  open Eliom_pervasives
  open HTML5.M
  let width = 700
  let height = 400
}}

{client{
(*  Dom_html.window##alert (Js.string "asdf") *)
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

{client{
  let draw ctx (color, size, (x1, y1), (x2, y2)) =
    ctx##strokeStyle <- (Js.string color);
    ctx##lineWidth <- float size;
    ctx##beginPath();
    ctx##moveTo(float x1, float y1);
    ctx##lineTo(float x2, float y2);
    ctx##stroke()
}}

let wiki_edit_page =
  let open Eliom_parameters in
  (* TODO: understand how to create service with 2 parameters *)
  My_appl.register_service ~path:["edit"] ~get_params:(string "p")
    (fun s () -> Lwt.return [])

let main_service =
  My_appl.register_service ~path:[""] ~get_params:Eliom_parameters.unit
    (fun () () ->
(*       Eliom_services.onload
         {{
           let canvas = Dom_html.createCanvas Dom_html.document in
           let ctx = canvas##getContext (Dom_html._2d_) in
           canvas##width <- width; canvas##height <- height;
           ctx##lineCap <- Js.string "round";

           Dom.appendChild Dom_html.document##body canvas;

           draw ctx ("#ffaa33", 12, (10, 10), (200, 100))
         }}; 
*)
      Lwt.return [
	div ~a:[a_id "navbar"] [
	  div ~a:[a_id "akmenu"] [p [
	    span ~a:[a_class ["nwikilogo"]] [pcdata "MiniWiki"];	    
(*            a ~service:wiki_view_page
              ~a:[a_accesskey 'h'; a_class ["ak"]]
              [pcdata "Home"] "WikiStart"; *)
	    a   [pcdata "Edit page"];
(*            a ~service:(wiki_edit_page)
	      ~a:[(*a_accesskey 'e'; a_class ["ak"]*)]
              [pcdata "Edit page"] ; *)
	    br ()]]];
	div ~a:[a_id "content"] [];
	
	h1 [pcdata "Graffiti"]])


