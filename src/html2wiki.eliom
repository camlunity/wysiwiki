(*
 * HTML to wiki-syntax translator
 * Dmitry Kosarev 2011
 *)
{client{

module Html = Dom_html

let (>>=) = Lwt.bind
let ( |> ) x f = f x
exception Break of bool

let is_visible_text s = 
  (* TODO: rewrite with regexps. *)
  let len = String.length s in
  let rec loop i =
    if i >= len then () else 
    if (s.[i] = '\\') && (i<len-1) && (s.[i+1]='\\') then loop (i+2)
    else match  s.[i] with
        | '\n' -> loop (i+1)
        | _   -> raise (Break true)       
  in 
  try 
    loop 0;
    false
  with Break b -> b

open Dom

let rec html2wiki ?inH:(inH=false) body = 
  let ans = Buffer.create 10 in
  let add_str ?surr:(surr="") s = 
    if is_visible_text s then Buffer.add_string ans (surr^s^surr)
    else () in
  let childNodes = body##childNodes in
  for i=0 to childNodes##length-1 do
    Js.Optdef.iter (childNodes##item (i)) (fun node ->
      match Js.to_string node##nodeName with
        | "B" | "STRONG" -> let inner = html2wiki node in
			    add_str inner ~surr:"**"
        | "I" | "EM" -> let inner = html2wiki node in
			add_str inner ~surr:"//"
        | "#text" -> (match Js.Opt.to_option  node##nodeValue with
                         | Some x -> Buffer.add_string ans (Js.to_string x)
                         | None -> ())
        | "P" -> let inner = html2wiki node in
                 add_str (inner ^ "\n\n")
        | "BR"  -> Buffer.add_string ans "\\\\"
        | "HR"  -> Buffer.add_string ans "----\n"
        | "DIV" -> let inner = html2wiki node in
                   Buffer.add_string ans inner               
        | "A"   ->
          let x  : element Js.t Js.opt = Js.some (Js.Unsafe.coerce node) in
          let el = Js.Opt.get x (fun _ -> assert false)  in

          Js.Opt.case (el##getAttribute (Js.string "wysitype"))
            (fun () -> Buffer.add_string ans "^error_in_anchor^")
            (fun s -> 
              let url = Js.Opt.get (el##getAttribute (Js.string "href")) (fun _ -> assert false) 
                |> Js.to_string in
              match Js.to_string s with
              | "global" -> 
                let desc = html2wiki node in
                Buffer.add_string ans (String.concat "" ["[[";url;"|";desc;"]]"]) 
              | "wiki"   -> String.concat "" ["[[";url;"]]"] |> Buffer.add_string ans 
              | _        -> Buffer.add_string ans "^error2_in_anchor^"
            )
        | ("H1" | "H2" | "H3" | "H4" | "H5" | "H6") as hh ->
          let n = int_of_char hh.[1] - (int_of_char '0') in
          let prefix = String.make n '=' in
          let inner = html2wiki node in
          Buffer.add_string ans (prefix^inner^"\n\n")
        | _ as name -> 
          Buffer.add_string ans ("^"^ name^"^")
    )
  done;
  Buffer.contents ans


(*888888888888888888888888888888888888888888888888888888888**)                                 
let button_adder iframe root = 
  let iWin  = iframe##contentWindow in
  let iDoc = Js.Opt.get (iframe##contentDocument) (fun () -> assert false) in
  Dom.appendChild root (Html.createBr Dom_html.document);

    (* see http://www.quirksmode.org/dom/execCommand.html 
     * http://www.mozilla.org/editor/midas-spec.html
     *)
  let createButton ?show:(show=Js._false) ?value:(value=None) title action = 
    let but = Html.createInput ?_type:(Some (Js.string "submit")) Dom_html.document in
    but##value <- Js.string title;
    let wrap s = match s with
      | None -> Js.null | Some s -> Js.some (Js.string s) in
    
    but##onclick <- Html.handler (fun _ -> 
      iWin##focus ();
      iDoc##execCommand (Js.string action, show, wrap value); 
      Js._true);
    Dom.appendChild root but;
    but
    in

    ignore (createButton "hr" "inserthorizontalrule");
    ignore (createButton "remove format" "removeformat");
    ignore (createButton "B" "bold");
    ignore (createButton "I" "italic");
    Dom.appendChild root (Html.createBr  Dom_html.document);
    ignore (createButton "p" "formatblock" ~value:(Some "p"));
    ignore (createButton "h1" "formatblock" ~value:(Some "h1"));
    ignore (createButton "h2" "formatblock" ~value:(Some "h2"));
    ignore (createButton "h3" "formatblock" ~value:(Some "h3"));

    (createButton "link" "inserthtml")##onclick <- Html.handler (fun _ ->
      let link = iWin##prompt (Js.string "Enter a link", Js.string "http://google.ru") 
                 |>  Js.to_string in
      let desc = iWin##prompt (Js.string "Enter description", Js.string "desc") 
                 |>  Js.to_string in
      let link = String.concat "" ["<a href=\""; link; "\" wysitype=\"global\">"; desc; "</a>"] in
      iWin##alert (Js.string link); 
      iDoc##execCommand (Js.string "inserthtml", Js._false, Js.some (Js.string link) );
      Js._true
     );
    (createButton "link2wiki" "inserthtml")##onclick <- Html.handler (fun _ ->
      let link = iWin##prompt (Js.string "Enter a wikipage", Js.string "lololo") 
                 |>  Js.to_string in
      let link = ["<a href=\""; link; "\" wysitype=\"wiki\">"; link; "</a>"] 
                 |> String.concat "" in
      iWin##alert (Js.string link); 
      iDoc##execCommand (Js.string "inserthtml", Js._false, Js.some (Js.string link) );
      Js._true
     );
    Dom.appendChild root (Html.createBr  Dom_html.document);

}}
