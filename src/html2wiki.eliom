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
        | "B" -> let inner = html2wiki node in
                 add_str inner ~surr:"**"
        | "I" -> let inner = html2wiki node in
                 add_str inner ~surr:"//"
        | "#text" -> (match Js.Opt.to_option  node##nodeValue with
                         | Some x -> Buffer.add_string ans (Js.to_string x)
                         | None -> ())
        | "P" -> let inner = html2wiki node in
                 add_str (inner ^ "\n\n")
        | "BR"  -> Buffer.add_string ans "\\\\"
        | "HR"  -> Buffer.add_string ans "----"
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
        | ("H1" | "H2" | "H3") as hh ->
          let n = int_of_char hh.[1] - (int_of_char '0') + 1 in
          let prefix = String.make n '=' in
          let inner = html2wiki node in
          Buffer.add_string ans (prefix^inner^"\n\n")
        | _ as name -> 
          Buffer.add_string ans ("^"^ name^"^")
    )
  done;
  Buffer.contents ans

}}
