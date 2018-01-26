open Eliom_content.Html5.D

let form_service =
  Eliom_service.Http.service
  ~path:[""]
  ~get_params:Eliom_parameter.(suffix (opt (string "query"))) ()

let report_service =
  Eliom_service.Http.service
  ~path:["report"]
  ~get_params:Eliom_parameter.(suffix (string "query")) ()

let css s =
  css_link ~uri:(make_uri (Eliom_service.static_dir ()) ["css"; s ^ ".css"]) ()

let wrap content =
  Lwt.return (
    html (
      head (
        title (pcdata "Canswer")
      ) [
        css "cansweb";
      ]
    ) (
      body content
    )
  )

let latexcounter = ref 0
let tolatex s =
  let i = !latexcounter in
  incr latexcounter;
  let texfile = "static/" ^ string_of_int i ^ ".tex" in
  let pdffile = string_of_int i ^ ".pdf" in
  let pngfile = "static/" ^ string_of_int i ^ ".png" in
  ignore (Sys.command ("cat texhead.tex > " ^ texfile));
  let c = open_out_gen [Open_append; Open_text] 0o644 texfile in
  output_string c s;
  close_out c;
  ignore (Sys.command ("cat texfoot.tex >> " ^ texfile));
  ignore (Sys.command ("pdflatex -halt-on-error " ^ texfile));
  ignore (Sys.command ("convert -density 150 " ^ pdffile ^ " -quality 100 " ^ pngfile));
  ignore (Sys.command ("rm " ^ pdffile));
  Raw.a ~a:[a_href (
    make_uri ~service:(Eliom_service.static_dir ())
    [texfile])] [
      img
      ~alt:s
      ~src:(make_uri ~service:(Eliom_service.static_dir ()) [string_of_int i ^ ".png"]) ()]

