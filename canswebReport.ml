open Eliom_content.Html5.D
module TS = Canswer.ToString
module TL = Canswer.ToLatex
module TD = Canswer.ToDot
module C = Canswer.Core
module F = Canswer.Cqafo
module A = Canswer.AttackGraph
module R = Canswer.Rewrite
module CF = Canswer.Cqafo

open Common

let commas ?(sep=", ") s = String.concat sep s.C.Lset.l
let pair a b = [ dt [pcdata a]; dd [pcdata b] ]

let report_query qc =
  dl [
    dt [pcdata "Canswer form"];
    dd [pcdata (TS.conjunctive qc)];
    dt [pcdata "DRC"];
    dd [tolatex (TL.drc (R.Drc.conjunctive qc))];
    dt [pcdata "TRC"];
    dd [tolatex (TL.trc (R.Trc.conjunctive qc))];
    dt [pcdata "SQL"];
    dd [pre [pcdata (R.Sql.conjunctive qc)]];
    dt [pcdata "Relations used"];
    dd [pcdata (commas (Lset.map (fun a ->
        TS.relation_name a.C.Atom.relation) qc.C.Conjunctive.atoms))];
  ]

let report_props qc =
  dl [
    dt [pcdata "(Non-free) variables"];
    dd [pcdata (commas (C.Conjunctive.vars qc))];
    dt [pcdata "Free variables"];
    dd [pcdata (commas (C.Symbol.vars qc.C.Conjunctive.free))];
    dt [pcdata "Self-join-free"];
    dd [pcdata (string_of_bool (C.Conjunctive.self_join_free qc))];
  ]

let report_atom qc ag node =
  let atom = node.A.AttackGraph.atom in
  li [
    pcdata (TS.atom atom);
    dl [
      dt [pcdata "key variables"];
      dd [pcdata (commas (C.Conjunctive.atomkey qc atom))];
      dt [pcdata "key closure"];
      dd [pcdata (commas node.A.AttackGraph.keycl)];
      dt [pcdata "attacked variables"];
      dd [pcdata (commas node.A.AttackGraph.attvars)];
      dt [pcdata "attacked atoms"];
      dd [pcdata (commas (Lset.map TS.atom node.A.AttackGraph.attatoms))];
      dt [pcdata "unattacked"];
      dd (
        if Lset.mem atom (A.AttackGraph.unattacked ag) then
          [
            pcdata "yes ";
            a ~service:report_service [pcdata "(subquery)"]
              (TS.conjunctive (C.Conjunctive.(qc -- atom)));
          ]
        else [pcdata "no"]
      );
    ]
  ]

let report_atoms qc ag =
  ul (List.map (report_atom qc ag) ag.Lset.l)

let report_fodefinitions qc =
  ul [
    li [
      pcdata "DRC";
      dl [
        dt [pcdata "Split + Group"];
        dd [tolatex (TL.drc (R.Drc.rewrite qc))];
        dt [pcdata "Group"];
        dd [tolatex (TL.drc (R.Drc.rewrite ~split:false qc))];
        dt [pcdata "Split"];
        dd [tolatex (TL.drc (R.Drc.rewrite ~group:false qc))];
        dt [pcdata "Naive"];
        dd [tolatex (TL.drc (R.Drc.rewrite ~group:false ~split:false qc))];
      ];
    ];
    li [
      pcdata "TRC";
      dl [
        dt [pcdata "Split + Group"];
        dd [tolatex (TL.trc (R.Trc.rewrite qc))];
        dt [pcdata "Group"];
        dd [tolatex (TL.trc (R.Trc.rewrite ~split:false qc))];
        dt [pcdata "Split"];
        dd [tolatex (TL.trc (R.Trc.rewrite ~group:false qc))];
        dt [pcdata "Naive"];
        dd [tolatex (TL.trc (R.Trc.rewrite ~group:false ~split:false qc))];
      ];
    ];
    li [
      pcdata "SQL";
      dl [
        dt [pcdata "Split + Group"];
        dd [pre [pcdata (R.Sql.rewrite qc)]];
        dt [pcdata "Group"];
        dd [pre [pcdata (R.Sql.rewrite ~split:false qc)]];
        dt [pcdata "Split"];
        dd [pre [pcdata (R.Sql.rewrite ~group:false qc)]];
        dt [pcdata "Naive"];
        dd [pre [pcdata (R.Sql.rewrite ~group:false ~split:false qc)]];
      ];
    ];
  ]

let anchor id text =
  Raw.a ~a:[a_href (uri_of_string (fun _ -> id))] [pcdata text]

let rec gen ?(acc=[]) f start = function
  | i when i < start -> acc
  | i -> gen ~acc:(f i :: acc) f start (i - 1)

let genvars prefix start stop =
  String.concat " " (gen (Printf.sprintf "%s%i" prefix) start stop)

let qclass1 (title : string) (start : int) (stop : int) (f : int -> string) =
  div [
    pcdata title;
    ul (gen (fun i ->
      li [a ~service:report_service [pcdata (string_of_int i)] (f i)])
      start stop)
  ]

let menu qraw =
  div ~a:[a_class ["menu"]] [
    h1 [pcdata "Canswer"];
    a ~service:form_service [pcdata "Change query"] (Some qraw);
    h2 [pcdata "This query"];
    ul [
      li [anchor "#query" "Query"];
      li [anchor "#props" "Basic Properties"];
      li [anchor "#atoms" "Atoms"];
      li [anchor "#agraph" "Attack Graph"];
      li [anchor "#fodef" "FO Definitions"];
      li [anchor "#canon" "Canonical Databases"];
      li [anchor "#lattice" "Lattice"];
      li [anchor "#dbs" "Databases"];
    ];
    h2 [pcdata "Query classes"];
    ul [
      li [qclass1 "Path" 0 10 (fun i ->
        Printf.sprintf "R@%i(x@0 | x@1)" i)];
      li [qclass1 "Cycle" 0 10 (fun i ->
        Printf.sprintf "R@%i(x@0 | x@1) R%i(x%i | x1)" i (i + 1) (i + 1))];
      li [qclass1 "Focal" 0 10 (fun i ->
        Printf.sprintf "R@%i(a | x@0) S(%s |)" i (genvars "x" 1 i))];
      li [qclass1 "Wheel" 0 10 (fun i ->
        Printf.sprintf "R@%i(x@0|) S(| %s)" i (genvars "x" 1 i))];
      li [qclass1 "EvenCycles" 0 5 (fun i ->
        Printf.sprintf "R@%i(x | z@0) S@%i(y | z@0)" i i)];
    ];
    h2 [pcdata "References"];
    p [pcdata "Jef Wijsen: On the first-order expressibility of computing \
    certain answers to conjunctive queries over uncertain databases. PODS \
    2010."];
    p [pcdata "Alexandre Decan, Fabian Pijcke, Jef Wijsen: Certain Conjunctive \
    Query Answering in SQL. SUM 2012."];
    p [pcdata "Paraschos Koutris, Jef Wijsen: A Trichotomy in the Data \
    Complexity of Certain Query Answering for Conjunctive Queries."];
    p [pcdata "Floris Geerts, Fabian Pijcke, Jef Wijsen: First-Order \
    Under-Approximations of Consistent Query Answers. SUM 2015."];
  ]

let dodot file dot =
  let dotf = "static/" ^ file ^ ".dot" in
  let svgf = "static/" ^ file ^ ".svg" in
  let tikzf = "static/" ^ file ^ ".tikz" in
  let c = open_out dotf in
  output_string c dot;
  close_out c;
  ignore (Sys.command ("dot -Tsvg \"" ^ dotf ^ "\" > \"" ^ svgf ^ "\""));
  ignore (Sys.command ("dot2tex \"" ^ dotf ^ "\" > \"" ^ tikzf ^ "\""))

let main qraw () =
  try
  let qc =
    if String.trim qraw = "" || String.trim qraw = "|" then Conjunctive.empty
    else C.FromString.conjunctive qraw
  in
  let qs = TS.conjunctive qc in
  let ag = A.AttackGraph.make qc in
  let dot = TD.split_attack_graph qc in
  let lat = CF.Expanded.make qc in
  let clat = CF.Collapsed.collapse lat in
  let ntlat = CF.Collapsed.remove_transitive_edges clat in
  let dotlat = TD.lattice lat in
  let dotclat = TD.lattice (clat :> Expanded.t) in
  let dotntlat = TD.lattice ntlat in
  dodot qs dot;
  dodot ("lat_" ^ qs) dotlat;
  dodot ("clat_" ^ qs) dotclat;
  dodot ("ntlat_" ^ qs) dotntlat;
  wrap [
    menu qraw;
    div ~a:[a_class ["content"]] [
      h1 [pcdata (TS.conjunctive qc)];
      h2 ~a:[a_id "query"] [pcdata "Query"];
      report_query qc;
      h2 ~a:[a_id "props"] [pcdata "Basic Properties"];
      report_props qc;
      h2 ~a:[a_id "atoms"] [pcdata "Atoms"];
      report_atoms qc ag;
      h2 ~a:[a_id "agraph"] [pcdata "Attack Graph"];
      img ~alt:"Attack Graph"
      ~src:(make_uri ~service:(Eliom_service.static_dir ()) [qs ^ ".svg"]) ();
      p [Raw.a ~a:[a_href (
        make_uri ~service:(Eliom_service.static_dir ())
        [qs ^ ".dot"])] [pcdata "Download dot"]];
      p [Raw.a ~a:[a_href (
        make_uri ~service:(Eliom_service.static_dir ())
        [qs ^ ".tikz"])] [pcdata "Download tikz"]];
      h2 ~a:[a_id "fodef"] [pcdata "FO definition"];
      report_fodefinitions qc;
      h2 ~a:[a_id "canon"] [pcdata "Canonical Databases"];
      h2 ~a:[a_id "lattice"] [pcdata "Lattice"];
      img ~alt:"Lattice"
      ~src:(make_uri ~service:(Eliom_service.static_dir ()) ["lat_" ^ qs ^ ".svg"]) ();
      p [Raw.a ~a:[a_href (
        make_uri ~service:(Eliom_service.static_dir ())
        ["lat_" ^ qs ^ ".dot"])] [pcdata "Download dot"]];
      p [Raw.a ~a:[a_href (
        make_uri ~service:(Eliom_service.static_dir ())
        ["lat" ^ qs ^ ".tikz"])] [pcdata "Download tikz"]];
      img ~alt:"Compact Lattice"
      ~src:(make_uri ~service:(Eliom_service.static_dir ()) ["clat_" ^ qs ^ ".svg"]) ();
      p [Raw.a ~a:[a_href (
        make_uri ~service:(Eliom_service.static_dir ())
        ["clat_" ^ qs ^ ".dot"])] [pcdata "Download dot"]];
      p [Raw.a ~a:[a_href (
        make_uri ~service:(Eliom_service.static_dir ())
        ["clat" ^ qs ^ ".tikz"])] [pcdata "Download tikz"]];
       img ~alt:"Compact Lattice without redundant edges"
      ~src:(make_uri ~service:(Eliom_service.static_dir ()) ["ntlat_" ^ qs ^ ".svg"]) ();
      p [Raw.a ~a:[a_href (
        make_uri ~service:(Eliom_service.static_dir ())
        ["ntlat_" ^ qs ^ ".dot"])] [pcdata "Download dot"]];
      p [Raw.a ~a:[a_href (
        make_uri ~service:(Eliom_service.static_dir ())
        ["ntlat" ^ qs ^ ".tikz"])] [pcdata "Download tikz"]];
      h2 ~a:[a_id "dbs"] [pcdata "Interesting Databases"];
    ]
  ]
  with e -> wrap [
    h1 [pcdata "Error"];
    p [pcdata (Printexc.to_string e)];
    pre [pcdata (Printexc.get_backtrace ())];
  ]

