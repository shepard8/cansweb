open Eliom_content.Html5.D
module TS = Canswer.ToString
module C = Canswer.Core
module A = Canswer.AttackGraph
module R = Canswer.Rewrite

open Common

let example_link q = a ~service:report_service [pcdata q] q

let main query () =
  let query = match query with None -> "" | Some q -> q in
  wrap [
    get_form ~service:report_service (fun q -> [
      textarea ~name:q ~value:query ~a:[a_rows 10; a_cols 80] ();
      string_input ~input_type:`Submit ~value:"Analyze" ();
    ]);
    p [pcdata "Enter a query. Some examples are listed below."];
    dl [
      dt [example_link "R(a x | y z) S(z | v)"];
      dd [pcdata
        "Queries are formed of atoms, which are formed of a relation name \
        (alphanumeric starting with a capital letter), a key part and a \
        non-key part, separated by a vertical bar (pipe). These parts are \
        composed of symbols, a symbol must be alphanumeric and start with a \
        lowercase letter. If the first character is a, b, c, d, e, f, g or \
        h, then this symbol is considered a constant. Otherwise it is \
        considered a variable."];
      dt [example_link "a v | R(a x | y z) S(z | v)"];
      dd [pcdata
        "Queries can also have a free part, composed of symbols"];
      dt [example_link "R(A a B x | C y D z) S(D z | v)"];
      dd [pcdata
        "Atoms' attributes can be named, note that it is possible to use \
        both named and unnamed perspective in the same query."];
      dt [example_link "R@5(x@0 | x@1)"];
      dd [pcdata
        "This examples illustrates the query schema functionality. This \
        query is composed of 5 atoms R1(x1|x2), ..., R5(x5|x6)."];
      dt [example_link "R(a | x) S(a | y) T(x y |) U(| a)"];
      dd [pcdata
        "Note the four different rewritings of this query when using \
        grouping, splitting, none or both."];
      dt [example_link "R@5(x@0|x@1)"];
      dd [pcdata "The path query class"];
      dt [example_link "R@5(x@0|x@1)R6(x6|x1)"];
      dd [pcdata "The cycle query class"];
      dt [example_link "R@5(a|x@0)S(x1 x2 x3 x4 x5|)"];
      dd [pcdata "The focal query class"];
      dt [example_link "R@5(x@0|)S(|x1 x2 x3 x4 x5)"];
      dd [pcdata "The wheel query class"];
      dt [example_link "R@3(x|z@0)S@3(y|z@0)"];
      dd [pcdata "A query class in which only cycles of even size occur. \
      Note that every attack is strong."];
      dt [example_link "R(a|x)S(x z|y)T(y|z)"];
      dd [pcdata "This example shows that the engine will rewrite as much \
      as possible of a query (according to the grouping/splitting strategy) \
      before giving up when no unattacked atom remains."];
    ];
  ]
