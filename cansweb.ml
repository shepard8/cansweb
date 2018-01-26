open Eliom_content.Html5.D
module TS = Canswer.ToString
module C = Canswer.Core
module A = Canswer.AttackGraph
module R = Canswer.Rewrite

open Common

let () = Eliom_registration.Html5.register
    ~service:form_service
    CanswebForm.main

let () = Eliom_registration.Html5.register
    ~service:report_service
    CanswebReport.main

