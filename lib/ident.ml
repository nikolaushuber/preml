type t = {
  package : string option; 
  name : string; 
}

let mk ?(package) name = {package; name} 

open Format 

let fmt ppf {package; name} = match package with 
  | None -> pp_print_string ppf name 
  | Some p -> fprintf ppf "%s.%s" p name 


