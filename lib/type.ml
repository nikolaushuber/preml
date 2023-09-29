type t = 
  | TUnit 
  | TBool 
  | TInt 
  | TFloat 
  | TUnknown 

open Format 

let fmt ppf = function 
  | TUnit -> pp_print_string ppf "unit" 
  | TBool -> pp_print_string ppf "bool" 
  | TInt -> pp_print_string ppf "int" 
  | TFloat -> pp_print_string ppf "float" 
  | TUnknown -> pp_print_string ppf "?" 

let to_string = asprintf "%a" fmt 
