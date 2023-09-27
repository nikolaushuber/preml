type t = 
  | TUnit 
  | TBool 
  | TInt 
  | TFloat 
  | TVar of t option ref 

open Format 

let rec fmt ppf = function 
  | TUnit -> pp_print_string ppf "unit" 
  | TBool -> pp_print_string ppf "bool" 
  | TInt -> pp_print_string ppf "int" 
  | TFloat -> pp_print_string ppf "float" 
  | TVar { contents = Some t } -> fmt ppf t 
  | TVar { contents = None } -> pp_print_string ppf "?" 

let to_string = asprintf "%a" fmt 
