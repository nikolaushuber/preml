type t = 
  | TUnit 
  | TBool 
  | TInt 
  | TFloat 
  | TChar 
  | TVar of t option ref 
