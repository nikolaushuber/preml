# PreML 

PreML is a simple language inspired by the ML family of functional programming languages (SML, OCaml, F#). It's main design choices are the following: 

- Ease of compilation to C 
- Possibility to translate to SMTLIB formula 

Therefore, many features are missing in PreML: 

- PreML only has the basic datatypes `int`, `bool`, `float`, and `unit` 
- Functions are not first class, i.e. functions are different from values 

## Installation 

Installation is easiest through opam: 

```bash
opam pin preml https://github.com/nikolaushuber/preml.git -y 
```

You can also clone this repository and build it with dune. This is specifically interesting if you want to change the source of PreML. 

```bash
git clone https://github.com/nikolaushuber/preml.git 
cd preml 
dune build 
dune install 
```

## Syntax 

Every PreML program consists of a list of toplevel definitions. A toplevel definition is either a function, a (collection of) recursive functions(s), or a named expression. 

```
val pi : float = 3.1415 
fun area (r : float) -> float = pi *. r *. r 

rec even (x : int) -> bool = 
    if x == 0 then true else odd (x-1) 
and odd (x : int) -> bool = 
    if x == 0 then false else even (x-1) 
```

## Evaluating expressions 

You can evaluate expressions from the command line by calling 

``` 
preml eval "4 + 5" 
``` 

You can load a file with function definitions beforehand. If you put the above mentioned example code in a file even_odd.prml you can then call: 

``` 
preml eval "odd 15" --load even_odd.prml 
``` 

which should return `true`. 

## License 

MIT
