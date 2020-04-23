open Slurp
open Route

let () = get
    ~id:"sum"
    ~path:Path.(path "sum" /: int "x" /: int "y" /? unit --> int "result")
    (fun x y () -> x + y)
