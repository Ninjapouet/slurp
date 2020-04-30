open Slurp
open Route

let () = get
    ~id:"sum_lwt"
    ~path:Path.(path "sum_lwt" /: int "x" /: int "y" /? unit --> lwt (int "result"))
    (fun x y () -> wrap @@ Lwt.return (x + y))
