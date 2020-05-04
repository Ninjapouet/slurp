open Slurp
open Route

let scheme : Ezjsonm.value = `O ["foo", `String "float"; "bar", `String "string"]
let env : Ezjsonm.value = `O ["foo", `Float 42.; "bar", `String "pouet"]

let () = get
    ~id:"env"
    ~description:"Returns a json value"
    ~path:Path.(path "env" / "default" /? unit --> json scheme "env")
    (fun () -> env)
