open Base

type pgm = exp

and exp =
  | Int of int
  | Var of var
  | Add of exp * exp
  | Sub of exp * exp
  | IsZero of exp
  | If of exp * exp * exp
  | Let of var * exp * exp
  | Proc of var * exp
  | AppProc of exp * exp
  | Read

and value = VInt of int | VBool of bool | VProc of var * exp * env
and env = var -> (value, string) Result.t
and var = string

module Env = struct
  type t = env

  let empty _ = Error "Empty Env"
  let extend x v env = fun y -> if Poly.(x <> y) then env y else Ok v
  let lookup x e = e x
end

let rec eval : Env.t -> exp -> (value, string) Result.t =
 fun rho exp ->
  let open Result in
  match exp with
  | Int n -> Ok (VInt n)
  | Var v -> Env.lookup v rho
  | Add (e1, e2) -> bin_op ~f:( + ) rho e1 e2
  | Sub (e1, e2) -> bin_op ~f:( - ) rho e1 e2
  | IsZero e -> (
      eval rho e >>= function
      | VInt 0 -> Ok (VBool true)
      | VInt _ -> Ok (VBool false)
      | _ -> Error "TypeError")
  | If (e, te, ee) -> (
      eval rho e >>= function
      | VBool true -> eval rho te
      | VBool false -> eval rho ee
      | _ -> Error "TypeError")
  | Let (x, e1, e2) ->
      eval rho e1 >>= (fun v -> Ok (Env.extend x v rho)) >>= Fn.flip eval e2
  | Proc (v, e) -> Ok (VProc (v, e, rho))
  | AppProc (e1, e2) ->
      let func =
        eval rho e1 >>= function
        | VProc (v, e, rho') -> Ok (v, e, rho')
        | _ -> Error "TypeError"
      in
      let v = eval rho e2 in
      combine func v
        ~ok:(fun (p, e, rho') a -> eval (Env.extend p a rho') e)
        ~err:Fn.const
      |> join
  | Read ->
      let open Option in
      let open Stdio.In_channel in
      input_line stdin >>= Int.of_string_opt
      |> value_map ~f:(fun x -> Ok (VInt x)) ~default:(Error "TypeError")

and bin_op :
    f:(int -> int -> int) -> Env.t -> exp -> exp -> (value, string) Result.t =
 fun ~f rho e1 e2 ->
  let open Result in
  let ensure_vint = function VInt n -> Ok n | _ -> Error "TypeError" in
  let n1 = eval rho e1 >>= ensure_vint in
  let n2 = eval rho e2 >>= ensure_vint in
  combine n1 n2 ~ok:(fun n1 n2 -> VInt (f n1 n2)) ~err:Fn.const

let string_of_value = function
  | VInt n -> Int.to_string n
  | VBool b -> Bool.to_string b
  | VProc (var, _, _) -> Printf.sprintf "<proc(%s)>" var

let () =
  let open Stdio.Out_channel in
  let sample : pgm =
    Let
      ( "x",
        Int 1,
        Let
          ( "f",
            Proc ("y", Add (Var "x", Var "y")),
            Let
              ( "x",
                Int 2,
                Let
                  ( "g",
                    Proc ("y", Add (Var "x", Var "y")),
                    Add (AppProc (Var "f", Int 1), AppProc (Var "g", Int 1)) )
              ) ) )
  in
  match eval Env.empty sample with
  | Ok x -> string_of_value x |> print_endline
  | Error e -> print_endline e
