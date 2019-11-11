open Core
open Core_bench

let jk69_entry n i j =
  if i = j then -1. else 1. /. (float n -. 1.)

let lacaml_jk69 n = Lacaml.D.Mat.init_rows n n (jk69_entry n)
let owl_jk69 n = Owl.Mat.init_2d n n (jk69_entry n)


let mat_mat_mul n =
  let c = lacaml_jk69 n in
  let c' = owl_jk69 n in
  let lacaml =
    Bench.Test.create ~name:(sprintf "lacaml-mat-mat-mul-%d" n) (fun () -> Lacaml.D.gemm c c)
  and owl =
    Bench.Test.create ~name:(sprintf "owl-mat-mat-mul-%d" n) (fun () -> Owl.Mat.dot c' c') in
  Bench.make_command [lacaml ; owl]

let mat_vec_mul n =
  let c = lacaml_jk69 n in
  let c' = owl_jk69 n in
  let v = Lacaml.D.Vec.init n float in
  let v' = Owl.Mat.init_2d n 1 (fun i _ -> float i) in
  let lacaml = Bench.Test.create ~name:(sprintf "lacaml-mat-vec-mul-%d" n) (fun () -> Lacaml.D.gemv c v) in
  let owl = Bench.Test.create ~name:(sprintf "owl-mat-vec-mul-%d" n) (fun () -> Owl.Mat.dot c' v') in
  Bench.make_command [lacaml ; owl]

let vec_init n =
  let k = n / 2 in
  let f i = if i = k then 1. else 0. in
  let lacaml =
    Bench.Test.create ~name:(sprintf "lacaml-vec-init-%d" n) (fun () ->
        Lacaml.D.Vec.init n f
      )
  and owl =
    Bench.Test.create ~name:(sprintf "owl-vec-init-%d" n) (fun () ->
        Owl.Arr.init [|n|] f
      )
  in
  Bench.make_command [lacaml ; owl]

let command = Command.group ~summary:"Performance benches" [
    "mat-mat-mul-small", mat_mat_mul 4 ;
    "mat-mat-mul-large", mat_mat_mul 100 ;
    "mat-vec-mul-small", mat_vec_mul 4 ;
    "mat-vec-mul-large", mat_vec_mul 100 ;
    "vec-init-small", vec_init 4 ;
    "vec-init-large", vec_init 100 ;
  ]

let () = Command.run command
