(** Classify isn't needed for native, it's here to support the universal
    approach of the library. In the end, it classify Yojson.Basic.t to
    Yojson.Basic.t *)

let classify : Yojson.Basic.t -> Yojson.Basic.t = fun x -> x
let declassify : Yojson.Basic.t -> Yojson.Basic.t = fun x -> x
