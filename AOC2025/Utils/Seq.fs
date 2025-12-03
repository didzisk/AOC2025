module Seq

let maxi (s:'T seq) =
    s
    |> Seq.mapi (fun i v -> i, v)
    |> Seq.maxBy snd