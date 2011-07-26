module type ASig = sig (* B.f => *) val f : int -> int (* <= B.f *) end
module A = struct let f x = x + 1 end
let x = (module A : ASig)
let y = 
  let module B = (val x : ASig) in
  B.f (* ? B.f *) 1
