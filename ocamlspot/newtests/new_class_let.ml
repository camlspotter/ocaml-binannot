class c = 
  let (* x => *) x (* <= x *) = 1 in
  object 
    method f () = x (* ? x *)
  end
