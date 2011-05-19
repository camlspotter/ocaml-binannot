module F(A : (* A => *) sig val x : int end (* <= A *)) = struct
  let y = A.x (* ? A *)
end
