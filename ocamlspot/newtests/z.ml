module M = struct
  let x = 1
  let x = 2
  let x = 3
  let x = 4
end

module N = struct
  include M
  let y = x 
end
