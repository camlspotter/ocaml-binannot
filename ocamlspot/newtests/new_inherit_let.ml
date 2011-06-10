class c = object end

class nc = object
  inherit let _x = 1 in c (* ? c *)
end
