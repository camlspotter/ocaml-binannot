type (* t => *) t = { foo : int ; bar : float } (* <= t *)
    
let x = { foo (* ? t *) = 1; bar = 4.2 }
let _ = x.foo (* ? t *)
