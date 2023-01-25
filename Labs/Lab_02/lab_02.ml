let circle_circum_v1 radius = 2.0 *. 3.1415 *. radius

let circle_circum_v2 radius = let pi = 3.1415 
                               in 2.0 *. pi *. radius

let rec power expo base  = 
  if expo = 0 then 1.0
  else base *. power (expo - 1) base

let cube = power 3
