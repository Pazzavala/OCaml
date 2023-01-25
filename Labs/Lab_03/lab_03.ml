let is_square (n: int) : bool =
  if n = 0
    then true
  else
    let rec increment i =
      if i > n
        then false
      else if i * i = n
        then true
      else increment (i + 1)
    in increment 0

let square_all (lst: int list) : int list =
  

let all_squares (lst: int list) : int list =


let product_of_squares (lst: int list) : int =


(* let _ = assert(is_square 0 = true);;
      assert(is_square 4 = true);;
      assert(is_square 15 = false);;
      assert(square_all [1; 2; 3; 4] = [1; 4; 9; 16]);;
      assert(square_all [1; 3; 5; 7; 9] = [1; 9; 25; 49; 81]);;
      assert(square_all [0; 10; 20; 30; 40] = [0; 100; 400; 900; 1600]);;
      assert(all_squares [1; 2; 3; 4] = [1; 4]);;
      assert(all_squares [0; 3; 9; 25] = [0; 9; 25]);;
      assert(all_squares [10; 20; 30; 40] = []);;
      assert(product_of_squares [1; 2; 3; 4] = 576);;
      assert(product_of_squares [0; 3; 9; 25] = 0);;
      assert(product_of_squares [5; 10; 15; 20] = 225000000) *)