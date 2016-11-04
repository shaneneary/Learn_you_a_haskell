-- let space = ' '


hello_world = ['H','e','l','l','o']  ++ " World"

test_append = 'A' : 'p' : 'p' : 'e' : 'n' : 'd' : []

test_get_element = hello_world !! 3

boombang xs = [if x > 10 then "Boom!" else "Bang!" | x <-xs, odd x]

test_two_in xs ys = [x*y| x<-xs, y<-ys]

length' xs = sum [1 | _<-xs]