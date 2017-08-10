distance_m = function(x, y, d = 2){
  return ((sum((abs(x-y))^d))^(1/d))
}

a = c(1,2,3,10)
b = c(2,3,4,5)

distance_m(a,b)
distance_m(a, b, d = 3)