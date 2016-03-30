fibby = function(x){
  listy = c()
  listy[1] = 1
  listy[2] = 1
  for(i in 3:x){
    listy[i] = listy[i-1] + listy[i-2]
  }
  return(listy[x])
}

fibby0 = function(x){
  if(x<3){return(1)}
  else{return(fibby0(x-1)+fibby0(x-2))}
}

fibby1 = function(x){
  fib1 = 1
  fib2 = 1
  current = 2
  for(i in 3:x){
    current = fib1 + fib2
    fib1 = fib2
    fib2 = current
  }
  return(current)
}
