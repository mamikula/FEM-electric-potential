n = 10
dl = 3
#pomocnicze
integrate <- function(f, a, b){
  return(
    (b-a)/2*(
      f((b-a)/2*(1/sqrt(3)) + (b+a)/2)
      + f((b-a)/2*(-1/sqrt(3)) + (b+a)/2)
    )
  )
}

draw_plot <- function(){
  u = main()
  plot(seq(0, 3, 1/(100*n)), 
       mapply(u, seq(0, 3, 1/(100*n))),
       main = 'Potencjal elektryczny',
       xlab=' ',
       ylab=' ',
       type='l')
}

#glowne funkcje
B <- function(i, j){
  
  start = max(0, xi(i-1), xi(j-1))
  end = min(xi(i+1), xi(j+1))
  return(
    e(0, i)*e(0, j) - integrate(uv_prim(i,j), start, end)
  )
}


L <- function(i){
  
  left = 5 * e(0, i)
  start = max(0, xi(i-1))
  end = min(xi(i+1), 3)
  
  
  er <- function(x){
    if(x <= 1 && x >= 0) return(10)
    else if(x <= 2 && x > 1) return(5)
    else if(x <= 3 && x > 2) return(1)
  }

  
  f <- function(x){
    return (e(x,i)/er(x))
  }
  
  integ = integrate(f, start, end);
  return (left - integ - 2*B(n, i));
}


xi <- function(i){
  return(dl*i/n)
}

e <- function(x, i){
  if (x > xi(i-1) && x <= xi(i)){ return (n/dl*x - i + 1) }
  else if (x > xi(i) && x < xi(i+1)){ return (-n/dl*x + i + 1) }
  else { return (0) }
}

e_der <- function(x, i){
  if (x > xi(i-1) && x <= xi(i)){ return (n/dl) }
  else if (x > xi(i) && x < xi(i+1)){ return (-n/dl) }
  else { return (0) }
}


uv_prim <- function(i, j){
  return (function(x){
    return(e_der(x, i)*e_der(x, j))
  }
  )
}

#funckja rozwiazujaca rownanie
main <- function(){
  
  M = matrix(0.0, nrow=n, ncol=n)
  
  for (i in 1:n){
    for (j in 1:n){
      if(abs(i - j) <= 1)
      {
        M[i,j] = B(j-1, i-1)
      }
    }
  }


  C = vector()
  for (i in 1:n){
    C = c(C, L(i-1))
  }
  
  print(M)
  print(C)
  
  vs = solve(M, C) 
  
  return_sol<- function(x, v = vs){
    result = 2 * e(x, n)
    for (i in 1:n){
      result = result + v[i]*e(x, i-1)
    }
    return(result)
  }
  
  
  return (return_sol)
}

draw_plot()

