fib <- function(n) {
  if (n==0 || n==1) {
    return(1)
  }
  if (n >=2) {
    return (fib(n-1) + fib(n-2))
  }
}