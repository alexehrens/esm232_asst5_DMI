
#'  Simple population growth
#' @param time time
#' @param P initial population
#' @param r intrinsic growth rate 
#' @return derivative of population with time 
#' @examples use with ode solver
#' ode(y=1,time=c(1;100),dexppop, parms=c(0.012))

kpop = function(time, P, parms) {
  kpop = parms$r*P*(1-P/parms$K)
  
  # set rate of change to 0 if P is greater than carrying capacity
  kpop = ifelse(P > parms$K, 0, kpop)
  return(list(kpop))
}