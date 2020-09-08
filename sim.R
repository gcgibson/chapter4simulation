## deterministic

parameters <- c(beta=1.1,gamma=.2)
simulate_discrete <- function(t,parameters){
  delta_t <- 1e-6
  i_prime <- c(1)
  s <- c(100000)
  i <- c(1)
  for (t_ in 2:t){
     i_prime <- c(i_prime,parameters[1]*s[t_-1]*i[t_-1]*delta_t)
     s <- c(s,s[t_-1] - i_prime[t_])
     i <- c(i,i[t_-1] + i_prime[t_] + parameters[2]*i[t_-1]*delta_t)
     
  }
  ret_df <- data.frame(i_prime=i_prime,s=s)
  return (ret_df)
} 



cori_estimator <- function(I_prime,parameters){
  r_t <-c()
  for (t in 2:length(I_prime)){
    p_sum <-0 
    for (i in 1:(t-1)){
      p_sum <-p_sum + I_prime[t-i]*parameters[2]*(1-parameters[2])**(i)
    }
    r_t <- c(r_t,I_prime[t]/p_sum)
  }
  return (r_t)
}

out <- simulate_discrete(500,parameters)
plot(out$i_prime)
r_t_hat <- cori_estimator(out$i_prime,parameters)
plot(r_t_hat,ylim=c(0,parameters[1]/parameters[2]+1))
lines(parameters[1]/parameters[2]*out$s/out$s[1],col='red')
