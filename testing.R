## deterministic

parameters <- c(beta=.8,gamma=.6 )
simulate_discrete <- function(t,parameters){
  i_prime <- c(1)
  i_prime_observed <- c(1)
  s <- c(1000000000)
  i <- c(10000)
  for (t_ in 2:t){
    i_prime <- c(i_prime,parameters[1]*(s[t_-1]/s[1])*i[t_-1])
    s <- c(s,max(s[t_-1] - i_prime[t_],0))
    i <- c(i,i[t_-1] + i_prime[t_] - parameters[2]*i[t_-1])
    if (i_prime[t_] > 0){
      i_prime_observed[t_] <- rbinom(n=1,size=round(i_prime[t_]),prob=c(rep(.3,40),rep(.7,160))[t_])
    } else{
      i_prime_observed[t_] <-  0
      
    }
  }
  ret_df <- data.frame(i_prime=i_prime,s=s,i=i,i_prime_observed)
  return (ret_df)
} 
out <- simulate_discrete(200,parameters)
plot(out$i_prime)
plot(out$i)




cori_estimator <- function(I_prime,parameters){
  r_t <-c()
  for (t in 2:length(I_prime)){
    p_sum <-0 
    for (i in 1:(t-1)){
      p_sum <-p_sum + I_prime[t-i]*parameters[2]*(1-parameters[2])**(i-1)
    }
    r_t <- c(r_t,I_prime[t]/p_sum)
  }
  return (r_t)
}


r_t_hat <- cori_estimator(out$i_prime_observed,parameters)
plot(r_t_hat,ylim=c(0,parameters[1]/parameters[2]+1))
lines(parameters[1]/parameters[2]*out$s/out$s[1],col='red')


