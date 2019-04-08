repeated.estimates <- function(reps=6, seed=FALSE, quantile.regression=FALSE) {
  if (seed != FALSE) {
    set.seed(seed)
  }
  estimates <- c()
  for (k in 1:reps) {
    bs.t5 <- sim.t(200000, df=5, seed=seed+k)
    ll.t5.t5 <- likes.t(bs.t5, df=5)
    ll.t5.normal <- likes.t(bs.t5, df=Inf)
    print(c(seed,seed+k,seed+k+reps))
    estimates[k] <- samples.needed(ll.t5.t5, ll.t5.normal, seed=seed+k+reps,
                                   max.samples=250, bootstrap.rows=1000, quantile.regression=quantile.regression)
  }
  return(estimates)
}

baseball.repeated.estimates <- function(reps=6, seed=FALSE, quantile.regression=FALSE) {
  if (seed != FALSE) {
    set.seed(seed)
  }
  estimates <- c()
  for (k in 1:reps) {
    bs.BAL <- sim.baseball(10000, BAL, seed=seed+k)
    ll.BAL.BAL <- likes.baseball(bs.BAL, BAL)
    ll.BAL.NYA <- likes.baseball(bs.BAL, NYA)
    print(c(seed,seed+k,seed+k+reps))
    estimates[k] <- samples.needed(ll.BAL.BAL, ll.BAL.NYA, seed=seed+k+reps,
                                   max.samples=1000, bootstrap.rows=1000, quantile.regression=quantile.regression)
  }
  return(estimates)
}