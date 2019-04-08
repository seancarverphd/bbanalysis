df.true <- 5    # Suggested range: 1 to 50
df.hyp <- 5     # Suggested range: 1 to 50
df.alt <- 2000  # Suggested range: 1 to 5000

nsamples <- 100000       # Suggested range: 1000 to 500000
max.samples <- 500      # Suggested range: 10 to 5000
bootstrap.rows <- 1000   # Suggested range: 10 to 5000

confidence.level = .95   # Suggested range: .5 to .99
seed <- 1                # Suggested range: 1 to 100

tru.data <- sim.t(n=nsamples, df=df.true, seed=seed)
h.likes = likes.t(tru.data, df=df.hyp)
a.likes = likes.t(tru.data, df=df.alt)
needed <- samples.needed(likes.hyp=h.likes, 
                         likes.alt=a.likes, 
                         max.samples=max.samples, 
                         bootstrap.rows=bootstrap.rows, 
                         seed=FALSE, 
                         confidence.level=confidence.level)
print(needed)
  