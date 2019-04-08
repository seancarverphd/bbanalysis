# KLI (Kullback-Leibler-Interactive) For R
# By Sean Carver
# Copyright 2017
# Provided Open Source, under GPL-version 3.0 License

require(quantreg)

#### EXAMPLE MODEL FUNCTIONS ####

sim.t <- function(n, df=Inf, seed=FALSE) {
  if (!identical(seed, FALSE)) {
    set.seed(seed)
  }
  # When df=Inf, rt is rnorm (default instead of error)
  return (rt(n, df))
}

likes.t <- function(x, df=Inf) {
  # When df=Inf, dt is dnorm (default)
  return (dt(x, df, log=TRUE))
}

# For other models, you would only need to rewrite 
# sim() and likes().  Nothing else would change.
# None of the functions below call sim() or likes().
# Instead, you pass in the results of these functions.
# Thus after you have created new sim() and likes(), 
# (multiple versions for different models, different names, OK),
# You can start using the code below, immediately.

#### CODE TO COMPARE MODELS ####

like.ratios <- function(likes.hyp, likes.alt) {
  return(likes.hyp - likes.alt)
}

aic <- function(likes.hyp, likes.alt) {
  # Only valid when not fitting parameters!
  return (sum(like.ratios(likes.hyp, likes.alt)))
}

#### CODE BASED ON KULLBACK-LEIBLER DIVERGENCE ####

KL.estimate <- function(likes.hyp, likes.alt) {
  return (mean(like.ratios(likes.hyp, likes.alt)))
}

reps10.estimate <- function(likes.hyp, likes.alt) {
  return (10/KL.estimate(likes.hyp,likes.alt))
}

KL.variance <- function(likes.hyp, likes.alt, KL=FALSE) {
  if (KL==FALSE) {
    KL <- KL.estimate(likes.hyp, likes.alt)
  }
  m <- length(likes.hyp)
  sum.squared.deviations <- sum((likes.hyp-likes.alt-KL)^2)
  return (sum.squared.deviations/(m^2))
}

KL.sd <- function(likes.hyp, likes.alt, KL=FALSE) {
  return (sqrt(KL.variance(likes.hyp, likes.alt, KL)))
}

KL.cv <- function(likes.hyp, likes.alt, KL=FALSE) {
  if (KL==FALSE) {
    KL <- KL.estimate(likes.hyp, likes.alt)
  }
  return (KL.sd(likes.hyp, likes.alt, KL)/KL)
}

#### DEFAULT SIZES FOR RATIO MATRIX ####

calculate.max.samples <- function(nratios,max.samples=NULL) {
  if (is.null(max.samples)) {
    return (floor(sqrt(nratios)))
  } else {
    return (max.samples)
  }
}

nreps.rows <- function(nratios, max.samples=NULL, bootstrap.rows=TRUE) {
  if (is.logical(bootstrap.rows)) {
    max.samples <- calculate.max.samples(nratios,max.samples)
    stopifnot(max.samples<=nratios)
    return (floor(nratios/max.samples))
  } else {
    return (bootstrap.rows)
  }
}

#### CUMSUM CODE ####

cumsum.rat <- function(likes.hyp, likes.alt) {
  return (cumsum(likeratios(likes.hyp, likes.alt)))
}

rat.matrix <- function(likes.hyp, likes.alt, max.samples=NULL) {
   ratios <- like.ratios(likes.hyp, likes.alt)
   max.samples <- calculate.max.samples(length(ratios), max.samples)
   reps <- nreps.rows(length(ratios), max.samples)
   ratios <- ratios[1:(reps*max.samples)]
   return (matrix(ratios,nrow=reps,ncol=max.samples))
}

cumsum.matrix <- function(likes.hyp, likes.alt, max.samples=NULL) {
  max.samples <- calculate.max.samples(length(likes.hyp), max.samples)
  rm.cumsum <- rat.matrix(likes.hyp, likes.alt, max.samples)
  for (row in (1:nrow(rm.cumsum))) {
    rm.cumsum[row,] <- cumsum(rm.cumsum[row,])
  }
  return (rm.cumsum)
}

#### CODE BASED ON BOOTSTRAP ####

bootstrap.matrix <- function(likes.hyp, likes.alt, max.samples=NULL, bootstrap.rows=TRUE, seed=FALSE) {
  if (bootstrap.rows == FALSE) {
    return (cumsum.matrix(likes.hyp, likes.alt, max.samples))
  }
  if (bootstrap.rows == TRUE) {
    bootstrap.rows <- nreps.rows(length(likes.hyp), max.samples)
  }
if (seed != FALSE) {
    set.seed(seed)
  }
  max.samples <- calculate.max.samples(length(likes.hyp), max.samples)
  rm.bootstrap <- matrix(rep(NA,bootstrap.rows*max.samples), nrow=bootstrap.rows, ncol=max.samples)
  rats = like.ratios(likes.hyp, likes.alt)
  for (i in 1:bootstrap.rows) {
    for (j in 1:max.samples) {
      rm.bootstrap[i,j] = sum(sample(rats, size=j, replace=TRUE))
    }
  }
  return (rm.bootstrap)
}

#### CODE FOR SAMPLES NEEDED ####

choose.hyp.matrix <- function(likes.hyp, likes.alt, max.samples=NULL, bootstrap.rows=TRUE, seed=FALSE) {
  return (bootstrap.matrix(likes.hyp, likes.alt, max.samples, bootstrap.rows, seed) > 0)
}

count.choose.hyp <- function(likes.hyp, likes.alt, max.samples=NULL, bootstrap.rows=TRUE, seed=FALSE) {
  max.samples <- calculate.max.samples(length(likes.hyp), max.samples)
  chm <- choose.hyp.matrix(likes.hyp, likes.alt, max.samples, bootstrap.rows, seed)
  count.h <- rep(NA,max.samples)
    for (i in 1:max.samples) {
      count.h[i] <- sum(chm[,i])
    }
    return (count.h)
}

prop.choose.hyp <- function(likes.hyp, likes.alt, max.samples=NULL, bootstrap.rows=TRUE, seed=FALSE) {
   counts <- count.choose.hyp(likes.hyp, likes.alt, max.samples, bootstrap.rows, seed)
   bootstrap.rows <- nreps.rows(length(likes.hyp), max.samples, bootstrap.rows)
   return (counts/bootstrap.rows)
}

variance.prop.choose.hyp <- function(likes.hyp, likes.alt, max.samples=NULL, bootstrap.rows=TRUE, seed=FALSE) {
  chm <- choose.hyp.matrix(likes.hyp, likes.alt, max.samples, bootstrap.rows, seed)
  prop <- prop.choose.hyp(likes.hyp, likes.alt, max.samples, bootstrap.rows, seed)
  max.samples <- calculate.max.samples(length(likes.hyp), max.samples)
  m <- nreps.rows(length(likes.hyp), max.samples, bootstrap.rows)
  sum.squared.deviations = rep(NA, max.samples)
  for (i in 1:max.samples) {
    sum.squared.deviations[i] <- sum((chm[,i]-prop[i])^2)
  }
  return (sum.squared.deviations/(m^2))
}

sd.prop.choose.hyp <- function(likes.hyp, likes.alt, max.samples=NULL, bootstrap.rows=TRUE, seed=FALSE) {
  return (sqrt(variance.prop.choose.hyp(likes.hyp, likes.alt, max.samples, bootstrap.rows, seed)))
}

quantile.columns <- function(likes.hyp, likes.alt, max.samples=NULL, bootstrap.rows=TRUE, seed=FALSE, confidence.level=.95) {
  stopifnot(confidence.level>0)
  stopifnot(confidence.level<1)
  max.samples <- calculate.max.samples(length(likes.hyp),max.samples)
  crm <- bootstrap.matrix(likes.hyp, likes.alt, max.samples, bootstrap.rows, seed)
  q.level <- 1.0 - confidence.level
  q <- rep(NA, max.samples)
  for (i in 1:max.samples) {
    q[i] <- quantile(crm[,i],prob=q.level)
  }
  return (q)
}

confident.hyp <- function(likes.hyp, likes.alt, max.samples=NULL, bootstrap.rows=TRUE, seed=FALSE, confidence.level=.95) {
  qc <- quantile.columns(likes.hyp, likes.alt, max.samples, bootstrap.rows, seed, confidence.level)
  return (qc > 0)
}

region.of.interest <- function(likes.hyp, likes.alt, max.samples=NULL, bootstrap.rows=TRUE, seed=FALSE, confidence.level=.95) {
  qc <- quantile.columns(likes.hyp, likes.alt, max.samples, bootstrap.rows, seed, confidence.level)
  confidenthyp <- (qc > 0)
  if (confidenthyp[1] == TRUE) {
    print("Unexpected: Already confident at 1 sample")
    begin.at.one <- TRUE
  } else {
    begin.at.one <- FALSE
  }
  if (tail((confidenthyp),1) == FALSE) {
    stop("Unexpected: Still not confident at last sample (sample number ", length(confidenthyp), ")")
  }
  transitions <- which(diff(confidenthyp)==1)
  if (begin.at.one == TRUE) {
    beginning <- 1
  } else {
    beginning <- transitions[1]
  }
  ending <- tail(transitions, 1) + 1
  roi <- beginning:ending
  quantiles.roi <- qc[roi]
  before = beginning - 1
  during = ending - beginning + 1
  after = max.samples - ending
  L <- list(roi, quantiles.roi, before/max.samples, during/max.samples, after/max.samples)
  names(L) <- c('samples','quantiles','prop.before','prop.during','prop.after')
  return(L)
}

quantile.fitted <- function(likes.hyp, likes.alt, 
                            max.samples=NULL, bootstrap.rows=TRUE, seed=FALSE, 
                            confidence.level=.95, quantile.regression=FALSE) {
  roi <- region.of.interest(likes.hyp, likes.alt, max.samples, bootstrap.rows, seed, confidence.level)
  if (quantile.regression) {
  # Computes CRM twice.  Need to fix this.
  crm <- bootstrap.matrix(likes.hyp, likes.alt, max.samples, bootstrap.rows, seed)
  M <- c()
  for (k in 1:max.samples) {
    M <- cbind(M,rep(k,bootstrap.rows))
  }
  M <- M[,roi$samples]
  crm <- crm[,roi$samples]
  summed.like.ratios <- data.frame(samples=as.vector(M),lrs=as.vector(crm))
  return(rq(lrs~samples, tau=1-confidence.level, data=summed.like.ratios))
  } else {
    return (lm(roi$quantiles~roi$samples))
  }
}

estimated.quantiles <- function(likes.hyp, likes.alt, 
                                max.samples=NULL, bootstrap.rows=TRUE, seed=FALSE, 
                                confidence.level=.95) {
  roi <- region.of.interest(likes.hyp, likes.alt, max.samples, bootstrap.rows, seed, confidence.level)
  fit <- lm(roi$quantiles~roi$samples)
  b = fit$coefficients[1]
  m = fit$coefficients[2]
  names(b) <- NULL
  names(m) <- NULL
  x <- roi$samples
  yhat <- m*roi$samples + b
  L <- list(x, yhat)
  names(L) <- c('samples','est.quantiles')
  return(L)
}

samples.needed <- function(likes.hyp, likes.alt, 
                           max.samples=NULL, bootstrap.rows=TRUE, seed=FALSE, 
                           confidence.level=.95, quantile.regression=FALSE) {
  fit <- quantile.fitted(likes.hyp, likes.alt, max.samples, bootstrap.rows, seed, 
                         confidence.level, quantile.regression)
  needed <- -fit$coefficients[1]/fit$coefficients[2]
  names(needed) <- NULL
  return(needed)
}

rounded.samples.needed <- function(likes.hyp, likes.alt, 
                                   max.samples=NULL, bootstrap.rows=TRUE, seed=FALSE, 
                                   confidence.level=.95, quantile.regression=FALSE) {
  return (ceiling(samples.needed(likes.hyp, likes.alt, max.samples, bootstrap.rows, seed, 
                                 confidence.level, quantile.regression)))
}
