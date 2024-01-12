group.stats <- function(groupOne, groupTwo) {
  nonparam.effect <- function(U, n1, n2) {
    return(1 - ((2*U)/(n1*n2)))
  }
  
  cis = lapply((1:ncol(groupOne)), function(x) {
    ci = NA; eff = NA; std.dev = NA; mw = NA; med = NA;
    if(length(groupOne) > 0 && length(groupTwo) > 0) {
      if(length(groupOne[,x]) > 1 && length(groupTwo[,x]) > 1) {
        ci = t.test(groupOne[,x], groupTwo[,x], conf.level = 0.95)
      }
      mw = wilcox.test(groupOne[,x], groupTwo[,x], conf.level = 0.95)
      med = c(median(groupOne[,x]), median(groupTwo[,x]))
      eff = rENA::fun_cohens.d(groupOne[,x], groupTwo[,x])
      std.dev = c(sd(groupOne[,x]), sd(groupTwo[,x]))
      
      return(list(ci = ci, effect.d = eff, std.dev = std.dev, mw = mw, median = med))
    } else {
      return(NA)
    }
  })
  toret = list(
    N = c(nrow(groupOne), nrow(groupTwo)),
    parametric = list(
      t = rep(NA,ncol(groupOne)),
      statistic = rep(NA,ncol(groupOne)),
      pvalue = rep(NA,ncol(groupOne)),
      effect = rep(NA,ncol(groupOne)),
      mean = matrix(0, ncol=ncol(groupOne), nrow=2),
      std.dev = matrix(0, ncol=ncol(groupOne), nrow=2)
    ),
    nonparametric = list(
      U = rep(NA,ncol(groupOne)),
      pvalue = rep(NA,ncol(groupOne)),
      effect = rep(NA,ncol(groupOne)),
      median = matrix(0, ncol=ncol(groupOne), nrow=2)
    )
  )
  
  if(length(groupOne) > 0 && length(groupTwo) > 0 ) {
    for(i in 1:ncol(groupOne)) {
      if(is(cis[[i]]$ci, "htest")) {
        toret[["parametric"]][["parameter"]][i] = cis[[i]]$ci$parameter
        toret[["parametric"]][["t"]][i] = cis[[i]]$ci$statistic
        toret[["parametric"]][["pvalue"]][i] = cis[[i]]$ci$p.value
        toret[["parametric"]][["mean"]][,i] = cis[[i]]$ci$estimate
      }
      toret$parametric[["effect"]][i] = cis[[i]]$effect.d
      toret$parametric[["std.dev"]][,i] = cis[[i]]$std.dev
      
      if(is(cis[[i]]$mw, "htest")) {
        toret[["nonparametric"]][["U"]][i] = cis[[i]]$mw$statistic
        toret[["nonparametric"]][["pvalue"]][i] = cis[[i]]$mw$p.value
        toret$nonparametric[["effect"]][i] = nonparam.effect(cis[[i]]$mw$statistic, nrow(groupOne), nrow(groupTwo))
      }
      toret$nonparametric[["median"]][,i] = cis[[i]]$median
    }
  }
  return(toret)
}
nonparam.effect <- function(U, n1, n2) {
  return(1 - ((2*U)/(n1*n2)))
}