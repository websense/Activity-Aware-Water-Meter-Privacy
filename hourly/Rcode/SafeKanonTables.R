## Safe-kanon Tables
## Calculations for Li2012
## R Cardell-Oliver
## Sep 2021


## Create a lookup table for these parameters to be tested
{
beta = c(0.5,0.1,0.2)[3]
ks = (2:10)*5 #as in table or kanon
k=20
deltamax = 10^-6 #upper bound for delta
##find eps (nat log = ln)
eps >= (-log(1-beta))
#delta=getdelta(k,b,eps) see below
}



# Defns from Li et al
{
# F(j; n, β) to denotes the cumulative probability mass function
# for binomial getting exactly j successes in n trials where 
# each trial succeeds with probability β. And
# 
# Theorem 5. Any strongly-safe k-anonymization algorithm 
# satisfies (β, ǫ, δ)-DPS for any 0 < β < 1, eps ≥ − ln(1 − β), and δ = d(k, β, ǫ), 
# where the function d is defined as
# 
# d(k, β, ǫ) = maxn for ni in n:n>ceil(k/g-1) (sum from j>gn to ni (f(j; n, β))
# 
# where γ = (E^eps−1+β) / E^eps


getdelta <- function(k,beta,eps) {
  allsi=c()
  g = (exp(1)^eps - 1 + beta) / exp(1)^eps
  n = ceiling( (k/g) - 1)
  for (ni in n:(n+10)) {
    gn = ceiling(g*ni)
    si = 0
    for (j in gn:ni) {
      si = si + dbinom(j,ni,beta)
    }
    allsi = c(allsi,si)
  }
  return(max(allsi))
}
}

## Make lookup table for parameter combinations
{
res=c()
for (k in 10:50) { #k=5 only met for 0.05 sample
  for (b in c(5,10,20)) { #sampling percent
    beta = b/100 
    epsmin = (-log(1-beta))
    epstry = seq(from=epsmin,to=2.0,by=0.01) 
    for (eps in epstry) {
      delta = getdelta(k,beta,eps)
      res = rbind(res, c(k,beta,eps,delta))
    }
  }
}


eallb=c()
for (b in c(5,10,20)/100) {
dok = which(res[,2]==b & res[,4]<=deltamax)
bb = boxplot(res[dok,3] ~ res[dok,1],plot=FALSE) #main=b)
emin = bb$stats[1,] #min e for d<10^-7
eallb = rbind(eallb,emin)
}
rownames(eallb)=c(5,10,20)/100
colnames(eallb)=10:50
}




## TABLE for paper for our kanon
{
besteps = matrix(nrow=3,ncol=length(kanon))
rownames(besteps)=c(5,10,20)/100
colnames(besteps)=kanon
for (ki in 1:length(kanon)) { #k=5 only met for 0.05 sample
  k = kanon[ki]
  for (bi in 1:3) { #sampling percent
    beta = c(5,10,20)[bi]/100 
    epsmin = (-log(1-beta))
    epstry = seq(from=epsmin,to=2.0,by=0.01) 
    epsres = c()
    for (eps in epstry) {
      delta = getdelta(k,beta,eps)
      epsres = rbind(epsres, c(eps,delta))
    }
    dok = which(epsres[,2]<=deltamax)
    if (length(dok)>0) {
      besteps[bi,ki] = min(epsres[dok,1])
    } 
  }
}

##basef on Li2012 table 2 caption="A table showing the relationship between β and ǫ in determining the value of δ 
#when k is fixed. In the above k = 20, and each cell in the table reports the value of 
#δ under the given values of β and ǫ")

require(xtable)
xtable(besteps,digits=2)
}
