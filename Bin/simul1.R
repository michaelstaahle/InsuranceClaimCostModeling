## Branch 1

## K_xl ##
branch1_sma <- branch1$ClaimCost[branch1$ClaimCost<100000]
branch1_stora <- branch1$ClaimCost[branch1$ClaimCost>100000]
b1_sma_lnfit <- fitdistr(branch1_sma,'lognormal')
p <- length(branch1_sma)/length(branch1$ClaimCost)


rm(x1)
rm(i)

## simulering 
l1 <- 0
n <- 1000
k_xl <- c()
mean_k_xl <- c()
while(l1 < n){
a <- round(runif(4,1,40))
b <- round(runif(8,1,80))
bra1s <- c()
bra1w <- c()
bra2s <- c()
bra2w <- c()

for(i in 1:4){
  bra1s <- c(bra1s,summermm[a[i]])
  bra2s <- c(bra2s,summermm2[a[i]])
}

rm(i)

for(i in 1:8){
  bra1w <- c(bra1w,wintermm[b[i]])
  bra2w <- c(bra2w,wintermm2[b[i]])
}

rm(i)

skador1 <- c()
k=0
while(k<sum(bra1s) + sum(bra1w)){
  ru <- runif(1,0,1)
  if(ru < p){
    skador1 <- c(skador1,rlnorm(1,b1_sma_lnfit$estimate[1],b1_sma_lnfit$estimate[2]))
  }
  else{
    skador1 <- c(skador1,rnorm(1,mean(branch1_stora),sd(branch1_stora)))
  }
  k <- k+1
}

sort_claimcost <- sort(skador1)
e_cdf <- 1:length(sort_claimcost) / length(sort_claimcost)
k_xl <- c(k_xl,sort_claimcost[which(e_cdf >= 0.9)[1]])
mean_k_xl <- c(mean_k_xl,mean(k_xl))
l1 <- l1 +1
}

## slut simulering

k_xl <- mean(k_xl)

z_xl <- sum(sapply(skador1, function(x) max(x-k_xl,0))) 

## k_xl i orginaldata

sort_claimcost <- sort(branch1$ClaimCost)
e_cdf <- 1:length(sort_claimcost) / length(sort_claimcost)
sort_claimcost[which(e_cdf >= 0.9)[1]]


## k_sl 

## simulering 
l1 <- 0
n <- 1000
sumskador <- c()
while(l1 < n){
  a <- round(runif(4,1,40))
  b <- round(runif(8,1,80))
  bra1s <- c()
  bra1w <- c()
  bra2s <- c()
  bra2w <- c()
  
  for(i in 1:4){
    bra1s <- c(bra1s,summermm[a[i]])
    bra2s <- c(bra2s,summermm2[a[i]])
  }
  
  rm(i)
  
  for(i in 1:8){
    bra1w <- c(bra1w,wintermm[b[i]])
    bra2w <- c(bra2w,wintermm2[b[i]])
  }
  
  rm(i)
  
  skador1 <- c()
  k=0
  while(k<sum(bra1s) + sum(bra1w)){
    ru <- runif(1,0,1)
    if(ru < p){
      skador1 <- c(skador1,rlnorm(1,b1_sma_lnfit$estimate[1],b1_sma_lnfit$estimate[2]))
    }
    else{
      skador1 <- c(skador1,rnorm(1,mean(branch1_stora),sd(branch1_stora)))
    }
    k <- k+1
  }
  
  sumskador <- c(sumskador,sum(skador1))
  l1 <- l1 +1
}

## slut simulering

sort_claimcost <- sort(sumskador)
e_cdf <- 1:length(sort_claimcost) / length(sort_claimcost)
k_sl <- sort_claimcost[which(e_cdf >= 0.9)[1]]


## z_xl 

## simulering 
l1 <- 0
n <- 1000
z_xl <- c()
meanz_xl <- c()
sum_min_yu <- c()
sumskador_tot <- c()
while(l1 < n){
  a <- round(runif(4,1,40))
  b <- round(runif(8,1,80))
  bra1s <- c()
  bra1w <- c()
  bra2s <- c()
  bra2w <- c()
  
  for(i in 1:4){
    bra1s <- c(bra1s,summermm[a[i]])
    bra2s <- c(bra2s,summermm2[a[i]])
  }
  
  rm(i)
  
  for(i in 1:8){
    bra1w <- c(bra1w,wintermm[b[i]])
    bra2w <- c(bra2w,wintermm2[b[i]])
  }
  
  rm(i)
  
  skador1 <- c()
  k=0
  while(k<sum(bra1s) + sum(bra1w)){
    ru <- runif(1,0,1)
    if(ru < p){
      skador1 <- c(skador1,rlnorm(1,b1_sma_lnfit$estimate[1],b1_sma_lnfit$estimate[2]))
    }
    else{
      skador1 <- c(skador1,rnorm(1,mean(branch1_stora),sd(branch1_stora)))
    }
    k <- k+1
  }
  
  z_xl <- c(z_xl,sum(sapply(skador1, function(x) max(x-mean(k_xl),0)))) 
  meanz_xl <- c(meanz_xl,mean(z_xl))
  sum_min_yu <- c(sum_min_yu,sum(sapply(skador1, FUN = function(x) min(x,mean(k_xl)))))
  sumskador_tot <- c(sumskador_tot,sum(skador1))
  l1 <- l1 +1
}

## slut simulering

par(mfrow=c(1,3))
hist(sumskador_tot,30)
hist(sum_min_yu,30)
hist(sum_min_yu +1.1*mean(z_xl),30)

## z_sl
## simulering 
l1 <- 0
n <- 2000
z_sl <- c()
meanz_sl <- c()
while(l1 < n){
  a <- round(runif(4,1,40))
  b <- round(runif(8,1,80))
  bra1s <- c()
  bra1w <- c()
  bra2s <- c()
  bra2w <- c()
  
  for(i in 1:4){
    bra1s <- c(bra1s,summermm[a[i]])
    bra2s <- c(bra2s,summermm2[a[i]])
  }
  
  rm(i)
  
  for(i in 1:8){
    bra1w <- c(bra1w,wintermm[b[i]])
    bra2w <- c(bra2w,wintermm2[b[i]])
  }
  
  rm(i)
  
  skador1 <- c()
  k=0
  while(k<sum(bra1s) + sum(bra1w)){
    ru <- runif(1,0,1)
    if(ru < p){
      skador1 <- c(skador1,rlnorm(1,b1_sma_lnfit$estimate[1],b1_sma_lnfit$estimate[2]))
    }
    else{
      skador1 <- c(skador1,rnorm(1,mean(branch1_stora),sd(branch1_stora)))
    }
    k <- k+1
  }
  
  z_sl <- c(z_sl,max(sum(skador1)-k_sl,0)) 
  meanz_sl <- c(meanz_sl,mean(z_sl))
  l1 <- l1 +1
}
## slut simulering

spara_1000 <- meanz_sl
