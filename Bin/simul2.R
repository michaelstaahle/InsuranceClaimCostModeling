## Branch 2

branch2_sma <- branch2$ClaimCost[branch2$ClaimCost<500000]
branch2_stora <- branch2$ClaimCost[branch2$ClaimCost>500000]
log_data <- log(branch2_sma)
b2_sma_lgamma <- fitdistr(log_data, 'gamma')
p2 <- length(branch2_sma)/length(branch2$ClaimCost)

## k_xl

## simulering 
l1 <- 0
n <- 2000
k_xl2 <- c()
mean_k_xl2 <- c()
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
  
  skador2 <- c()
  k=0
  while(k<sum(bra2s) + sum(bra2w)){
    ru <- runif(1,0,1)
    if(ru < p2){
      skador2 <- c(skador2,exp(rgamma(1,shape=b2_sma_lgamma$estimate[1],rate=b2_sma_lgamma$estimate[2])))
    }
    else{
      skador2 <- c(skador2,rnorm(1,mean(branch2_stora),sd(branch2_stora)))
    }
    k <- k+1
  }
  
  sort_claimcost <- sort(skador2)
  e_cdf <- 1:length(sort_claimcost) / length(sort_claimcost)
  k_xl2 <- c(k_xl2,sort_claimcost[which(e_cdf >= 0.9)[1]])
  mean_k_xl2 <- c(mean_k_xl2,mean(k_xl2))
  l1 <- l1 +1
}

## slut simulering


## k_xl i orginaldata

sort_claimcost <- sort(branch2$ClaimCost)
e_cdf <- 1:length(sort_claimcost) / length(sort_claimcost)
sort_claimcost[which(e_cdf >= 0.9)[1]]


## k_sl

## simulering 
l1 <- 0
n <- 2000
sumskador2 <- c()
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
  
  skador2 <- c()
  k=0
  while(k<sum(bra2s) + sum(bra2w)){
    ru <- runif(1,0,1)
    if(ru < p2){
      skador2 <- c(skador2,exp(rgamma(1,shape=b2_sma_lgamma$estimate[1],rate=b2_sma_lgamma$estimate[2])))
    }
    else{
      skador2 <- c(skador2,rnorm(1,mean(branch2_stora),sd(branch2_stora)))
    }
    k <- k+1
  }
  
  sumskador2 <- c(sumskador2,sum(skador2))
  l1 <- l1 +1
}

## slut simulering

sort_claimcost <- sort(sumskador2)
e_cdf <- 1:length(sort_claimcost) / length(sort_claimcost)
k_sl2 <- sort_claimcost[which(e_cdf >= 0.9)[1]]

## Test för rimligt resultat

bb1 <- c()
for(i in 0:9){
  bb1 <- c(bb1,sum(claim_m2[(1 +i*12):(12 + i*12)]))
}
rm(i)
### summa av skadorna per år i data
sum_skador_dag1 <- c()
for(i in 1:max(branch2$ClaimDay)){
  sum_skador_dag1 <- c(sum_skador_dag1,sum(branch2$ClaimCost[branch2$ClaimDay == i]))
}

myxts_skad1 <- xts(sum_skador_dag1, as.Date(day))
skador_year1 <- apply.yearly(myxts_skad1,FUN = sum) ## Skapar antalet claims per månad

## z_xl

## simulering 
l1 <- 0
n <- 2000
z_xl2 <- c()
meanz_xl2 <- c()
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
  
  skador2 <- c()
  k=0
  while(k<sum(bra2s) + sum(bra2w)){
    ru <- runif(1,0,1)
    if(ru < p2){
      skador2 <- c(skador2,exp(rgamma(1,shape=b2_sma_lgamma$estimate[1],rate=b2_sma_lgamma$estimate[2])))
    }
    else{
      skador2 <- c(skador2,rnorm(1,mean(branch2_stora),sd(branch2_stora)))
    }
    k <- k+1
  }
  
  z_xl2 <- c(z_xl2,sum(sapply(skador2, function(x) max(x-mean(k_xl2),0)))) 
  meanz_xl2 <- c(meanz_xl2,mean(z_xl2))
  l1 <- l1 +1
}

## slut simulering

## z_sl

## simulering 
l1 <- 0
n <- 2000
z_sl2 <- c()
meanz_sl2 <- c()
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
  
  skador2 <- c()
  k=0
  while(k<sum(bra2s) + sum(bra2w)){
    ru <- runif(1,0,1)
    if(ru < p2){
      skador2 <- c(skador2,exp(rgamma(1,shape=b2_sma_lgamma$estimate[1],rate=b2_sma_lgamma$estimate[2])))
    }
    else{
      skador2 <- c(skador2,rnorm(1,mean(branch2_stora),sd(branch2_stora)))
    }
    k <- k+1
  }
  
  z_sl2 <- c(z_sl2,max(sum(skador2)-k_sl2,0)) 
  meanz_sl2 <- c(meanz_sl2,mean(z_sl2))
  l1 <- l1 +1
}

## slut simulering

par(mfrow=c(2,2))
plot(meanz_xl,ylab = 'E[z_xl]', xlab = 'n', main = 'Branch 1')
plot(meanz_xl2,ylab = 'E[z_xl]', xlab = 'n', main = 'Branch 2')
plot(meanz_sl,ylab = 'E[z_sl]', xlab = 'n', main = 'Branch 1')
plot(meanz_sl2,ylab = 'E[z_sl]', xlab = 'n', main = 'Branch 2')
