# Båda brancher jointly

## k_sl

## simulering 
l1 <- 0
n <- 2000
k_xl <- 38163.02
k_xl2 <- 63171.34
k_sl <- 96871226
k_sl2 <- 336441115
z_xl <- c()
z_xl2 <- c()
z_sl <- c()
z_sl2 <- c()
meanz_sl <- c()
meanz_sl2 <- c()
sumskador <- c()
sumskador2 <- c()
meanz_xl <- c()
meanz_xl2 <- c()
sum_min_yu1 <- c()
sum_min_yu2 <- c()
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
  l2=0
  N = sum(bra1s) + sum(bra1w)
  while(l2<N){
    ru1 <- runif(1,0,1)
    if(ru1 < p){
      skador1 <- c(skador1,rlnorm(1,b1_sma_lnfit$estimate[1],b1_sma_lnfit$estimate[2]))
    }
    else{
      skador1 <- c(skador1,rnorm(1,mean(branch1_stora),sd(branch1_stora)))
    }
    l2 <- l2+1
  }
  
  sumskador <- c(sumskador,sum(skador1))
  z_xl <- c(z_xl,sum(sapply(skador1, function(x) max(x-k_xl,0)))) 
  meanz_xl <- c(meanz_xl,mean(z_xl))
  sum_min_yu1 <- c(sum_min_yu1,sum(sapply(skador1, FUN = function(x) min(x,k_xl))))
  
  z_sl <- c(z_sl,max(sum(skador1)-k_sl,0)) 
  meanz_sl <- c(meanz_sl,mean(z_sl))
  
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
  z_xl2 <- c(z_xl2,sum(sapply(skador2, function(x) max(x-k_xl2,0)))) 
  meanz_xl2 <- c(meanz_xl2,mean(z_xl2))
  sum_min_yu2 <- c(sum_min_yu2,sum(sapply(skador2, FUN = function(x) min(x,k_xl2))))
  
  z_sl2 <- c(z_sl2,max(sum(skador2)-k_sl2,0)) 
  meanz_sl2 <- c(meanz_sl2,mean(z_sl2))
  l1 <- l1 +1
}

## slut simulering

sumskador3 <- sumskador + sumskador2
sort_claimcost <- sort(sumskador3)
e_cdf <- 1:length(sort_claimcost) / length(sort_claimcost)
k_sl3 <- sort_claimcost[which(e_cdf >= 0.9)[1]]

## z_sl3
z_sl3 <- sapply(sumskador3, FUN = function(x) max(x-k_sl3,0))
mean(z_sl3)

meanz_sl3 <- c()
for(i in 1:length(sumskador3)){
  meanz_sl3 <- c(meanz_sl3,mean(z_sl3[1:i]))
}


