
## Distribution of total claim cost
par(mfrow=c(1,1))
tot_sum <- sumskador +sumskador2
hist(tot_sum,100)


## Distribution with XL cover
sum_mintot <- sum_min_yu1 +sum_min_yu2
hist(sum_mintot,50)

sum_xl <- sum_mintot + 1.1*(mean(z_xl) + mean(z_xl2))
hist(sum_xl,40)


## Distribution with sl cover
sum_minsl_1 <- sapply(sumskador, FUN=function(x) min(x,k_sl))
sum_minsl_2 <- sapply(sumskador2, FUN=function(x) min(x,k_sl2))

sum_min_totsl <- sum_minsl_1+sum_minsl_2
sum_sl <- sum_min_totsl + 1.1*(mean(z_sl) + mean(z_sl2))

hist(sum_sl,50)
hist(sum_min_totsl,50)
hist(tot_sum,50)


sort_claimcost2 <- sort(sum_xl)
e_cdf2 <- 1:length(sort_claimcost2) / length(sort_claimcost2)
sort_claimcost2[which(e_cdf2 >= 0.999)[1]]

plot(sort_claimcost2,e_cdf2)

sort_claimcost2 <- sort(sum_sl)
e_cdf2 <- 1:length(sort_claimcost2) / length(sort_claimcost2)
sort_claimcost2[which(e_cdf2 >= 0.95)[1]]


## z_sl3
k_sl3 <- k_sl+k_sl2
z_sl3 <- sapply(tot_sum, FUN = function(x) max(x-k_sl3,0))
mean(z_sl3)

sum_minsl3 <- sapply(tot_sum, FUN=function(x) min(x,k_sl3))
sum_sl3 <- sum_minsl3 + 1.1*mean(z_sl3)

hist(sum_sl3,40)
hist(sum_minsl3,50)
hist(tot_sum,100)

sort_claimcost2 <- sort(sum_sl3)
e_cdf2 <- 1:length(sort_claimcost2) / length(sort_claimcost2)
sort_claimcost2[which(e_cdf2 >= 0.95)[1]]
plot(sort_claimcost2,e_cdf2)


par(mfrow=c(1,3))
hist(tot_sum,50, xlab = 'Total claim cost', main = 'No cover')
hist(sum_xl,40, xlab = 'Total claim cost', main = 'XL-cover')
hist(sum_sl,40, xlab = 'Total claim cost', main = 'SL-cover')

par(mfrow=c(1,1))
hist(sum_sl3,50, xlab = 'Total claim cost', main = 'Joint SL-cover')

## XL

## DIst functions
sort_sumxl <- sort(sum_xl)
e_cdf1 <- 1:length(sort_sumxl) / length(sort_sumxl)

## Quantile function
se_q <- seq(from=0.9, to=0.999999,length.out = 1000)
qq_xl <- c()
for(i in se_q){
qq_xl <- c(qq_xl,sort_sumxl[which(e_cdf1 >= i)[1]])
}
plot(se_q,qq_xl)

## SL

sort_sumsl <- sort(sum_sl)
e_cdf2 <- 1:length(sort_sumsl) / length(sort_sumsl)
sort_sumsl[which(e_cdf2 >= 0.95)[1]]

## quantile function
qq_sl <- c()
for(i in se_q){
  qq_sl <- c(qq_sl,sort_sumsl[which(e_cdf2 >= i)[1]])
}
plot(se_q,qq_sl)

## Joint sl

sort_sumsl3 <- sort(sum_sl3)
e_cdf3 <- 1:length(sort_sumsl3) / length(sort_sumsl3)
sort_sumsl3[which(e_cdf3 >= 0.95)[1]]

## quantile function
qq_sl3 <- c()
for(i in se_q){
  qq_sl3 <- c(qq_sl3,sort_sumsl3[which(e_cdf3 >= i)[1]])
}
plot(se_q,qq_sl3)

par(mfrow=c(1,3))
plot(se_q,qq_xl,xlab = 'Probability', ylab = 'Quantiles', main= 'XL-cover')
plot(se_q,qq_sl,xlab = 'Probability', ylab = 'Quantiles', main= 'SL-cover')
plot(se_q,qq_sl3,xlab = 'Probability', ylab = 'Quantiles', main= 'Joint SL-cover')


par(mfrow=c(1,3))
plot(sort_sumxl,e_cdf1, xlab = 'Total claim cost', ylab = 'Probability', main = 'XL-cover')
plot(sort_sumsl,e_cdf2, xlab = 'Total claim cost', ylab = 'Probability', main = 'SL-cover')
plot(sort_sumsl3,e_cdf3, xlab = 'Total claim cost', ylab = 'Probability', main = 'Joint SL-cover')


