## Average claim size per day
mean.Claimcost <- aggregate(ClaimCost~ClaimDay,FUN=mean,data=branch1)

## Sum claim size per day
sum.Claimcost <- aggregate(ClaimCost~ClaimDay,FUN=sum,data=branch1)

mean.skad <- xts(mean.Claimcost$ClaimCost, as.Date(mean.Claimcost$ClaimDay, origin='1970-01-01'))
sum.skad <- xts(sum.Claimcost$ClaimCost, as.Date(sum.Claimcost$ClaimDay, origin='1970-01-01'))

plot(branch1$ClaimDay,branch1$ClaimCost)

plot(branch2$ClaimDay,branch2$ClaimCost)

hist(branch1$ClaimCost,50)

hist(branch2$ClaimCost)

mean_claims <- c()
for(i in 1:max(branch1$ClaimDay)){
  mean_claims <- c(mean_claims,mean(branch1$ClaimCost[which(branch1$ClaimDay == i)]))
}

## Stora skador branch 1
branch1_stora <- branch1$ClaimCost[branch1$ClaimCost>100000]
hist(branch1_stora,30)
qqnorm(branch1_stora) ## Ok anpassning till normal
qqline(branch1_stora)

qq_snorm1 <- qnorm(ppoints(branch1_stora), mean = mean(branch1_stora), sd = sd(branch1_stora))
sort_branch1s <- sort(branch1_stora)
plot(qq_snorm1,sort_branch1s, ylab = 'Theoretical quantiles', xlab = 'Sample quantiles', main = 'Branch 1')
lines(seq(0:420000),seq(0:420000))

fitg <- fitdistr(branch1_stora,'gamma')


## Små skador branch 1
branch1_sma <- branch1$ClaimCost[branch1$ClaimCost<100000]
hist(branch1_sma,30)
qqnorm(branch1_sma)  ## Väldigt dålig anpassning till normal
qqline(branch1_sma)

### Pareto
b1_sma_pfit <- fitdistr(branch1_sma,densfun = dPareto,start = list(alpha =6, gamma=4000), lower=c(0,0))

## lognormal
b1_sma_lnfit <- fitdistr(branch1_sma,'lognormal')
qq_ln <- qlnorm(ppoints(branch1_sma),b1_sma_lnfit$estimate[1],b1_sma_lnfit$estimate[2])
qqplot(qq_ln,branch1_sma)
lines(seq(0:120000),seq(0:120000))

sort_b1sma <- sort(branch1_sma)
plot(qq_ln,sort_b1s, ylab = 'Theoretical quantiles', xlab = 'Sample quantiles', main = 'Branch 1')
lines(seq(0:150000),seq(0:150000))

## log-gamma
log_data1 <- log(branch1_sma)
b1_sma_lgamma <- fitdistr(log_data1, 'gamma')
qq_b1sma_lgamma <- qgamma(ppoints(branch1_sma),shape = b1_sma_lgamma$estimate[1], rate = b1_sma_lgamma$estimate[2])
qqplot(exp(qq_b1sma_lgamma),branch1_sma)
lines(seq(1:140000),seq(1:140000))


### Stora skador branch2 
branch2_stora <- branch2$ClaimCost[branch2$ClaimCost>500000]
qqnorm(branch2_stora) ## Mycket bra anpassning till normal
qqline(branch2_stora)
hist(branch2_stora,30)

sort_branch2_stora <- sort(branch2_stora)
qq_branch2_stora <- qnorm(ppoints(branch2_stora),mean = mean(branch2_stora), sd = sd(branch2_stora))
plot(qq_branch2_stora,sort_branch2_stora, ylab = 'Theoretical quantiles', xlab = 'Sample quantiles', main = 'Branch 2')
lines(seq(0:3500000),seq(0:3500000))

### Små skador branch2

branch2_sma <-branch2$ClaimCost[branch2$ClaimCost<500000] 
hist(branch2_sma,30)
qqnorm(branch2_sma) ## väldigt dålig anpassning till normal
qqline(branch2_sma)

## lognormal
b2_sma_lnfit <- fitdistr(branch2_sma, 'lognormal')
qq_b2sma_ln <- qlnorm(ppoints(branch2_sma),b2_sma_lnfit$estimate[1],b2_sma_lnfit$estimate[2])
qqplot(qq_b2sma_ln,branch2_sma)
lines(seq(0:200000),seq(0:200000))


## Gamma
start_val <- list(shape = mean(branch2_sma)^2/var(branch2_sma), scale = var(branch2_sma)/mean(branch2_sma))

scaled_data <- 0.1*branch2_sma
b2_sma_gamma <- fitdistr(scaled_data, 'gamma')

my_scale <- (1/b2_sma_gamma$estimate[2])*10

qq_b2sma_gamma <- qgamma(ppoints(branch2_sma),shape = b2_sma_gamma$estimate[1], scale = my_scale)
qqplot(qq_b2sma_gamma,branch2_sma)
lines(seq(0:200000),seq(0:200000))



## Weibull
b2_sma_weibull <- fitdistr(branch2_sma, 'weibull')
qq_b2sma_weibull <- qweibull(ppoints(branch2_sma),b2_sma_weibull$estimate[1],b2_sma_weibull$estimate[2])
qqplot(qq_b2sma_weibull,branch2_sma)
lines(seq(0:200000),seq(0:200000))

## log-gamma
log_data <- log(branch2_sma)
b2_sma_lgamma <- fitdistr(log_data, 'gamma')
qq_b2sma_lgamma <- qgamma(ppoints(branch2_sma),shape = b2_sma_lgamma$estimate[1], rate = b2_sma_lgamma$estimate[2])
qqplot(exp(qq_b2sma_lgamma),branch2_sma)
lines(seq(1:200000),seq(1:200000))

sort_b2sma <- sort(branch2_sma)
plot(exp(qq_b2sma_lgamma),sort_b2sma, ylab = 'Theoretical quantiles', xlab = 'Sample quantiles', main = 'Branch 2')
lines(seq(0:200000),seq(0:200000))

## qq-plottar Alla
par(mfrow=c(1,2))
plot(qq_snorm1,sort_branch1s, ylab = 'Theoretical quantiles', xlab = 'Sample quantiles', main = 'Branch 1')
lines(seq(0:420000),seq(0:420000))
plot(qq_branch2_stora,sort_branch2_stora, ylab = 'Theoretical quantiles', xlab = 'Sample quantiles', main = 'Branch 2')
lines(seq(0:3500000),seq(0:3500000))
mtext('Large claims',side = 3,line = -1.5 ,cex = 1.5,outer = TRUE)


plot(qq_ln,sort_b1sma, ylab = 'Theoretical quantiles', xlab = 'Sample quantiles', main = 'Branch 1')
lines(seq(0:150000),seq(0:150000))
plot(exp(qq_b2sma_lgamma),sort_b2sma, ylab = 'Theoretical quantiles', xlab = 'Sample quantiles', main = 'Branch 2')
lines(seq(0:200000),seq(0:200000))
mtext('Small claims',side = 3,line = -1.5 ,cex = 1.5,outer = TRUE)


