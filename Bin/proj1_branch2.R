## BRANCH 2
numberclaims2 <- c()
for(i in 1:max(branch2$ClaimDay)){
  numberclaims2 <- c(numberclaims2,length(which(branch2$ClaimDay == i)))
}

rm(i)
rm(x1)

plot(numberclaims2,type = 'l', xlab = 'Time', ylab = 'X', main = 'Branch 2')

day <- c(1:3650)
myxts2 <- xts(numberclaims2, as.Date(day, origin='1970-01-01'))

claim_m2 <- apply.monthly(myxts2,FUN = sum) ## Skapar antalet claims per månad
plot(claim_m2)

rm(i)
rm(x1)

winterm2 <- c()
for(i in as.character(c(0:9))){
  x2 <- c(myxts2[paste0(':197',i,'-01'),],myxts2[paste0(':197',i,'-02'),],myxts2[paste0(':197',i,'-03'),],myxts2[paste0(':197',i,'-04'),],myxts2[paste0(':197',i,'-09'),],myxts2[paste0(':197',i,'-10'),],myxts2[paste0(':197',i,'-11'),],myxts2[paste0(':197',i,'-12'),])
  winterm2 <- c(winterm2,x2)
}

rm(i)
rm(x2)

## Plockar ut alla sommar-observationer (låg intensitet), konverteras om från xts till integer.
summerm2 <- c()
for(i in as.character(c(0:9))){
  x2 <- c(myxts2[paste0(':197',i,'-05'),],myxts2[paste0(':197',i,'-06'),],myxts2[paste0(':197',i,'-07'),],myxts2[paste0(':197',i,'-08'),])
  summerm2 <- c(summerm2,x2)
}

rm(i)
rm(x2)

hist(summerm2,20)
hist(winterm2,25)
acf(summerm2)
acf(winterm2)

### Beroende mellan brancher

plot(winterm,winterm2)
plot(summerm,summerm2)

cor.test(winterm,winterm2, method = 'spearman')
cor.test(winterm,winterm2, method = 'pearson')

cor.test(summerm,summerm2, method = 'spearman')
cor.test(summerm,summerm2, method = 'pearson')

### Fördelningsanpassning för branch 2 ###

negbin2 <- fitdistr(summerm2, 'negative binomial')

## kvantiler för modell

nn <- seq(from=0, to=1, by=c(1/1230))
qqnegbin2 <- qnbinom(p=nn, size= negbin2$estimate[1], mu=negbin2$estimate[2])
qqnegbin2 <- qqnegbin2[-1231]

## Kvantiler för data 
summerm2_sort<-sort(summerm2)
## qq-plot

plot(summerm2_sort,qqnegbin2)
lines(seq(0:13),seq(0:13))

rr <- rnbinom(n=1230,size=negbin2$estimate[1],mu=negbin2$estimate[2])
qqplot(summerm2,rr)

