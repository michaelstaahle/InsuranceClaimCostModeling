mydata = read.table("~/Riskmodeller, försäkring/Projekt1_Grupp6.txt", header = TRUE, sep = ";")

library('MASS')
library('xts')
library('zoo')
library('dplyr')
branch1 <- filter(mydata, ClaimType==1)
branch2 <- filter(mydata, ClaimType==2)

plot(branch1$ClaimDay, type='l')
hist(branch1$ClaimDay,30002)

plot(branch2$ClaimDay, type='l')
hist(branch2$ClaimDay,18785)

par(mfrow=c(1,2))
plot(branch1$ClaimCost, ylab = 'Claim size', xlab = 'Time', main = 'Branch 1')
plot(branch2$ClaimCost, ylab = 'Claim size', xlab = 'Time', main = 'Branch 2')

summary(branch1$ClaimDay)

###BRANCH 1###

## Antal claims per dag
numberclaims <- c()
for(i in 1:max(branch1$ClaimDay)){
  numberclaims <- c(numberclaims,length(which(branch1$ClaimDay == i)))
}


hist(numberclaims,50)

plot(numberclaims, type = 'l', xlab='Tme', ylab='X', main = 'Branch 1')


## Average claim size per day
Agg.ClaimDay <- aggregate(ClaimCost~ClaimDay,FUN=mean,data=branch1)

## Average claim size per månad 
xbarc <- c()
for(i in 0:117){
  x <- Agg.ClaimDay$ClaimCost[c((1+30*i):(30+30*i))]
  xbarc <- c(xbarc,mean(x))
} 

plot(xbarc,type='l')
plot(Agg.ClaimDay,type='l')


## skapar en tidsserie (xts-objekt) för antalet claims
day <- c(1:3650)
myxts <- xts(numberclaims, as.Date(day))

claim_m <- apply.monthly(myxts,FUN = sum) ## Skapar antalet claims per månad
plot(claim_m)



claim_w <- apply.weekly(myxts,FUN = sum)

## Plockar ut alla vinter-observationer (hög intensitet), konverteras om från xts till integer.
winterm <- c()
for(i in as.character(c(0:9))){
x1 <- c(myxts[paste0(':197',i,'-01'),],myxts[paste0(':197',i,'-02'),],myxts[paste0(':197',i,'-03'),],myxts[paste0(':197',i,'-04'),],myxts[paste0(':197',i,'-09'),],myxts[paste0(':197',i,'-10'),],myxts[paste0(':197',i,'-11'),],myxts[paste0(':197',i,'-12'),])
winterm <- c(winterm,x1)
}

rm(i)
rm(x1)

hist(winterm,30)
mean(winterm)
var(winterm)

## Plockar ut alla sommar-observationer (låg intensitet), konverteras om från xts till integer.
summerm <- c()
for(i in as.character(c(0:9))){
  x1 <- c(myxts[paste0(':197',i,'-05'),],myxts[paste0(':197',i,'-06'),],myxts[paste0(':197',i,'-07'),],myxts[paste0(':197',i,'-08'),])
  summerm <- c(summerm,x1)
}

rm(i)
rm(x1)

hist(summerm,40)
mean(summerm)
var(summerm)

x1 <- rpois(1230,mean(summerm))
hist(x1,20)

x2 <- rpois(2420,mean(winterm))
hist(x2,30)


plot(xbar,type='l')

# Skatta lambda genom att ta mean arrival för januari over de tio åren, februari över tio åren etc. 
# Kontrollera för time stationarity genom att lägga histogram över arrival dist årsvis.
# Kontrollera för poissonfördelning genom qq-plot över all data från låg-intensitets-månaderna respektive hög-intensitets-månaderna
# Kontrollera även att väntevärde och varians är samma, annars eventuell överspridning vilket leder till betingad poisson, negativ binomial.

## sommarmånader ##
## Anpassa negbin till antal claims per dag summerm

negbin <- fitdistr(summerm, 'negative binomial')

## kvantiler för modell

nn <- seq(from=0, to=1, by=c(1/1230))
qqnegbin <- qnbinom(p=nn, size= negbin$estimate[1], mu=negbin$estimate[2])
qqnegbin <- qqnegbin[-1231]

## kvantiler för data

summerm_sort <- sort(summerm)

## qq-plot
plot(summerm_sort, qqnegbin)
lines(seq(0:14),seq(0:14),type='l')

qqplot(qqnegbin,summerm, type='l')

## Vintermånader ##
###################

## Anpassa negbin till antal claims per dag vinterm

negbinv <- fitdistr(winterm, 'negative binomial')

## kvantiler för 1230

nnv <- seq(from=0, to=1, by=c(1/2420))
qqnegbinv <- qnbinom(p=nnv, size= negbinv$estimate[1], mu=negbinv$estimate[2])
qqnegbinv <- qqnegbinv[-2421]

## kvantiler för data

vinterm_sort <- sort(winterm)

plot(vinterm_sort, qqnegbinv)

qqplot(qqnegbinv,winterm)
lines(seq(0:35),seq(0:35),type='l')

## qq-plot sommer med pois()
pois_s <- fitdistr(winterm,'poisson')

qqpois <- qpois(nn, 2.830894)
qqpois <- qqpois[-1231]
plot(summerm_sort,qqpois)
lines(seq(0:14),seq(0:14),type='l')

### Tidsberoende###
acf(summerm)
acf(winterm)

acf()

Box.test(summerm)
Box.test(winterm)

acf(summerm2)
acf(winterm2)

Box.test(summerm2)
## summer branch 1
sort_summerm <- sort(summerm)
qqpois_s <- qpois(ppoints(sort_summerm),mean(summerm))
plot(sort_summerm,qqpois_s)
lines(seq(0:15),seq(0:15))

## winter branch 1
sort_winterm <- sort(winterm)
qqpois_w <- qpois(ppoints(sort_winterm),mean(winterm))
plot(sort_winterm,qqpois_w)
lines(seq(0:30),seq(0:30))

## summer branch 2
sort_summerm2 <- sort(summerm2)
qqpois_s2 <- qpois(ppoints(sort_summerm2),mean(summerm2))
plot(sort_summerm2,qqpois_s2)
lines(seq(0:10),seq(0:10))


## winter branch 2
sort_winterm2 <- sort(winterm2)
qqpois_w2 <- qpois(ppoints(sort_winterm2),mean(winterm2))
plot(sort_winterm2,qqpois_w2)
lines(seq(0:30),seq(0:30))

## Plottar alla qq

par(mfrow=c(2,2))
plot(sort_summerm,qqpois_s, ylab = 'Theoretical quantiles', xlab = 'Sample quantiles', main = 'Branch 1, low intensity')
lines(seq(0:15),seq(0:15))

plot(sort_winterm,qqpois_w, ylab = 'Theoretical quantiles', xlab = 'Sample quantiles', main = 'Branch 1, high intensity')
lines(seq(0:30),seq(0:30))

plot(sort_summerm2,qqpois_s2, ylab = 'Theoretical quantiles', xlab = 'Sample quantiles', main = 'Branch 2, low intensity')
lines(seq(0:10),seq(0:10))

plot(sort_winterm2,qqpois_w2, ylab = 'Theoretical quantiles', xlab = 'Sample quantiles', main = 'Branch 2, high intensity')
lines(seq(0:30),seq(0:30))


coef_s1 <- fitdistr(summermm,'negative binomial')
coef_w1 <- fitdistr(wintermm, 'negative binomial')
coef_s2 <- fitdistr(summermm2, 'negative binomial')
coef_w2 <- fitdistr(wintermm2,'negative binomial')

## summer branch 1
sort_summermm <- sort(summermm)
qqnegbin_s <- qnbinom(ppoints(sort_summermm),size = coef_s1$estimate[1],mu=coef_s1$estimate[2])
plot(sort_summermm,qqnegbin_s)
lines(seq(0:120),seq(0:120))

## winter branch 1
sort_wintermm <- sort(wintermm)
qqnegbin_w <- qnbinom(ppoints(sort_wintermm),size = coef_w1$estimate[1],mu=coef_w1$estimate[2])
plot(sort_wintermm,qqnegbin_w)
lines(seq(0:400),seq(0:400))

## summer branch 2
sort_summermm2 <- sort(summermm2)
qqnegbin_s2 <- qnbinom(ppoints(sort_summermm2),size = coef_s2$estimate[1],mu=coef_s2$estimate[2])
plot(sort_summermm2,qqnegbin_s2)
lines(seq(0:80),seq(0:80))


## winter branch 2
sort_wintermm2 <- sort(wintermm2)
qqnegbin_w2<- qnbinom(ppoints(sort_wintermm2),size = coef_w2$estimate[1],mu=coef_w2$estimate[2])
plot(sort_wintermm2,qqnegbin_w2)
lines(seq(0:270),seq(0:270))

sort_summermm <- sort(summermm)
qqnbin_s1 <- qnbinom()


## Beroende mellan brancher
par(mfrow=c(1,2))
plot(summermm,summermm2, ylab = 'Branch 2', xlab = 'Branch 1', main = 'Low intensity')
plot(wintermm,wintermm2, ylab = 'Branch 2', xlab = 'Branch 1', main = 'High intensity')
