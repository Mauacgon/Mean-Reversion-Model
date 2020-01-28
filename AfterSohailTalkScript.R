datagenic <- read.csv("C:/Users/D110148/OneDrive - pzem/Mijn Bureaublad/Modelos y Simulaciones/Excel/Location Spread.csv")
datagenic[datagenic == "NaN"] <- NA
datagenic <- datagenic[,1:2]
datagenic <- datagenic[,c(1,2)][!is.na(datagenic$Transport.spread),]


ma <- function(x, n = 5){
  filter(x, rep(1 / n, n), sides = 2)}

datagenic$ma <- ma(datagenic, 10)
datagenic$diff <- c(mean(diff(datagenic$Transport.spread)),
                    diff(datagenic$Transport.spread))
dgmean <- mean(datagenic$Transport.spread)
dgsd <- sd(datagenic$Transport.spread)
datagenic$ma <- ma(datagenic$Transport.spread, 10)
datagenic$upeer <- datagenic$ma + dgsd
datagenic$lower <- datagenic$ma - dgsd


##Exploratory data Analysis

plot(datagenic$Transport.spread, type = "l")
lines(datagenic$ma, col = "blue")
lines(datagenic$upeer, col = "red")
lines(datagenic$lower, col = "red")
lines(rep(mean(datagenic$Transport.spread), length(datagenic$ma)), col = "green")

par(mfrow = c(1,1))
hist1 <- hist(datagenic$Transport.spread, breaks = 50)
hist(datagenic$Transport.spread, breaks = 50)
multiplier1 <- unique(hist1$counts/hist1$density)[1]
seq1 <- seq(min(datagenic$Transport.spread),max(datagenic$Transport.spread),
            (max(datagenic$Transport.spread)-min(datagenic$Transport.spread))/50)
lines(x = seq1, y = dnorm(seq1, mean = dgmean, sd = dgsd)*multiplier1, lwd = 3, lty = 2)


hist2 <- hist(datagenic$diff, breaks = 50)
hist(datagenic$diff, breaks = 50)
multiplier2 <- unique(hist2$counts/hist2$density)[1]
seq2 <- seq(min(datagenic$diff),max(datagenic$diff),
            (max(datagenic$diff)-min(datagenic$diff))/50)
lines(x = seq1, y = dnorm(seq1, mean = mean(datagenic$diff),
                          sd = sd(datagenic$diff))*multiplier2, lwd = 3, lty = 2)

percent <- diff(datagenic$Transport.spread)/lag(zoo(datagenic$Transport.spread),-1, na.pad = TRUE)[-1]
hist3 <- hist(percent, breaks = 50)
multiplier3 <- unique(hist3$counts/hist3$density)[1]
seq3 <- seq(min(percent),max(percent),
            (max(percent)-min(percent))/50)
lines(x = seq3, y = dnorm(seq3, mean = mean(percent),
                          sd = sd(percent))*multiplier3, lwd = 3, lty = 2)



##Estimation of parameters

set.seed(10)

if(is.na(match("zoo", installed.packages()[,1]))) {
  install.packages("zoo")
}

require(zoo)

datagenic$dtmean <- datagenic$Transport.spread - dgmean

x <- zoo(datagenic$dtmean)

laggeddt <-  lag(x, -1, na.pad = TRUE)

tolmmodel <- as.data.frame(cbind(datagenic$diff, as.numeric(laggeddt)))
names(tolmmodel) <- c("diff", "dtmean")
tolmmodel <- tolmmodel[!is.na(tolmmodel$dtmean),]
beta <- cov(tolmmodel$diff,tolmmodel$dtmean)/var(tolmmodel$dtmean, na.rm = TRUE)

m <- mean(datagenic$diff)
dvol <- sd(datagenic$diff)

vectorsd <- rep(dvol,183)

# Read the forward curve

fcurve <- read.table("Fcurve.csv")

fcurve$V1 <- fcurve$V1

#Simulation of normals

d <- 210

l <- length(fcurve$V1)

rnormd <- as.data.frame(rnorm(l, mean = m, sd = vectorsd))

for (i in 1:(d+l))  {
  rnormd[,i] <- rnorm(l, mean = m, sd = vectorsd)
}

rnormd <- t(rnormd)

ssimulationss <- as.numeric(fcurve$V1)

ssimulation11 <- rbind(ssimulationss, matrix(0,d+l,l))

##For each column (each value of the forward curve)
for (i in seq_along(ssimulation11[1,])) {
  ##For each element of a whole column (each day since the valuation date)
  for (j in 1:(length(ssimulation11[,1])-1)) {
    ssimulation11[j+1,i] <- ssimulation11[j,i]+rnormd[j,i]+(beta*(ssimulation11[j,i]-dgmean))
  }
}

spayoffss <- pmax(ssimulation11,0)

spayoffss <- as.data.frame(spayoffss)



## Just to check volatilities ar correctly applied

plot(apply(spayoffss,2,max), type = "l")

##Turning it into a function

experiment2 <- function() 
{d <- 210

l <- length(fcurve$V1)

rnormd <- as.data.frame(rnorm(l, mean = m, sd = vectorsd))

for (i in 1:(d+l))  {
  rnormd[,i] <- rnorm(l, mean = m, sd = vectorsd)
}

rnormd <- t(rnormd)

ssimulationss <- as.numeric(fcurve$V1)

ssimulation11 <- rbind(ssimulationss, matrix(0,d+l,l))

##For each column (each value of the forward curve)
for (i in seq_along(ssimulation11[1,])) {
  ##For each element of a whole column (each day since the valuation date)
  for (j in 1:(length(ssimulation11[,1])-1)) {
    ssimulation11[j+1,i] <- ssimulation11[j,i]+rnormd[j,i]+(beta*(ssimulation11[j,i]-dgmean))
  }
}

spayoffss <- pmax(ssimulation11,0)

spayoffss <<- as.data.frame(spayoffss)

}

##Loop for 10.000 simulations

n <- 10000
smeanvectorr <- rep(0,l)
smeanmatrixx <- matrix(0,l,n)
stpoint <- dim(spayoffss)[1]-dim(spayoffss)[2]

for (i in 1:n) {
  experiment2()
  for (j in 1:l) {
    smeanvectorr[j] <- spayoffss[(j+stpoint),j]
  }
  smeanmatrixx[,i] <- smeanvectorr
  print(i)
  if (i>2) {
    print(mean(apply(smeanmatrixx[,1:i],1,mean)))
  }
}

##Plotting asymptotical effect

asymptotic <- rep(0,10000)

for (i in 1:length(meanmatrixx[1,])) {
  if (i>2) {
    asymptotic[i] <- mean(apply(meanmatrixx[,1:i],1,mean))
  } else {
    asymptotic[i] <- meanmatrixx[,i]
  }
}

plot(asymptotic[3:length(asymptotic)], type = "l", ylab = "Average value of the 183 options", 
     xlab = "Number of simulations", main = "ASYMPTOTIC EFFECT")
lines(rep(mean(apply(meanmatrixx,1,mean)), 10000), type = "l", col = "red")

##plotting every option value

plot(apply(meanmatrixx,1,mean), type = "h", main = "PAYOFFS TIME STRUCTURE", xlab = "MATURITY DATE",
     ylab ="VALUATION", col = "blue")
lines(apply(meanmatrixx,1,mean), type = "l", col = "blue")
lines(rep(mean(apply(meanmatrixx,1,mean)), 10000), type = "l", col = "red")
legend("topright", c("Option Value", "Mean"), fill = c("blue", "red"))

##Calculating the VaR
