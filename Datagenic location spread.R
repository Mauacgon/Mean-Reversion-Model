datagenic <- read.csv("C:/Users/D110148/OneDrive - pzem/Mijn Bureaublad/Modelos y Simulaciones/Location Spread.csv")
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
datagenic$upeer <- datagenic$ma + 0.4*dgsd
datagenic$lower <- datagenic$ma - 0.4*dgsd


##Exploratory data Analysis

plot(datagenic$Transport.spread, type = "l")
lines(datagenic$ma, col = "blue")
lines(datagenic$upeer, col = "red")
lines(datagenic$lower, col = "red")

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



##Modelization

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
  
  vectorsd <- seq(from = 0.52, to = 0.29, by = -(0.52-0.29)/182)
  
  vectorbetas <- seq(from = -0.5020259, to = beta, by = (0.5020259+beta)/182)
  
  rnorm <- as.data.frame(rnorm(183, mean = m, sd = vectorsd))
  
  
  
  for (i in 1:1000)  {
    rnorm[,i] <- rnorm(183, mean = m, sd = vectorsd)
  }
  
  simulations <- rep(0.01083709, 183)
  
  ###1000 simulations
  
  vector <- simulations
  
  for (j in 1:length(names(rnorm))) {
    
    for (i in 1:length(rnorm$V2)) {
      simulations[i+1] <- simulations[i]+rnorm[i,j]+(vectorbetas[i]*(simulations[i]-dgmean))
    }
    vector <- cbind(vector, simulations)
  }  
  
  vector <- as.data.frame(vector)
  names(vector) <- c(1:1001)
  
  plot(vector$`500`, type = "l", col = "red", ylim = c(-3,3))
  
  lines(datagenic$Transport.spread)
  
  ## Vector valuation
  
  vector <- as.matrix(vector[1:183,])
  
  payoffs <<- pmax(vector,0)
  
  value <<- mean(apply(payoffs, 1, mean))
  
  outcome <- payoffs[183,]
  
  
  value

##Plot analysis

par(mfcol = c(1,2))

plot(vector$`1`, type = "l", ylim = c(-3,3), xlab = "Time", ylab = "Spread"
     , main = "SPREAD PATHS")
for(i in 2:length(names(vector))) {
  lines(vector[,i], type = "l", col = "black")
}

plot(payoffs$`1`, type = "l", ylim = c(-3, 3), xlab = "Time", ylab = "Payoff",
     main = "PAYOFFS PATHS")
for(i in 2:length(names(payoffs))) {
  lines(payoffs[,i], type = "l", col = "black")
}


par(mfrow = c(1,2))

plot(payoffs$`1`, type = "l", ylim = c(-0.5, 2), xlab = "Time", ylab = "Payoff",
     main = "PAYOFFS PATHS",)
for(i in 2:length(names(payoffs))) {
  lines(payoffs[,i], type = "l", col = "black")
}

points(x = length(payoffs$`1`), y = value, type = "p", pch = 19, col = "red")


hist(as.numeric(payoffs[length(payoffs$`1`),]), main = "SUM OF PAYOFF SIMULATIONS", xlab = "PAYOFFS")
abline(v=value, col = "red")




