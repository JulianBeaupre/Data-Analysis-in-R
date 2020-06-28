####################################################################################
## Problem 1
####################################################################################
nosimula <- 1000
barx <- rep(0, nosimula)
ene <- 10

for(i in 1:nosimula){
    simula <- rbinom(ene, 3, 0.6)
    barx[i] <- mean(simula)
}

hist(barx, ncla=20)
boxplot(barx)

mean(barx)
median(barx)
## Quantifying symmetry, start with larger sample
nosimula <- 40000
barx <- rep(0, nosimula)
ene <- 10

for(i in 1:nosimula){
    simula <- rbinom(ene, 3, 0.6)
    barx[i] <- mean(simula)
}

hist(barx, ncla=20)
boxplot(barx)
meanbarx <- mean(barx)

bandme <- 0.11
sum(barx>1.8 & barx<=1.8+bandme)/nosimula
sum(barx<1.8 & barx>=1.8-bandme)/nosimula

## Discrete nature of our sample space allows to be
## much more precise
summary(as.factor(barx))/nosimula
hist(barx, ncla=200, freq=FALSE)

## We can do our own histogram here.
## Note that we could also plot the actual finite sample distribution in this case
## (binomial with n=30, etc). Try it!?
probsemp <- summary(as.factor(barx))/nosimula
plot(as.numeric(names(probsemp)), probsemp, xlab='Sample space', ylab='Probabilities')
for(i in 1:20){abline(h=i/100, lty=2, lwd=.5)}

pdf('graph20190908a.pdf', hei=9, wid=12)
probsemp <- summary(as.factor(barx))/nosimula
plot(as.numeric(names(probsemp)), probsemp, xlab='Sample space', ylab='Probabilities')
for(i in 1:20){abline(h=i/100, lty=2, lwd=.5)}
dev.off()


####################################################################################
## Problem 2
####################################################################################

nosimula <- 10000
barx <- rep(0, nosimula)
bars <- rep(0, nosimula)
ene <- 10

for(i in 1:nosimula){
    simula <- rnorm(ene, 0, 1)
    barx[i] <- mean(simula)
    bars[i] <- sd(simula)
}

par(mfrow=c(1,2))
hist(barx, ncla=20, freq=FALSE)
hist(bars, ncla=20, freq=FALSE)

boxplot(barx, ncla=20, freq=FALSE, main='Bar X')
boxplot(bars, ncla=20, freq=FALSE, main='Bar S')

pdf('graph20190908b.pdf', hei=9, wid=12)
par(mfrow=c(1,2))
hist(barx, ncla=20, freq=FALSE)
hist(bars, ncla=20, freq=FALSE)
dev.off()

####################################################################################
## Problem 3
####################################################################################

## Initialize parameters
nosimula <- 10000
par1 <- 10
par2 <- 10

## Generate random variables
simula1 <- rnorm(nosimula, 0, 1)
simula2 <- rchisq(nosimula, par1)
simula3 <- rchisq(nosimula, par2)

rv1 <- simula1/sqrt(simula2/par1)
rv2 <- simula2/simula3

par(mfrow=c(1,2))
hist(rv1, ncla=20, freq=FALSE, main="Histogram of random variable 1")
hist(rv2, ncla=20, freq=FALSE, main="Histogram of random variable 2")

x <- seq(-5,5, len=1000)

par(mfrow=c(1,1))
hist(rv1, ncla=20, freq=FALSE)
lines(x, dt(x, 9))

x <- seq(0,10, len=1000)

par(mfrow=c(1,1))
hist(rv2, ncla=20, freq=FALSE)
lines(x, df(x, par1, par2))


####################################################################################
## Problem 4
####################################################################################

pokersim <- function(x){
    ###############################################
    ## Define cards
    ###############################################
    ene <- x/4
    set1 <- rep(1:ene, 4)
    set2 <- c(rep(1, ene), rep(2, ene), rep(3, ene), rep(4, ene))
    ###############################################
    ## Pick 5 random numbers from our deck
    ###############################################
    rvsam <- sample(1:(ene*4), 5)
    hand1 <- set1[rvsam]
    hand2 <- hand1[order(hand1)]
    
    ###############################################
    ## Dealing with the flush
    ###############################################
    
    flush <- 0
    suit1 <- set2[rvsam[1]] - set2[rvsam[2]]
    suit2 <- set2[rvsam[2]] - set2[rvsam[3]]
    suit3 <- set2[rvsam[3]] - set2[rvsam[4]]
    suit4 <- set2[rvsam[4]] - set2[rvsam[5]]
    if(suit1 ==0 & suit2 ==0 & suit3 ==0 & suit4 ==0){
        flush <- 1
    }

    ###############################################
    ## Dealing with the straight
    ###############################################
    
    straight <- 0
    condition1 <- (hand2[2]-hand2[1]==1) & (hand2[3]-hand2[2]==1) & (hand2[4]-hand2[3]==1) & (hand2[5]-hand2[4]==1)
    condition2 <- (hand2[5]==ene) & (hand2[4]==ene-1) & (hand2[3]==ene-2) & (hand2[2]==ene-3) & (hand2[1]==1)
    if(condition1 | condition2){
        straight <- 1
    }

    ###############################################
    ## Dealing with the full house
    ###############################################
    
    full <- 0
    condition1 <- (hand2[2]-hand2[1]==0) & (hand2[3]-hand2[2]==0) & (hand2[4]-hand2[5]==0)
    condition2 <- (hand2[2]-hand2[1]==0) & (hand2[4]-hand2[3]==0) & (hand2[4]-hand2[5]==0)
    if(condition1 | condition2){
        full <- 1
    }

    ###############################################
    ## Dealing with three-of-a-kind
    ###############################################
    
    three <- 0
    condition1 <- (hand2[2]-hand2[1]==0) & (hand2[3]-hand2[2]==0) & !(hand2[4]-hand2[3]==0) & !(hand2[5]-hand2[4]==0)
    condition2 <- !(hand2[2]-hand2[1]==0) & (hand2[3]-hand2[2]==0) & (hand2[4]-hand2[3]==0) & !(hand2[5]-hand2[4]==0)
    condition3 <- !(hand2[2]-hand2[1]==0) & !(hand2[3]-hand2[2]==0) & (hand2[4]-hand2[3]==0) & (hand2[5]-hand2[4]==0)

    if(condition1 | condition2 | condition3){
        three <- 1
    }

    
    ###############################################
    ## Output
    ###############################################

    outputdg <- c(set1[rvsam], suit[rvsam], flush, straight, full, three)
    return(outputdg)
}

###############################################
## Run one simulation
###############################################
simula <- 100000
threeout <- fullout <- straightout <- flushout <- rep(0, simula)
for(i in 1:simula){
    flushout[i] <- pokersim(52)[11]
    straightout[i] <- pokersim(52)[12]
    fullout[i] <- pokersim(52)[13]
    threeout[i] <- pokersim(52)[14]
}
sum(flushout)/simula
sum(straightout)/simula
sum(fullout)/simula
sum(threeout)/simula

###############################################
## Run multiple simulations
###############################################
noofsim <- 100
stat1 <- stat2 <- stat3 <- stat4 <- rep(0, noofsim)

for(j in 1:noofsim){
    cat("Simulation", j, "\n")
    simula <- 10000
    threeout <- fullout <- straightout <- flushout <- rep(0, simula)
    for(i in 1:simula){
        flushout[i] <- pokersim(52)[11]
        straightout[i] <- pokersim(52)[12]
        fullout[i] <- pokersim(52)[13]
        threeout[i] <- pokersim(52)[14]
    }
    stat1[j] <- sum(flushout)/simula
    stat2[j] <- sum(straightout)/simula
    stat3[j] <- sum(fullout)/simula
    stat4[j] <- sum(threeout)/simula
}

stat1
par(mfrow=c(1,4))
boxplot(stat1, main="Flush")
boxplot(stat2, main="Straight")
boxplot(stat3, main="Full house")
boxplot(stat4, main="Three-of-a-kind")

pdf('graph20190908c.pdf', hei=9, wid=15)
par(mfrow=c(1,4))
boxplot(stat1, main="Flush")
boxplot(stat2, main="Straight")
boxplot(stat3, main="Full house")
boxplot(stat4, main="Three-of-a-kind")
dev.off()
