library(xtable)

###################################
## Aureliano case
###################################

data14 <- read.csv("~/Downloads/case14 (1).csv",
stringsAsFactors=FALSE, header=T) ## strings as factors fine today
head(data14)
dim(data14)
str(data14)

##########################################################################
## Preliminary analysis
##########################################################################

par(mfrow=c(2,2))
boxplot(data14[,1])
hist(data14[,1])

boxplot(log(data14[,1]))
hist(log(data14[,1]))

logprice <- log(data14[,1])

summary(lm(logprice ~ data14[,2] + data14[,3] + data14[,4] +
               data14[,5] + data14[,6] + data14[,7] + data14[,8] +
               data14[,9] + data14[,10]+ data14[,11] + data14[,12] +
               data14[,13] + data14[,14] + data14[,15] + data14[,16] +
               data14[,17] + data14[,18] + data14[,19] + data14[,20] +
               data14[,21] + data14[,22] + data14[,23] +  data14[,24] +
               data14[,25]))

summary(lm(logprice ~ data14[,2] + data14[,3] + data14[,4] +
               data14[,5] + data14[,6] + data14[,7] + data14[,8] +
               data14[,9] + data14[,10]+ data14[,11] + data14[,12] +
               data14[,13] + data14[,14] + data14[,16] +
               data14[,17] + data14[,18] + data14[,19] + data14[,20] +
               data14[,21] + data14[,22] + data14[,23] +  data14[,24] +
               data14[,25]))

summary(lm(data14[,1] ~ data14[,2] + data14[,3] + data14[,4] +
               data14[,5] + data14[,6] + data14[,7] + data14[,8] +
               data14[,9] + data14[,10]+ data14[,11] + data14[,12] +
               data14[,13] + data14[,15] + data14[,16] +
               data14[,17] + data14[,18] + data14[,19] + data14[,20] +
               data14[,21] + data14[,22] + data14[,23] +  data14[,24] +
               data14[,25]))

############################################

regfit102 <- lm(logprice ~ data14[,2] + data14[,3] + data14[,4] +
               data14[,5] + data14[,6] + data14[,7] + data14[,8] +
               data14[,9] + data14[,10]+ data14[,11] + data14[,12] +
               data14[,13] + data14[,15] + data14[,16] +
               data14[,17] + data14[,18] + data14[,19] + data14[,20] +
               data14[,21] + data14[,22] + data14[,23] +  data14[,24] +
               data14[,25])

coefsreg102 <- summary(regfit102)$coef

plot(coefsreg102[,4])

## We compare a new house, to a non-new house
## log(P_old) ----> log(P_old) + 0.139
## e^(log(P_old)) ---->  e^(log(P_old) + 0.139)
## P_old ---->  e^(log(P_old)) e^(+ 0.139)
## P_old ---->  P_old e^(0.139)

## Checking out the variables
pdf("graph20191017.pdf", wid=12, hei=9)
par(mfrow=c(2,3))
for(i in 1:24){
    plot(data14[,i], main=names(data14)[i])
}
dev.off()


plot(data14[,12] + data14[,13] + data14[,14], data14[,15]) ## Collinearity
boxplot(logprice ~ data14[,4])

##########################################################################
## Let's construct all our independent variables.
## I am being very careful, lots of non-trivial calls, I am going with the
## default protocol of defining all factors as 1/0s (Bernoullis), keeping
## all numeric data (int) as scalars (questionable protocol).
##########################################################################

independentvar <- matrix(0, dim(data14)[1], 0)

independentvar <- cbind(independentvar, data14[,2])
colnames(independentvar)[1] <- "LotFrontage"

slope1 <- rep(0, dim(data14)[1])
slope1[data14[,3]=="Gtl"] <- 1
slope2 <- rep(0, dim(data14)[1])
slope2[data14[,3]=="Mod"] <- 1
slope3 <- rep(0, dim(data14)[1])
slope3[data14[,3]=="Sev"] <- 1
independentvar <- cbind(independentvar, slope1, slope2)

style1 <- rep(0, dim(data14)[1])
style1[data14[,4]=="1.5Fin"] <- 1
style2 <- rep(0, dim(data14)[1])
style2[data14[,4]=="1.5Unf"] <- 1
style3 <- rep(0, dim(data14)[1])
style3[data14[,4]=="1Story"] <- 1
style4 <- rep(0, dim(data14)[1])
style4[data14[,4]=="2.5Fin"] <- 1
style5 <- rep(0, dim(data14)[1])
style5[data14[,4]=="2.5Unf"] <- 1
style6 <- rep(0, dim(data14)[1])
style6[data14[,4]=="2Story"] <- 1
style7 <- rep(0, dim(data14)[1])
style7[data14[,4]=="SFoyer"] <- 1
style8 <- rep(0, dim(data14)[1])
style8[data14[,4]=="SLvl"] <- 1
independentvar <- cbind(independentvar, style1, style2, style3, style4, style5, style6, style7)

independentvar <- cbind(independentvar, data14[,5])
independentvar <- cbind(independentvar, data14[,6])

colnames(independentvar)[11] <- "OverallQual"
colnames(independentvar)[12] <- "OverallCond"

foundation1 <- rep(0, dim(data14)[1])
foundation1[data14[,7]=="BrkTil"] <- 1
foundation2 <- rep(0, dim(data14)[1])
foundation2[data14[,7]=="CBlock"] <- 1
foundation3 <- rep(0, dim(data14)[1])
foundation3[data14[,7]=="PConc"] <- 1
foundation4 <- rep(0, dim(data14)[1])
foundation4[data14[,7]=="Slab"] <- 1
foundation5 <- rep(0, dim(data14)[1])
foundation5[data14[,7]=="Stone"] <- 1
foundation6 <- rep(0, dim(data14)[1])
foundation6[data14[,7]=="Wood"] <- 1
independentvar <- cbind(independentvar, foundation1, foundation2, foundation3, foundation4, foundation5)

basement1 <- rep(0, dim(data14)[1])
basement1[data14[,8]=="Ex"] <- 1
basement2 <- rep(0, dim(data14)[1])
basement2[data14[,8]=="Fa"] <- 1
basement3 <- rep(0, dim(data14)[1])
basement3[data14[,8]=="Gd"] <- 1
basement4 <- rep(0, dim(data14)[1])
basement4[data14[,8]=="TA"] <- 1
independentvar <- cbind(independentvar, basement1, basement2, basement3, basement4)
## What should we do about NAs?

independentvar <- cbind(independentvar, data14[,9])
colnames(independentvar)[22] <- "TotalBsmtSF"

heating1 <- rep(0, dim(data14)[1])
heating1[data14[,10]=="Ex"] <- 1
heating2 <- rep(0, dim(data14)[1])
heating2[data14[,10]=="Fa"] <- 1
heating3 <- rep(0, dim(data14)[1])
heating3[data14[,10]=="Gd"] <- 1
heating4 <- rep(0, dim(data14)[1])
heating4[data14[,10]=="Po"] <- 1
heating5 <- rep(0, dim(data14)[1])
heating5[data14[,10]=="TA"] <- 1
independentvar <- cbind(independentvar, heating1, heating2, heating3, heating4)

centralair1 <- rep(0, dim(data14)[1])
centralair1[data14[,11]=="Y"] <- 1
centralair2 <- rep(0, dim(data14)[1])
centralair2[data14[,11]=="N"] <- 1
independentvar <- cbind(independentvar, centralair1)

independentvar <- cbind(independentvar, data14[,12])
independentvar <- cbind(independentvar, data14[,13])
independentvar <- cbind(independentvar, data14[,14])
independentvar <- cbind(independentvar, data14[,16]) ## 15 collinear with 12:14
independentvar <- cbind(independentvar, data14[,17])
independentvar <- cbind(independentvar, data14[,18])
independentvar <- cbind(independentvar, data14[,19])

colnames(independentvar)[28] <- "X1stFlrSF"
colnames(independentvar)[29] <- "X2ndFlrSF"
colnames(independentvar)[30] <- "LowQualFinSF"
colnames(independentvar)[31] <- "BsmtFullBath"
colnames(independentvar)[32] <- "TotRmsAbvGrd"
colnames(independentvar)[33] <- "GarageCars"
colnames(independentvar)[34] <- "GarageArea"

garage1 <- rep(0, dim(data14)[1])
garage1[data14[,20]=="Ex"] <- 1
garage2 <- rep(0, dim(data14)[1])
garage2[data14[,20]=="Fa"] <- 1
garage3 <- rep(0, dim(data14)[1])
garage3[data14[,20]=="Gd"] <- 1
garage4 <- rep(0, dim(data14)[1])
garage4[data14[,20]=="Po"] <- 1
garage5 <- rep(0, dim(data14)[1])
garage5[data14[,20]=="TA"] <- 1
independentvar <- cbind(independentvar, garage1, garage2, garage3, garage4, garage5)

drivepave1 <- rep(0, dim(data14)[1])
drivepave1[data14[,21]=="Y"] <- 1
drivepave2 <- rep(0, dim(data14)[1])
drivepave2[data14[,21]=="N"] <- 1
drivepave3 <- rep(0, dim(data14)[1])
drivepave3[data14[,21]=="P"] <- 1
independentvar <- cbind(independentvar, drivepave1, drivepave2)

independentvar <- cbind(independentvar, data14[,22])
independentvar <- cbind(independentvar, data14[,23])
independentvar <- cbind(independentvar, data14[,24])

colnames(independentvar)[42] <- "OpenPorchSF"
colnames(independentvar)[43] <- "MoSold" ## Probably should be treated categorically
colnames(independentvar)[44] <- "YrSold" ## Probably should be treated categorically

saletype1 <- rep(0, dim(data14)[1])
saletype1[data14[,25]=="COD"] <- 1
saletype2 <- rep(0, dim(data14)[1])
saletype2[data14[,25]=="Con"] <- 1
saletype3 <- rep(0, dim(data14)[1])
saletype3[data14[,25]=="ConLD"] <- 1
saletype4 <- rep(0, dim(data14)[1])
saletype4[data14[,25]=="ConLI"] <- 1
saletype5 <- rep(0, dim(data14)[1])
saletype5[data14[,25]=="ConLw"] <- 1
saletype6 <- rep(0, dim(data14)[1])
saletype6[data14[,25]=="CWD"] <- 1
saletype7 <- rep(0, dim(data14)[1])
saletype7[data14[,25]=="New"] <- 1
saletype8 <- rep(0, dim(data14)[1])
saletype8[data14[,25]=="Oth"] <- 1
saletype9 <- rep(0, dim(data14)[1])
saletype9[data14[,25]=="WD"] <- 1
independentvar <- cbind(independentvar, saletype1, saletype2, saletype3, saletype4, saletype5, saletype6, saletype7, saletype8)

#####################################################################
## Choosing variables v1.0
## We start with all of them, then start dropping them
## according to the ones that have the smallest improvement
## in adjusted-R^2.
## Note that the model stops when it has 20 observations (fixed, arbitrary).
#####################################################################

independentvarnew <- independentvar

while(dim(independentvarnew)[2]>10){
    colsme <- dim(independentvarnew)[2]
    saversquare <- rep(0, dim(independentvarnew)[2])
    saversquare0 <- summary(lm(logprice ~ independentvarnew[,]))$adj

    for(i in 1:dim(independentvarnew)[2]){
        dometimes <- dim(independentvarnew)[2]
        saversquare[i] <- summary(lm(logprice ~ independentvarnew[,-i]))$adj
    }
    diffme <- saversquare0 - saversquare
    dropme <- !min(diffme)==(saversquare0 - saversquare)
    independentvarnew <- independentvarnew[,dropme]
}
summary(lm(logprice ~ independentvarnew))

xtable(summary(lm(logprice ~ independentvarnew)))

#####################################################################
## Choosing variables v2.0
## We start with all of them, then start dropping them
## according to the ones that have the largest p-value
#####################################################################

independentvarnew <- independentvar
maxpval <- 1
cutoff <- 0.01


while(maxpval>cutoff){
    regcoefs <- summary(lm(logprice ~ independentvarnew[,]))$coef
    maxpval <- max(regcoefs[,4])
    if(maxpval>cutoff){
        dropme <- !regcoefs[,4] == maxpval
        independentvarnew <- independentvarnew[,dropme[2:length(dropme)]]
    }
}
summary(lm(logprice ~ independentvarnew))
