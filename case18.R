library(foreign)
library(sandwich)
library(stargazer)
library(lmtest)
library(AER)

#######################################################
## Case question 1
#######################################################

CASchools <- read.csv("~/Desktop/case18.csv",
stringsAsFactors=FALSE, header=T)
head(CASchools)
head(CASchools)
dim(CASchools)

model01 <- lm(CASchools$testscr ~ CASchools$calw_pct)
summary(model01)
model02 <- lm(CASchools$testscr ~ CASchools$meal_pct)
summary(model02)
model03 <- lm(CASchools$testscr ~ CASchools$el_pct)
summary(model03)
model04 <- lm(CASchools$testscr ~ CASchools$calw_pct + CASchools$meal_pct)
summary(model04)
model05 <- lm(CASchools$testscr ~ CASchools$calw_pct + CASchools$el_pct)
summary(model05)
model06 <- lm(CASchools$testscr ~ CASchools$meal_pct + CASchools$el_pct)
summary(model06)
model07 <- lm(CASchools$testscr ~ CASchools$calw_pct + CASchools$meal_pct + CASchools$el_pct)
summary(model07)


table07 <- matrix(0, 7, 6)

table07[1,1:2] <- summary(model01)$coef[2,c(1,3)]
table07[2,3:4] <- summary(model02)$coef[2,c(1,3)]
table07[3,5:6] <- summary(model03)$coef[2,c(1,3)]
table07[4,1:2] <- summary(model04)$coef[2,c(1,3)]
table07[4,3:4] <- summary(model04)$coef[3,c(1,3)]
table07[5,1:2] <- summary(model05)$coef[2,c(1,3)]
table07[5,5:6] <- summary(model05)$coef[3,c(1,3)]
table07[6,3:4] <- summary(model06)$coef[2,c(1,3)]
table07[6,5:6] <- summary(model06)$coef[3,c(1,3)]
table07[7,1:2] <- summary(model07)$coef[2,c(1,3)]
table07[7,3:4] <- summary(model07)$coef[3,c(1,3)]
table07[7,5:6] <- summary(model07)$coef[4,c(1,3)]

################################################
## Case question 2
################################################

str(CASchools)
hist(CASchools$testscr)
hist(CASchools$enrl_tot)
hist(CASchools$avginc)

model11 <- lm(CASchools$testscr ~ CASchools$calw_pct + CASchools$meal_pct + CASchools$el_pct
              + log(CASchools$enrl_tot) + CASchools$comp_stu + CASchools$expn_stu + log(CASchools$avginc) + CASchools$str)

summary(model11)


model12 <- lm(CASchools$testscr ~ CASchools$meal_pct + CASchools$el_pct
              + CASchools$comp_stu + CASchools$expn_stu + log(CASchools$avginc))

summary(model12)

model13 <- lm(CASchools$testscr ~ CASchools$meal_pct + CASchools$el_pct
              + CASchools$comp_stu + CASchools$expn_stu + log(CASchools$avginc) + CASchools$county)

summary(model13)

anova(model12, model13)

summary(model12)$coef[2:5,]
summary(model13)$coef[2:5,]

xtable(summary(model12)$coef[2:5,])
xtable(summary(model13)$coef[2:5,])

linearHypothesis(model12, c("CASchools$el_pct=0", "CASchools$comp_stu=0"))

################################################
## Case question 3
################################################

coeftest(model12)
coeftest(model12, vcov=vcovHC(model12, type='HC1'))
coeftest(model12, vcov=vcovHC(model12, type='HC3'))
coeftest(model12, vcov=vcovCL(model12, cluster = CASchools$county))

coeftest(model13)[2:6,]
coeftest(model13, vcov=vcovCL(model13, cluster = CASchools$county))[2:6,]

par(mfrow=c(2,2))
plot(model12)
plot(model13)


linearHypothesis(model12, c("CASchools$el_pct=0", "CASchools$expn_stu=0"))
linearHypothesis(model12, c("CASchools$el_pct=0", "CASchools$expn_stu=0"),
                 vcov = vcovCL(model12, cluster = CASchools$county))


xtable(coeftest(model13)[2:6,])
xtable(coeftest(model13, vcov=vcovCL(model13, cluster = CASchools$county))[2:6,])

##############################################################################################################
## From https://www.econometrics-with-r.org/7-6-analysis-of-the-test-score-data-set.html
##############################################################################################################

m <- rbind(c(1, 2), c(3, 0))
graphics::layout(mat = m)

# scatterplots
plot(math_scr ~ el_pct, 
     data = CASchools, 
     col = "steelblue", 
     pch = 20, 
     xlim = c(0, 100),
     cex.main = 0.9,
     main = "Percentage of English language learners")

plot(math_scr ~ meal_pct, 
     data = CASchools, 
     col = "steelblue", 
     pch = 20,
     cex.main = 0.9,
     main = "Percentage qualifying for reduced price lunch")

plot(math_scr ~ calw_pct, 
     data = CASchools, 
     col = "steelblue", 
     pch = 20, 
     xlim = c(0, 100),
     cex.main = 0.9,
     main = "Percentage qualifying for income assistance")

cor(CASchools$testscr, CASchools$el_pct)
cor(CASchools$testscr, CASchools$meal_pct)
cor(CASchools$testscr, CASchools$calw_pct)

#######################################################
## Regression analysis
#######################################################

spec1 <- lm(testscr ~ enrl_tot, data = CASchools)
spec2 <- lm(testscr ~ enrl_tot + el_pct, data = CASchools)
spec3 <- lm(testscr ~ enrl_tot + el_pct + meal_pct, data = CASchools)
spec4 <- lm(testscr ~ enrl_tot + el_pct + calw_pct, data = CASchools)
spec5 <- lm(testscr ~ enrl_tot + el_pct + meal_pct + calw_pct, data = CASchools)

plot(spec5)
summary(spec5)

# gather robust standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(spec1, type = "HC1"))),
               sqrt(diag(vcovHC(spec2, type = "HC1"))),
               sqrt(diag(vcovHC(spec3, type = "HC1"))),
               sqrt(diag(vcovHC(spec4, type = "HC1"))),
               sqrt(diag(vcovHC(spec5, type = "HC1"))))

## Comparing SEs
sqrt(diag(vcovHC(spec5, type = "HC1")))
sqrt(diag(vcovHC(spec5, type = "const")))

summary(spec5)

# generate a LaTeX table using stargazer
stargazer(spec1, spec2, spec3, spec4, spec5,
          se = rob_se,
          digits = 3,
          header = F,
          column.labels = c("(I)", "(II)", "(III)", "(IV)", "(V)"))

