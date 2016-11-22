#sideload function
source("mice.impute.rstanarm.R")

#test
require(mice)
require(rstanarm)
imp1 <- mice(boys[,1:5], method = "rstanarm", m=5)
imp2 <- mice(boys[,1:5], method = "norm", m=5)
summary(pool(with(imp1, lm(bmi ~ age + wgt + hgt))))
summary(pool(with(imp2, lm(bmi ~ age + wgt + hgt))))

#test
#make complete subset of data
boys2 <- na.omit(boys[1:5])
#ampute (MCAR)
set.seed(123)
boys2[sample(1:nrow(boys2), 170), 2] <- NA
boys2[sample(1:nrow(boys2), 170), 3] <- NA
boys2[sample(1:nrow(boys2), 170), 4] <- NA
boys2[sample(1:nrow(boys2), 170), 5] <- NA
#impute
system.time(imp3 <- mice(boys2, method = "rstanarm", m=5, print=FALSE, seed=123))
system.time(imp4 <- mice(boys2, method = "norm", m=5, print=FALSE, seed=123))
#eval
summary(lm(bmi ~ age + hgt, data = na.omit(boys[1:5]))) #population
summary(lm(bmi ~ age + hgt, data = boys2)) #obs
summary(pool(with(imp3, lm(bmi ~ age + hgt)))) #rstanarm
summary(pool(with(imp4, lm(bmi ~ age + hgt)))) #norm
