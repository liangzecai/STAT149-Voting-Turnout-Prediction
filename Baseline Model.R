setwd("~/Desktop/Harvard/Spring 2017/Stat 149/Project")

#read in the data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

bench.glm = glm(voted ~ gender + cd + hd + age + party + racename + hsonly + mrrg + 
        chldprsnt + cath+ evang + nonchrst + otherchrst + days.since.reg, 
        family=binomial, data=train)

#predict.bench = predict(bench.glm, newdata = test, type = "response", se.fit = TRUE)
predict.bench = predict(bench.glm, newdata = test, type = "response")
predict.bench

#write the final csv file
write.csv(predict.bench, "Pred_Prob.csv")
