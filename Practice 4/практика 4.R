

data <- dataLands[,-1]
# data[2]
LM<-lm(data$`mortality from accidents`~as.factor(data$group)+data$`gross domestic product 97`, subset(data, data$group!=0))
summary(LM)



wilcox.test(data$`natural population growth 97`~as.factor(data$group), exact=FALSE,  subset(data, data$group!=0))
with(data, tapply(data$`natural population growth 97`,as.factor(data$group), 'mean'))

