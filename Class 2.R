require(neuralnet)
library(neuralnet)
data <- read.csv('D:/Term 3/New folder/Datasets/dividendinfo.csv', header=TRUE)
attach(data)
str(data)

scaleddata<-scale(data)
normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
maxmindata<-as.data.frame(lapply(data,normalize))

trainset<- maxmindata[1:150, ]
testset<- maxmindata[151:200, ]

library(neuralnet)
nn <- neuralnet(dividend~fcfps+earnings_growth+de+mcap+current_ratio, data = trainset, hidden=c(2,1), linear.output = FALSE, threshold = 0.01)
nn$result.matrix
plot(nn)

nn$result.matrix

temp_test<- subset(testset, select = c("fcfps", "earnings_growth", "de", "mcap", "current_ratio"))
head(temp_test)
nn.results<- compute(nn,temp_test)
results<- data.frame(actual=testset$dividend, prediction= nn.results$net.result)

results

fcfps = c(2, 3, 4)
earnings_growth = c(15, 21, 11)
de = c(2, 3, 4)
mcap = c(545, 300, 145)
current_ratio = c(0, 1, 2)
df = data.frame(fcfps, earnings_growth, de, mcap, current_ratio)

test=data.frame(fcfps,earnings_growth, de, mcap, current_ratio)

Predict=compute(nn,test)
Predict$net.result
