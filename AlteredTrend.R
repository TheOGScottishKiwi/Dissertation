# reconsidering trend for my own Model

ag <- unique(Year)
g <- g[-1] 
g <- g[-length(g)]
g

annual_average <- c(1)
for (i in 1:length(g)) annual_average[i] <- mean(Flow[RiverFlowData$year==g[i]])

plot(g,annual_average)


# building some sweet linear models:
YearTrendMod<-lm(NonSeasonal ~ year)
DayTrendMod<-lm(NonSeasonal ~ index)

#summary(TrendMod)
AIC(YearTrendMod)
AIC(DayTrendMod)

abline(YearTrendMod, col = "red")
abline(DayTrendMod, col = "blue")

summary(YearTrendMod)
summary(DayTrendMod)

c <- summary(DayTrendMod)$coefficients["(Intercept)","Estimate"]
m <- summary(DayTrendMod)$coefficients["index","Estimate"]

for (i in 1:length(tsFlow)) RiverFlowData$Random[i] <- (RiverFlowData$NonSeasonal[i]-(m*(i-1)+c))
detach(RiverFlowData)
attach(RiverFlowData)

plot(RiverFlowData$index,RiverFlowData$Random)

ggplot(RiverFlowData, aes(Random)) +
  geom_histogram(bins = round(sqrt(length(tsFlow)))) +
  ggtitle("Random Component of Measured Flows") +
  labs(x="Random (m3/s)", y="Day Count")