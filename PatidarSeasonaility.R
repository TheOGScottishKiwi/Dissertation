# trying to seperate out trend and seasonaility from signal data as per Patidar

tsFlow <-ts(Flow,start=c(RiverFlowData$year[1],RiverFlowData$day[1]),frequency=365)
stlFlow <- stl(tsFlow,s.window="periodic")
seasonSTLFlow = stlFlow$time.series[,1]
trendSTLFlow = stlFlow$time.series[,2]
#plot(stlFlow, main="Flow")

for (i in 1:length(tsFlow)) RiverFlowData$NonSeasonal[i] <- (RiverFlowData$Flow[i]-seasonSTLFlow[i])
detach(RiverFlowData)
attach(RiverFlowData)
