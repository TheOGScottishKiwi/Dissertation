# Check if any Flow elements are less than 0, set to NaN 
Flow[Flow <= 0]
Flow[is.nan(Flow)]

# Preping data:
# Split data into year, month, dat
RiverFlowData <- RiverFlowData %>% mutate(date=ymd(Date)) %>% mutate_at(vars(date),lst(year,month,day))
# Add season to data
RiverFlowData <- RiverFlowData %>% mutate(season = case_when(month %in% 3:5 ~ "Spr",
                                                             month %in% 6:8 ~ "Sum",
                                                             month %in% 9:11 ~ "Aut",
                                                             TRUE ~ "Win"))
# Seperate out and remove leap days
leap_days <- subset(RiverFlowData, RiverFlowData$month == 2 & RiverFlowData$day == 29)
RiverFlowData <- RiverFlowData[-as.numeric(rownames(leap_days)),]
RiverFlowData$index <- as.numeric(rownames(RiverFlowData))

detach(RiverFlowData)
attach(RiverFlowData)