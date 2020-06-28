# Creating a HMM as per Patidar 

# Observed states (as by Patidar):

Q0 <- min(Flow)
Q10 <- quantile(Flow, 0.1) 
Q20 <- quantile(Flow, 0.2) 
Q30 <- quantile(Flow, 0.3) 
Q40 <- quantile(Flow, 0.4) 
Q50 <- quantile(Flow, 0.5) 
Q60 <- quantile(Flow, 0.6) 
Q70 <- quantile(Flow, 0.7) 
Q80 <- quantile(Flow, 0.8) 
Q90 <- quantile(Flow, 0.9) 
Q99 <- quantile(Flow, 0.99) 
Q100 <- max(Flow)

RiverFlowData <- mutate(RiverFlowData$Observed_state <- case_when((Flow < Q10) ~ "A",
                                                    (Flow >= Q10 & Flow < Q20) ~ "B",
                                                    (Flow >= Q20 & Flow < Q30) ~ "C",
                                                    (Flow >= Q30 & Flow < Q40) ~ "D",
                                                    (Flow >= Q40 & Flow < Q50) ~ "E",
                                                    (Flow >= Q50 & Flow < Q60) ~ "F",
                                                    (Flow >= Q60 & Flow < Q70) ~ "G",
                                                    (Flow >= Q70 & Flow < Q80) ~ "H",
                                                    (Flow >= Q80 & Flow < Q90) ~ "I",
                                                    (Flow >= Q90 & Flow < Q99) ~ "J",
                                                    (Flow >= Q99) ~ "K"))

RiverFlowData <- mutate(RiverFlowData$Unobserved_state <- case_when((Observed_state == "A") ~ (10*(Flow-Q0))%/%(Q10-Q0),
                                                      (Observed_state == "B") ~ (10*(Flow-Q10))%/%(Q20-Q10),
                                                      (Observed_state == "C") ~ (10*(Flow-Q20))%/%(Q30-Q20),
                                                      (Observed_state == "D") ~ (10*(Flow-Q30))%/%(Q40-Q30),
                                                      (Observed_state == "E") ~ (10*(Flow-Q40))%/%(Q50-Q40),
                                                      (Observed_state == "F") ~ (10*(Flow-Q50))%/%(Q60-Q50),
                                                      (Observed_state == "G") ~ (10*(Flow-Q60))%/%(Q70-Q60),
                                                      (Observed_state == "H") ~ (10*(Flow-Q70))%/%(Q80-Q70),
                                                      (Observed_state == "I") ~ (10*(Flow-Q80))%/%(Q90-Q80),
                                                      (Observed_state == "J") ~ (10*(Flow-Q90))%/%(Q99-Q90),
                                                      (Observed_state == "K") ~ (10*(Flow-Q99))%/%(Q100-Q99)))
detach(RiverFlowData)                 
attach(RiverFlowData)   

# Creating a vector of Observed State data:
osd <- Observed_state

# Creating a vector of unobserved State data:
usd <- Unobserved_state

#Creating the state transitional probability matrix
#Making empty matrix
states <- c('A','B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K')
stpc <- matrix(0L, nrow = 11, ncol = 11, 
               dimnames = list(states, states))

# Getting counts
for (k in 1:(lngth-1)) stpc[osd[k],osd[(k+1)]] <- (stpc[osd[k],osd[(k+1)]]+1)

# and row sums
stpcs = rowSums(stpc)

#Creating empty probability matrix
stp <- matrix(0L, nrow = 11, ncol = 11, 
              dimnames = list(states, states))

#then calculate probabilities:
for (i in 1:length(states)) for (j in 1:length(states)) stp[i,j] <- stpc[i,j]/stpcs[i]


##Creating the emission probability matrix
symbols = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
epc <- matrix(0L, nrow = 11, ncol = 10, 
              dimnames = list(states,symbols))
ep <- matrix(0L, nrow = length(states), ncol = length(symbols), 
             dimnames = list(states,symbols))

#Filling these matrices with counts
for (k in 1:(lngth)) epc[osd[k],usd[(k)]] <- (epc[osd[k],usd[(k)]]+1)

# getting row sums
epcs <- rowSums(epc)

#Calculating the probabilities
for (i in 1:length(states)) for (j in 1:length(symbols)) ep[i,j] <- epc[i,j]/epcs[i]




# two things perhaps:
# 1): and maybe 2): there is not much data for the observed state K
# not sure its fair to base any more of only few data points
# Other point, no data seems to be in unobserved state 9
# reckon I've messed the calc