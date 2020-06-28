# Creating a mixed model of the signal distribution

# Clustering for a start
rfr <- RiverFlowData$Random

rfr.kmeans <- kmeans(rfr, 2)
rfr.kmeans.cluster <- rfr.kmeans$cluster

rfr.df <- tibble(x = rfr, cluster = rfr.kmeans.cluster)

#plotting clusters
rfr.df %>%
  mutate(num = row_number()) %>%
  ggplot(aes(y = num, x = x, color = factor(cluster))) +
  geom_point() +
  ylab("Values") +
  ylab("Data Point Number") +
  scale_color_discrete(name = "Cluster") +
  ggtitle("K-means Clustering")

#Splitting data
tail <- rfr.df[which(rfr.df$cluster == 2),]
bulk <- rfr.df[which(rfr.df$cluster == 1),]

#Pareto distribution for tail

tail.min = min(tail$x)
tail.min

tail <- tail %>% mutate(z = x - tail.min)

#tail.scale <- paretoScale(tail, w = NULL, groups = NULL, method = "VanKerm",
#                          center = c("mean", "median"), probs = c(0, 0),
#                          na.rm = FALSE)
#tail.scale[1]

f1c <- fitdist(tail$z,"pareto",method="mle", start=list(shape = 1, scale = 5))
summary(f1c)
tail.shape <- f1c$estimate['shape']
tail.scale <- f1c$estimate['scale']
tail.location <- tail.min
tail.alpha <- length(tail)/length(rfr.df$x)

#tail expectation
comp2.prod <- dpareto(x = rfr, tail.location, 
                      shape = tail.shape)* tail.alpha


# Normal Distribution for bulk
bulk.mu <- mean(bulk$x)
bulk.var <- var(bulk$x)
bulk.std <- sd(bulk$x)
bulk.alpha <- length(bulk$x)/length(rfr.df$x)

# Bulk expectation
comp1.prod <- dnorm(x = rfr, mean = bulk.mu,
                    sd = bulk.std) * bulk.alpha





normalizer <- comp1.prod + comp2.prod

comp1.post <- comp1.prod / normalizer
comp2.post <- comp2.prod / normalizer


# Maximising: re-estiamte of component parameters
comp1.n <- sum(comp1.post, na.rm = FALSE)
comp2.n <- sum(comp2.post, na.rm = FALSE)