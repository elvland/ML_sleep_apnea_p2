# Hierarchical Clustering: OSA Vowel Formants:
#

rm(list = ls())


Input_file <- 'OSA_wav.csv'

Data_Directory <- "C:\\R_MLLB\\DATA\\"

file ='OSA_wav.csv'

df = read.csv(Input_file, header = TRUE)

head(df)

df$label <- as.factor(df$label)

library(ggplot2)
ggplot(df, aes(F1, F2, color = label)) + geom_point()


# A simple KMeans
set.seed(20)
vowelCluster <- kmeans(df[, 4:5], 5, nstart = 20)
vowelCluster


# Let us compare the clusters with the vowels.

table(vowelCluster$cluster, df$label)


# We can also plot the data to see the clusters:

vowelCluster$cluster <- as.factor(vowelCluster$cluster)
ggplot(df, aes(F1, F2, color = vowelCluster$cluster)) + geom_point()


# HIERARCHICAL CLUSTERING
# hclust() function implements hierarchical clustering in R.

x=df[, 4:5]

hc.complete = hclust (dist(x), method ="complete")

hc.average =hclust (dist(x), method ="average")
hc.single =hclust (dist(x), method ="single")



# We can now plot the dendrograms obtained
# using the usual plot() function.
# The numbers at the bottom of the plot identify
# each observation.


# par(mfrow =c(1,3))
plot(hc.complete ,main ="Complete Linkage", xlab="", sub ="",
       cex =.9)
plot(hc.average , main ="Average Linkage", xlab="", sub ="",
       cex =.9)
plot(hc.single , main="Single Linkage", xlab="", sub ="",
       cex =.9)

# Cluster labels
vowelClusterH <- cutree (hc.complete , 5)

# We can also plot the data to see the clusters:

vowelClusterH <- as.factor(vowelClusterH)
ggplot(df, aes(F1, F2, color = vowelClusterH)) + geom_point()

# Scaling before clustering
xsc=scale (x)
hc.complete =hclust (dist(xsc), method ="complete")

# Cluster labels
vowelClusterH <- cutree (hc.complete , 5)

# We can also plot the data to see the clusters:

vowelClusterH <- as.factor(vowelClusterH)
ggplot(df, aes(F1, F2, color = vowelClusterH)) + geom_point()



