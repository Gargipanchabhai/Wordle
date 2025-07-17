data = read.csv("C:\\Users\\admin\\Desktop\\Gargi study\\TYBsc project\\final project\\For final project dataset.csv", header = TRUE)
head(data)
colnames(data)

# Selecting relevant columns
guess_distribution = data[, c("guess.1", "guess.2", "guess.3", "guess.4","guess.5", "guess.6", "guess.x")]
other_features = data[, c("ScrabbleScore", "CV_Ratio", "UniqueLetters", "Positional_Score", 
                           "Bigram", "Trigram")]

clustering_data = cbind(guess_distribution, other_features)

##corrplot
library(corrplot)
library(dplyr)

# correlation matrix
cor_matrix = cor(clustering_data, use="complete.obs")

# correlation plot
corrplot(cor_matrix, method="color", type="upper", tl.cex = 0.8)

# Scaling
scaled_clustering_data = scale(clustering_data)

# PCA 
pca_result = prcomp(scaled_clustering_data, center = TRUE, scale. = TRUE)

# Scree plot
screeplot(pca_result, type = "lines", main = "Scree Plot")

summary(pca_result)


# Or a nicer ggplot version
library(factoextra)
fviz_eig(pca_result)


pca_scores = pca_result$x

pca_subset = pca_scores[, 1:8] 

# determine optimal number of clusters
library(NbClust)
nb_cluster = NbClust(data=clustering_data, method="ward.D2")

warnings()

# k-means
set.seed(123)
kmeans_model = kmeans(pca_subset, centers = 3, nstart = 25)

clustering_data$cluster = as.factor(kmeans_model$cluster)
nrow(pca_subset) == nrow(data)

data$cluster = as.factor(kmeans_model$cluster)

table(data$cluster)

# Load ggplot2 if not already
library(ggplot2)

plot_data = as.data.frame(pca_scores[, 1:2])  # First 2 PCs
plot_data$cluster = as.factor(kmeans_model$cluster)

# scatter plot
ggplot(plot_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(title = "K-Means Clustering Based on PCA",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

library(cluster)


# Compute silhouette score for the clustering
silhouette_score = silhouette(kmeans_model$cluster, dist(scaled_clustering_data))
silhouette_score

# Plot silhouette score
plot(silhouette_score)

# Mean of each feature
aggregate(clustering_data, by = list(data$cluster), FUN = mean)


easy_words = subset(data, cluster == 2)$Wordle.Answer
easy_words
moderate_words = subset(data, cluster == 3)$Wordle.Answer
moderate_words
difficult_words = subset(data, cluster == 1)$Wordle.Answer
difficult_words
# View first few words from each cluster
head(easy_words)
head(moderate_words)
head(difficult_words)

## from results we can say that cluster 3: hard, cluster 2:easy, cluster 1:moderate

aggregate(data[, c("Average.guess", "ScrabbleScore", "CV_Ratio", 
                   "UniqueLetters", "Positional_Score", "Bigram", "Trigram")],
          by = list(Cluster = data$cluster), FUN = median)


data$cluster = as.factor(data$cluster)

# Mann-Whitney U test
pairwise.wilcox.test(data$Average.guess, data$cluster,
                     p.adjust.method = "bonferroni")

# ScrabbleScore
pairwise.wilcox.test(data$ScrabbleScore, data$cluster, p.adjust.method = "bonferroni")

# CV_Ratio
pairwise.wilcox.test(data$CV_Ratio, data$cluster, p.adjust.method = "bonferroni")

# UniqueLetters
pairwise.wilcox.test(data$UniqueLetters, data$cluster, p.adjust.method = "bonferroni")

# Positional_Score
pairwise.wilcox.test(data$Positional_Score, data$cluster, p.adjust.method = "bonferroni")


library(ggplot2)

ggplot(data, aes(x = cluster, y = Average.guess, fill = cluster)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Average Guesses by Cluster",
       x = "Cluster (Difficulty Level)",
       y = "Average Number of Guesses") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Map cluster numbers to difficulty levels
data$Difficulty = factor(data$cluster,
                          levels = c(2, 3, 1),
                          labels = c("Easy", "Moderate", "Difficult"))

library(ggplot2)

ggplot(data, aes(x = Difficulty, y = Average.guess, fill = Difficulty)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Guess Distribution per Difficulty Level",
       x = "Difficulty Level",
       y = "Average Number of Guesses") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

library(ggplot2)

# Scrabble Score
ggplot(data, aes(x = cluster, y = ScrabbleScore, fill = cluster)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Scrabble Score by Cluster",
       x = "Cluster (Difficulty Level)",
       y = "Scrabble Score") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# CV Ratio
ggplot(data, aes(x = cluster, y = CV_Ratio, fill = cluster)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Consonant-Vowel Ratio by Cluster",
       x = "Cluster (Difficulty Level)",
       y = "CV Ratio") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Unique Letters
ggplot(data, aes(x = cluster, y = UniqueLetters, fill = cluster)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Unique Letters by Cluster",
       x = "Cluster (Difficulty Level)",
       y = "Number of Unique Letters") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Positional Score
ggplot(data, aes(x = cluster, y = Positional_Score, fill = cluster)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Positional Score by Cluster",
       x = "Cluster (Difficulty Level)",
       y = "Positional Score") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


# Bigram bar plot
ggplot(data, aes(x = cluster, fill = factor(Bigram))) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Words with Bigrams by Cluster",
       x = "Cluster", y = "Proportion", fill = "Bigram Present") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")

# Trigram bar plot
ggplot(data, aes(x = cluster, fill = factor(Trigram))) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Words with Trigrams by Cluster",
       x = "Cluster", y = "Proportion", fill = "Trigram Present") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel2")

