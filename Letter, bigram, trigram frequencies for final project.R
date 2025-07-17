# Load libraries
library(ggplot2)

# STEP 1: Load words
possible_answers <- read.table("C:\\Users\\admin\\Desktop\\Gargi study\\Word files\\wordle word lists\\wordle answer list.txt")
words <- as.character(possible_answers[[1]])

# STEP 2: Corpus-Based Frequency Tables

# LETTERS
all_letters <- unlist(strsplit(tolower(paste(words, collapse = "")), ""))
letter_table <- prop.table(table(factor(all_letters, levels = letters[1:26])))

# BIGRAMS
all_bigrams <- unlist(
  lapply(words, function(word) {
    sapply(1:(nchar(word) - 1), function(i) substr(word, i, i + 1))
  })
)
bigram_table <- prop.table(table(all_bigrams))

# TRIGRAMS
all_trigrams <- unlist(
  lapply(words, function(word) {
    sapply(1:(nchar(word) - 2), function(i) substr(word, i, i + 2))
  })
)
trigram_table <- prop.table(table(all_trigrams))

# STEP 3: Feature Extraction Function
extract_features <- function(word) {
  chars <- tolower(unlist(strsplit(word, "")))
  
  # Average corpus-based letter frequency
  letter_vals <- letter_table[chars]
  letter_vals[is.na(letter_vals)] <- 0
  avg_letter_freq <- mean(as.numeric(letter_vals))
  
  # Bigram frequencies
  bigrams <- sapply(1:(nchar(word) - 1), function(i) substr(word, i, i + 1))
  bigram_vals <- bigram_table[bigrams]
  bigram_vals[is.na(bigram_vals)] <- 0
  bigram_freq <- mean(as.numeric(bigram_vals))
  
  # Trigram frequencies
  trigrams <- sapply(1:(nchar(word) - 2), function(i) substr(word, i, i + 2))
  trigram_vals <- trigram_table[trigrams]
  trigram_vals[is.na(trigram_vals)] <- 0
  trigram_freq <- mean(as.numeric(trigram_vals))
  
  # Combine features
  features <- c(avg_letter_freq, bigram_freq, trigram_freq)
  names(features) <- c("letter_freq", "bigram_freq", "trigram_freq")
  return(features)
}

# STEP 4: Extract features for all words
features_df <- do.call(rbind, lapply(words, extract_features))
features_df <- as.data.frame(features_df)
features_df$word <- words

# STEP 5: Visualize each corpus-based frequency

# A. LETTER FREQUENCY PLOT
letter_df <- data.frame(
  letter = names(letter_table),
  frequency = as.numeric(letter_table)
)

ggplot(letter_df, aes(x = reorder(letter, -frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Corpus-Based Letter Frequencies", x = "Letter", y = "Frequency") +
  theme_minimal()

# B. BIGRAM FREQUENCY PLOT
top_bigrams <- head(sort(bigram_table, decreasing = TRUE), 15)
bigram_df <- data.frame(
  bigram = names(top_bigrams),
  frequency = as.numeric(top_bigrams)
)

ggplot(bigram_df, aes(x = reorder(bigram, -frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(title = "Top 15 Corpus-Based Bigram Frequencies", x = "Bigram", y = "Frequency") +
  theme_minimal()

# C. TRIGRAM FREQUENCY PLOT
top_trigrams <- head(sort(trigram_table, decreasing = TRUE), 15)
trigram_df <- data.frame(
  trigram = names(top_trigrams),
  frequency = as.numeric(top_trigrams)
)

ggplot(trigram_df, aes(x = reorder(trigram, -frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "tomato") +
  labs(title = "Top 15 Corpus-Based Trigram Frequencies", x = "Trigram", y = "Frequency") +
  theme_minimal()

