library(tm)
library(tidytext)
library(ggplot2)

df$texto <- tolower(df$Noticia.Completa)
df$texto <- removePunctuation(df$texto)
df$texto <- removeNumbers(df$texto)
df$texto <- gsub("\\bnum\\b", "", df$texto) # La forma "\\*\\b" representa una expresión regular capaz de identificar texto aislado
df$texto <- gsub("enero", "", df$texto)
df$texto = trimws(df$texto, which = "both")
df$texto <- stripWhitespace(df$texto)

corpus <- Corpus(VectorSource(df$texto))
corpus_clean <- tm_map(corpus, removeWords, stopwords("spanish"))
df$texto_limpio <- sapply(corpus_clean, identity)

bigram_data <- df %>%
  unnest_tokens(bigram, texto, token = "ngrams", n = 2) # El n permite modifica distintos tipos de n-gramas

bigram_counts <- bigram_data %>%
  count(bigram, sort = TRUE)

ggplot(bigram_counts[1:10,], aes(x = reorder(bigram, n), y = n)) +
  geom_bar(stat = "identity") +
  xlab("Bigrama") + ylab("Frecuencia") +
  coord_flip() + # Para hacer el gráfico horizontal
  theme_minimal()
