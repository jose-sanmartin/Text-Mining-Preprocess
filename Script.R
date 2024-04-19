library(tm)
library(tidytext)
library(ggplot2)

df$texto <- tolower(df$v)
df$texto <- removePunctuation(df$texto)
df$texto <- removeNumbers(df$texto)
df$texto <- gsub("á", "a", df$texto)
df$texto <- gsub("é", "e", df$texto)
df$texto <- gsub("í", "i", df$texto)
df$texto <- gsub("ó", "o", df$texto)
df$texto <- gsub("ú", "u", df$texto)
df$texto <- gsub("\\bnum\\b", "", df$texto) # La forma "\\*\\b" representa una expresion regular capaz de identificar texto aislado
df$texto <- gsub("enero", "", df$texto) # Eliminacion simple de terminos
df$texto = trimws(df$texto, which = "both") # eliminacion de espacios en blanco al inicio y final del vector str
df$texto <- stripWhitespace(df$texto) # eliminacion de espacios dobles o mas en blanco dentro del vector str

corpus <- Corpus(VectorSource(df$texto)) #transformacion de la variable str en un corpus
corpus_clean <- tm_map(corpus, removeWords, stopwords("spanish")) #identificacion de stopwords
df$texto_limpio <- sapply(corpus_clean, identity) #eliminacion de stopwords

bigram_data <- df %>%
  unnest_tokens(bigram, texto_limpio, token = "ngrams", n = 2) # El n permite modifica distintos tipos de n-gramas

bigram_counts <- bigram_data %>%
  count(bigram, sort = TRUE)

ggplot(bigram_counts[1:10,], aes(x = reorder(bigram, n), y = n)) +
  geom_bar(stat = "identity") +
  xlab("Bigrama") + ylab("Frecuencia") +
  coord_flip() + # Para hacer el gráfico horizontal
  theme_minimal()
