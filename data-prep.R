library(quanteda)
library(readtext)
library(stopwords)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(forcats)
library(stm)
set.seed(123)
quanteda_options(
  "threads" = getOption("Ncpus", 1L) - 1)

chars <- read_csv("data/lotr_characters.csv")
df <- readtext("data/lotr_scripts.csv", text_field = "dialog")

interesting_chars = c("FRODO", "SAM", "GANDALF", "ARAGORN", "PIPPIN", "MERRY", "GOLLUM", "GIMLI", "LEGOLAS")

df <- df %>%
  mutate(char = str_replace_all(char, " ", "")) %>%
  mutate(char = if_else(char %in% interesting_chars, char, "Other"))
saveRDS(df, "processed-data/df.rds")

corpus <- corpus(df)
saveRDS(corpus, "processed-data/corpus.rds")

toks <- tokens(corpus,
remove_punct = TRUE,
remove_symbols = TRUE,
remove_numbers = TRUE,
remove_separators = TRUE,
split_hyphens = TRUE
)

saveRDS(toks, "processed-data/toks.rds")

dfm_mat <- dfm(
  toks,
  tolower = TRUE)

dfm_mat <- dfm_remove(
  dfm_mat,
  pattern = c(
    stopwords(language = "en", source = "snowball"),
    stopwords(language = "en", source = "stopwords-iso"),
    stopwords(language = "en", source = "nltk")
  ))
dfm_mat <- dfm_wordstem(
  dfm_mat
)
saveRDS(dfm_mat, "processed-data/dfm_mat.rds")

dtm_stm <- convert(dfm_mat, to = "stm")
saveRDS(dtm_stm, "processed-data/dtm_stm.rds")

how_many_k <- searchK(dtm_stm$documents,
                      vocab = dtm_stm$vocab,
                      K = 3:10,
                      init.type = "Spectral",
                      # multicore problems using only 1
                      # I think its a RAM problem
                      #cores = floor(getOption("Ncpus")/4))
                      cores = 1)
saveRDS(how_many_k, "processed-data/how_many_k.rds")

topic_model <- stm(dtm_stm$documents,
                   vocab = dtm_stm$vocab,
                   data = dtm_stm$meta,
                   K = 3,
                   prevalence =~ movie + char,
                   seed = 123,
                   init.type = "Random")
saveRDS(topic_model, "processed-data/topic_model.rds")

regression <- estimateEffect(
  1:3 ~ movie + char,
  topic_model,
  meta = dtm_stm$meta,
  uncertainty = "Global")
saveRDS(regression, "processed-data/regression.rds")

