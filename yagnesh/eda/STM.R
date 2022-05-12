setwd("C:/Users/yagne/Desktop/tad-dep")
libraries <- c("topicmodels", "dplyr", "stm", "quanteda", "lubridate", "ggplot2")
lapply(libraries, require, character.only = T)

data <- read.csv("data/final_lemmatized_go.csv")
data$datetime <- as.POSIXct(strptime(data$date, format= "%Y-%m-%d %H:%M:%S",tz = "GMT"))
data$dateonly <- mdy(paste(month(data$datetime), day(data$datetime), year(data$datetime), sep = "-"))

data["id"] <- seq(1,nrow(data))
data <- data[data$id != 18031,]
data <- data[data$id != 30669,]
data <- data[data$id != 41834,]
data <- data[data$id != 60985,]
data <- data[data$id != 74001,]


data<-data %>%
  mutate(DepFlag = as.numeric(depression)) %>% 
  mutate(DateNum = as.numeric(dateonly))


text_corpus <- corpus(data, docid_field = "id", text_field = "cleaned_text")
text_tokens <- tokens(text_corpus, include_docvars=T, remove_punct = T, remove_symbols = T)
text_tokens <- tokens_tolower(text_tokens)
text_tokens <- tokens_select(text_tokens, min_nchar = 2)
text_tokens <- tokens_remove(text_tokens, stopwords("english"))
text_dfm <- dfm(text_tokens) %>% dfm_trim(min_docfreq = 10)
# text_dfm <- dfm_subset(text_dfm, ntoken(text_dfm) > 0)
text_dfm


text_stm <-
  stm(
    text_dfm,
    K = 20,
    prevalence = ~ DepFlag + s(DateNum),
    data = docvars(text_corpus)
  )
saveRDS(text_stm, file = "stm_model99.rds")

plot(text_stm, type = "labels")
plot(text_stm, type = "summary")
plot(text_stm, type="perspectives", topics = c(8,1))
prep <- estimateEffect(c(1:20) ~ DepFlag + s(DateNum) , text_stm, meta =  docvars(text_corpus))

plot(prep, "DateNum", text_stm, topics = c(1:20), method = "continuous", xaxt = "n", xlab = "Date")


plot(
  prep,
  "DepFlag",
  model = text_stm,
  topics =  c(1:20),
  method = "difference",
  cov.value1 = "Depressed",
  cov.value2 = "Not Depressed"
)
