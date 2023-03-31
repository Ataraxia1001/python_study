library(dplyr)
library(multilinguer)
library(KoNLP)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
useNIADic()
twitter <-read.csv("R/data/twitter.csv", header = T, fileEncoding = "UTF-8")
twitter <- rename(twitter, no = 번호,
                  id = 계정이름,
                  date = 작성일,
                  tw = 내용)
#특수문자 제거
twitter$tw <- str_replace_all(twitter$tw, "\\W", " ")
#트윗에서 명사를 추출
nouns <- extractNoun(twitter$tw)
wordcount <- table(unlist(nouns))
wordcount
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word <-  rename(df_word, word = Var1, 
                   freq = Freq)
df_word <- filter(df_word, nchar(word) >= 2)
top_20 <-  df_word %>% 
  arrange(desc(freq)) %>%  head(20)
top_20

ggplot()
order <- arrange(top_20, freq)$word #빈도 순서 변수 생성
order
ggplot(data = top_20, aes(x=word, y=freq)) +
  geom_col() +
  ylim(0, 2500) +
  coord_flip() +
  scale_x_discrete(limit = order) +
  geom_text(aes(label = freq), hjust= - 0.3) # 빈도표시

pal <- brewer.pal(8, "Dark2")
set.seed(2023)
wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 10,
          max.words = 200,
          random.order = F,
          rot.per = 0,
          scale = c(6, 0.5),
          colors = pal)


