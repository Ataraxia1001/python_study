library(dplyr)
install.packages("multilinguer")
 
library(multilinguer)
install_jdk()
install.packages(c("stringr", "hash", "tau","Sejong", "RSQLite", "devtools"), type = "binary")
install.packages("remotes")
remotes::install_github("haven-jeon/KoNLP", upgrade = "never",
                        INSTALL_opts=c("--no-multiarch"))
library(KoNLP)
useNIADic()

txt <- readLines("R/data/hiphop.txt")
library(stringr)
txt <- str_replace_all(txt, "\\W", " ") #특수문자제거

extractNoun("대한민국의 영토는 한반도와 그 부속 도서로 한다")
nouns <- extractNoun(txt)
# 추출한 명사 list를 문자열 벡터로 변환 단어별 빈도표 생성
wordcount <- table(unlist(nouns))
wordcount
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word <- rename(df_word, word = Var1, freq = Freq) #두글자 이상 단어 추출
df_word <- filter(df_word, nchar(word) >= 2)
top_20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)
top_20

install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)
pal <- brewer.pal(8, "Dark2")
set.seed(2023)
wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(4, 0.3),
          colors = pal)

pal2 <- brewer.pal(9, "Blues")[5:9]
set.seed(1234)
wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(4, 0.3),
          colors = pal2)






