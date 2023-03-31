install.packages("mapproj")
install.packages("ggiraphExtra")
library(ggiraphExtra)
str(USArrests)
head(USArrests)
library(tibble) #dplyr 과 함께 설치됨
crime <- rownames_to_column(USArrests, var='state')
crime$state <- tolower(crime$state)
str(crime)
# 미국지도 데이터 준비
install.packages("maps") # 미국 위도경도 정보가있는 패키지
library(ggplot2)
state_map <- map_data("state")
str(state_map)
ggChoropleth(data = crime,
             aes(fill = Murder,
                 map_id = state),
             map= state_map)
# 인터랙티브한 단계구분도 만들기
ggChoropleth(data = crime,
             aes(fill = Murder,
                 map_id = state),
             map= state_map,
             interactive = T)

#대한민국 시도별 인구 결핵환자
install.packages('stringi')
install.packages('devtools')
devtools::install_github("cardiomoon/kormaps2014")
library(kormaps2014)
str(changeCode(korpop1))
library(dplyr)
korpop1 <- rename(korpop1,
                  pop=총인구_명,
                  name = 행정구역별_읍면동)
korpop1$name <- iconv(korpop1$name, "UTF-8", "CP949")
View(korpop1)
str(changeCode(kormap1))
library(ggplot2)
library(ggiraphExtra)
ggChoropleth(data = korpop1, 
             aes(fill = pop, 
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)

## 결핵환자 수 데이터
str(changeCode(tbc))
tbc$name <- iconv(tbc$name, "UTF-8", "CP949")
View(tbc)

ggChoropleth(data = tbc, 
             aes(fill = NewPts, 
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)
