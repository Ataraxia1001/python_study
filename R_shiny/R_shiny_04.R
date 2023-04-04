load("./03_integrated/03_apt_price.rdata")
head(apt_price, 2)
# 1단계 결측값 확인
table(is.na(apt_price))
apt_price <- na.omit(apt_price) # 결측값 제거
table(is.na(apt_price)) #결측값 확인
# 2단계 문자열전처리
library(stringr) #문자열 처리하는 패키지
apt_price <- as.data.frame(apply(apt_price, 2, str_trim))#공백 제거 apply함수에서 1은 행, 2 열
head(apt_price$price, 2)
#3단계 항목별 데이터 다듬기
#3-1 매매 연월일, 데이터를 만들기
install.packages('lubridate') #make_date() 메소드 사용
library(lubridate)
library(dplyr) # %>%
apt_price <- apt_price %>% 
                mutate(ymd = make_date(year, month, day)) #YYYY-MM-DD
apt_price$ym <- floor_date(apt_price$ymd, "month")
#월로 바꿔서 컬럼을 생성
head(apt_price, 2)
# 3-2 매매가 확인후 숫자로 변경
head(apt_price$price, 4)
apt_price$price <- apt_price$price %>% 
            sub(",","",.) %>% as.numeric()
head(apt_price$price, 4)
# 3-3 주소 조합
head(apt_price$apt_nm, 30)
apt_price$apt_nm <- gsub("\\(.*","",apt_price$apt_nm)
head(apt_price$apt_nm, 30)
# 3-3 아파트이름 지역코드, 주소를 조합
loc <- read.csv('R/data/sigun_code.csv', fileEncoding = "UTF-8")
apt_price <- merge(apt_price, loc, by= 'code')
head(apt_price, 3)
apt_price$juso_jubun <- paste0(apt_price$addr_2, " ", 
                               apt_price$dong_nm, " ",
                               apt_price$jibun, " ",
                               apt_price$apt_nm)
head(apt_price, 3)
# 4단계 건축연도 숫자 변환
head(apt_price$con_year, 3)
apt_price$con_year <-  apt_price$con_year %>% as.numeric()
head(apt_price$con_year, 3)
# 5단계 평당 매매가 소수점은 round를 0으로 
# 면적을 3.3을 곱하여 평으로 변환후 평당 매매가를 구하시오
head(apt_price$area, 3)
apt_price$area <- apt_price$area %>% as.numeric() %>% round(0)
head(apt_price$area, 3)
apt_price$py <- round(((apt_price$price/apt_price$area)*3.3), 0)
head(apt_price$py, 3)
# 6단계 층수 변환 숫자로 변환 후에 절대값변환
min(apt_price$floor)
apt_price$floor <- apt_price$floor %>% as.numeric() %>% abs()
min(apt_price$floor)
apt_price$cnt <- 1 #모든 데이터 숫자 1을 할당
head(apt_price, 3)

# 7단계 저장하기 
apt_price <-  apt_price %>% 
  select(ymd, ym, year, code, addr_1, apt_nm, juso_jubun, price,
         con_year, area, floor, py, cnt)
head(apt_price, 2)
dir.create("./04_preprocess")
save(apt_price, file = "./04_preprocess/04_preprocess.rdata")
write.csv(apt_price, "./04_preprocess/04_preprocess.csv")






