load("./03_integrated/03_apt_price.rdata")  # 실거래 자료 불러오기
head(apt_price, 2)                          # 확인

#---# [2단계: 결측값 확인]

table(is.na(apt_price))         # 현재 결측값 확인
apt_price <- na.omit(apt_price) # 결측값 제거
table(is.na(apt_price))         # 결측값 제거 확인

head(apt_price$price, 2)      # 현재 결측값 확인  

library(stringr)              # 문자열 처리 패키지 실행
apt_price <- as.data.frame(apply(apt_price, 2, str_trim)) # 공백 제거
head(apt_price$price, 2)                                  # 확인

#--------------------------
# 4-2 항목별 데이터 다듬기
#--------------------------

#---# [1단계: 매매 연월일, 연월 데이터 만들기]

library(lubridate)  # install.packages("lubridate") / install.packages("cli") 
library(dplyr)      # install.packages("dplyr")
apt_price <- apt_price %>% mutate(ymd=make_date(year, month, day))  # 연월일
apt_price$ym <- floor_date(apt_price$ymd, "month")                  # 연월
head(apt_price, 2)                                                  # 확인

#---# [2단계: 매매가 확인]

head(apt_price$price, 3)

apt_price$price <- apt_price$price %>% sub(",","",.) %>% as.numeric() # 매매가 변환(문자 → 숫자)
head(apt_price$price, 3)  # 확인

#---# [3단계: 주소 조합]

head(apt_price$apt_nm, 30)  # 아파트 이름 현황

apt_price$apt_nm <- gsub("\\(.*","", apt_price$apt_nm) # 괄호이후 삭제
head(apt_price$apt_nm, 30)                             # 아파트 이름 확인

loc <- read.csv("R/data/sigun_code.csv", fileEncoding="UTF-8")  # 지역코드 불러오기
apt_price <- merge(apt_price, loc, by = 'code')         # 지역명 결합하기
apt_price$juso_jibun <- paste0(apt_price$addr_2, " ", apt_price$dong_nm," ",
                               apt_price$jibun," ",apt_price$apt_nm) # 주소조합
head(apt_price, 2)                                      # 확인

#---# [4단계: 건축연도 변환]

head(apt_price$con_year, 3)

apt_price$con_year <- apt_price$con_year %>% as.numeric()   # 건축연도 숫자변환
head(apt_price$con_year, 3)   # 건축연도 확인

#---# [5단계: 평당 매매가 만들기]

head(apt_price$area, 3)   # 확인

apt_price$area <- apt_price$area %>% as.numeric() %>% round(0)  # 전용면적 숫자변환
head(apt_price$area, 3)          # 확인

apt_price$py <- round(((apt_price$price/apt_price$area) * 3.3), 0) # 평당가격 계산
head(apt_price$py, 3)           # 확인


#---# [6단계: 층수 변환]

min(apt_price$floor)   # 확인

apt_price$floor <- apt_price$floor %>% as.numeric() %>% abs() # 층수 숫자변환
min(apt_price$floor)

apt_price$cnt <- 1   # 모든 데이터에 숫자 1 할당
head(apt_price, 2)   # 확인


#---------------
# 4-3 저장하기
#---------------

#---# [1단계: 필요칼럼 추출]

apt_price <- apt_price %>% select(ymd, ym, year, code, addr_1, apt_nm, 
                                  juso_jibun, price, con_year,  area, floor, py, cnt) # 칼럼 추출
head(apt_price, 2)  # 확인

#---# [2단계: 저장]


dir.create("./04_preprocess")   # 새로운 폴더 생성
save(apt_price, file = "./04_preprocess/04_preprocess.rdata") # 저장
write.csv(apt_price, "./04_preprocess/04_preprocess.csv") 
 