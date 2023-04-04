install.packages("rstudioapi")
getwd()
# 지역코드 불러오기
loc <- read.csv("R/data/sigun_code.csv", fileEncoding = "UTF-8")
loc$code <- as.character(loc$code)
head(loc, 2)
# 수집기간 설정하기
datelist <- seq(from = as.Date('2021-01-01'),
                to = as.Date('2021-12-31'),
                by = '1 month')
datelist <- format(datelist, format = '%Y%m')
# (YYYY-MM-DD => YYYYMM)
datelist[1:3]
service_key <- "huEy2FccpR8mKLnAqpCZ99QdqUA%2FxwQtGysBZIpD3ML4Fc%2FEdqeprh8uNpLrIshd0%2BXkq1M95bMVvNb%2FzMXcDA%3D%3D"

# 요청목록 빈리스트 만들기
url_list <- list()
cnt <- 0 # 반복문 제어변 초기값 설정
#요청목록 채우기
for(i in 1:nrow(loc)){
  for(j in 1:length(datelist)){
    cnt <- cnt + 1
    # URL 목록 채우기 
    url_list[cnt] <- paste0("http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?",
                "LAWD_CD=", loc[i,1],
                "&DEAL_YMD=", datelist[j],
                "&numOfRows=", 100,
                "&serviceKey=", service_key)
    Sys.sleep(0.1) #0.1 멈춤
    msg <- paste0("[", i, "/", nrow(loc), "] ", loc[i, 3], "의 크롤링 목록이 생성됨 => 총 [",  cnt, "] 건")
    cat(msg, "\n\n")                      
  }
}
length(url_list)
browseURL(paste0(url_list[1]))

install.packages("XML")
install.packages("data.table")
install.packages("stringr")
library(XML)
library(data.table)
library(stringr)

# 1단계 임시저장 리스트생성
raw_data <- list() # xml 임시저장소
root_Node <- list() #거래내역 추출 임시저장
total <- list() #거래내역 정리 임시저장
dir.create("02_raw_data") #새로운 디렉토리 생성
# 2단계 URL 요청 - XML 응답
for(i in 1:length(url_list)){
  raw_data[[i]] <- xmlTreeParse(url_list[i], useInternalNodes = TRUE, encoding = "utf-8")
  root_Node[[i]] <- xmlRoot(raw_data[[i]])
  #전체 거래건수 추출
  items <- root_Node[[i]][[2]][['items']] 
  # 전체 거래건수 확인
  size <-  xmlSize(items) 
  
  # 거래내역 추출
  item <- list()
  item_temp_dt <- data.table() # 세부거래내역(item) 저장 임시테이블
  Sys.sleep(.1)
  for(m in 1:size){
    item_temp <- xmlSApply(items[[m]], xmlValue)
    item_temp_dt <- data.table(year = item_temp[4], #년도
                            month = item_temp[7], #월
                            day = item_temp[8],
                            price = item_temp[1],
                            code = item_temp[12],
                            dong_nm = item_temp[5],
                            jibun = item_temp[11],
                            con_year = item_temp[3],
                            apt_nm = item_temp[6],
                            area = item_temp[9],
                            floor = item_temp[13])
    item[[m]] <- item_temp_dt
    }
  apt_bind <- rbindlist(item) # 통합저장
  # 응답내역 저장
  region_nm <- subset(loc, code== str_sub(url_list[i], 115,119))$addr_1 #지역명 추출
  month <-  str_sub(url_list[i], 130, 135) # 연월(YYYMM) 추출
  path <- as.character(paste0("./02_raw_data/", region_nm,"-",month,".csv")) # 저장위치 설정
  write.csv(apt_bind, path)
  msg <- paste0("[", i, "/", length(url_list),"] 수집한 데이터를 [", path, "] 에 저장 합니다. ")
  cat(msg, "\n\n")
}

files <-  dir('./02_raw_data')
library(plyr) # install.packages("plyr")
apt_price <- ldply(as.list(paste0("./02_raw_data/", files)), read.csv) #결합
apt_price
dir.create("./03_integrated")
save(apt_price, file = "03_apt_price.rdata")
write.csv(apt_price, "03_apt_price.csv")




