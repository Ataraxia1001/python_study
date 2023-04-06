load( "./04_preprocess/04_preprocess.rdata")  # 실거래 불러오기          
apt_juso <- data.frame(apt_price$juso_jibun)  # 주소컬럼만 추출
apt_juso <- data.frame(apt_juso[!duplicated(apt_juso), ]) # unique 주소 추출 
head(apt_juso, 2)   # 추출결과 확인


#---------------------------------------
# 5-2 주소를 좌표로 변환하는 지오코딩
#---------------------------------------

#---# [1단계: 지오코딩 준비]

add_list <- list()   # 빈 리스트 생성
cnt <- 0             # 반복문 카운팅 초기값 설정
kakao_key = ""       # 인증키

#---# [2단계: 지오코딩]

library(httr)        # install.packages('httr')
library(RJSONIO)     # install.packages('RJSONIO')
library(data.table)  # install.packages('data.table')
library(dplyr)       # install.packages('dplyr')

for(i in 1:nrow(apt_juso)){ 
  #---# 에러 예외처리 구문 시작
  tryCatch(
    {
      #---# 주소로 좌표값 요청
      lon_lat <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
                     query = list(query = apt_juso[i,]),
                     add_headers(Authorization = paste0("KakaoAK ", kakao_key)))
      #---# 위경도만 추출하여 저장 
      coordxy <- lon_lat %>% content(as = 'text') %>% RJSONIO::fromJSON()
      #---# 반복횟수 카운팅
      cnt = cnt + 1
      #---# 주소, 경도, 위도 정보를 리스트로 저장
      add_list[[cnt]] <- data.table(apt_juso = apt_juso[i,], 
                                    coord_x = coordxy$documents[[1]]$x, 
                                    coord_y = coordxy$documents[[1]]$y)
      #---# 진행상황 알림 메시지
      message <- paste0("[", i,"/",nrow(apt_juso),"] 번째 (", 
                        round(i/nrow(apt_juso)*100,2)," %) [", apt_juso[i,] ,"] 지오코딩 중입니다: 
       X= ", add_list[[cnt]]$coord_x, " / Y= ", add_list[[cnt]]$coord_y)
      cat(message, "\n\n")
      #---# 예외처리 구문 종료
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
  )
}

#---# [3단계: 지오 코딩 결과 저장]

juso_geocoding <- rbindlist(add_list)   # 리스트를 데이터프레임 변환
juso_geocoding$coord_x <- as.numeric(juso_geocoding$coord_x) # 좌표값 숫자형 변환
juso_geocoding$coord_y <- as.numeric(juso_geocoding$coord_y)
juso_geocoding <- na.omit(juso_geocoding)   # 결측치 제거
dir.create("./05_geocoding")   # 새로운 폴더 생성
save(juso_geocoding, file="./05_geocoding/05_juso_geocoding.rdata") # 저장
write.csv(juso_geocoding, "./05_geocoding/05_juso_geocoding.csv")
