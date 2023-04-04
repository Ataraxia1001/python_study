load("./04_preprocess/04_preprocess.rdata")
load("./05_geocoding/05_juso_geocoding.rdata")
library(dplyr)
apt_price <- left_join(apt_price, juso_geocoding,
                       by= c('juso_jubun' = "apt_juso"))
apt_price <- na.omit(apt_price)
# 지오 데이터 프레임 만들기
install.packages('sp')
library(sp)
install.packages('sf')
library(sf)
coordinates(apt_price) <- ~coord_x + coord_y #좌표값 할당
proj4string(apt_price) <- "+proj=longlat +datum=WGS84 +no_defs" #좌표계 (CRS)정의
apt_price <- st_as_sf(apt_price) # sp형 -> sf형으로 변환
# 지오데이터프레임 시각화
plot(apt_price$geometry, axes = T, pch=1)

library(leaflet) # install.packages("leaflet") 지도에 그리는 라이브러리
leaflet() %>% addTiles() %>% 
  addCircleMarkers(data=apt_price[1:1000, ], label=~apt_nm)

dir.create("06_geodataframe")
save(apt_price, file = "./06_geodataframe/06_apt_price.rdata")
write.csv(apt_price, "./06_geodataframe/06_apt_price.csv")
