rm(list = ls())
library(sf)
load("06_geodataframe/06_apt_price.rdata") #실거래가
load("07_map/07_kde_high.rdata") #최고가 레스터 이미지
grid <- st_read("R/seoul/seoul.shp") # 1km 그리드
install.packages("tmap")
library(tmap)
tmap_mode("view")
tm_shape(grid) + tm_borders() + tm_text("ID", col = "red")+
  tm_shape(raster_high) + tm_raster(palette = c("blue", "green", "yellow", "red"), alpha = .4) +   tm_basemap(server = c("OpenStreetMap"))

# 전체지역 / 관심지역 저장
library(dplyr)
apt_price <- st_join(apt_price, grid, join = st_intersects) #실거래 + 그리드
apt_price <- apt_price %>% st_drop_geometry() # 실거래 공간 속성 지우기
all <-  apt_price 
sel <- apt_price %>% filter(ID == 81016)
dir.create("./08_chart")
save(all, file = "./08_chart/all.rdata")
save(sel, file = "./08_chart/sel.rdata")
rm(list = ls())
