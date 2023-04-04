# 7-3  우리동네가 옆 동네보다 비쌀까?
# 1단계 데이터 준비
rm(list = ls())
load("./06_geodataframe/06_apt_price.rdata")
load("07_map/07_kde_high.rdata")
load("07_map/07_kde_hot.rdata")
# 이상치 설정 (평당가격의 하위10%, 상위 90%)
pcnt_10 <- as.numeric(quantile(apt_price$py, probs = seq(.1, .9, by= .1))[1])
pcnt_90 <- as.numeric(quantile(apt_price$py, probs = seq(.1, .9, by= .1))[9])
# 마커 클러스터링 함수
load("circle_marker/circle_marker.rdata")
library(sf)
grid <- st_read("R/seoul/seoul.shp") #서울시 1km격자 
bnd <- st_read("R/sigun_bnd/seoul.shp") #서울시 경계선
# 마커 클러스터링 컬러 설정 : 상(r),중(g),하(b)
circle.colors <- sample(x =c("red","green", "blue"),
                        size = 1000, replace = TRUE)
# 마커 클러스터링 시각화
library(purrr)
leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=bnd, weight = 3, color = "red", fill = NA) %>% 
  addRasterImage(raster_high, 
                 colors = colorNumeric(c("blue","green", "yellow","red"), values(raster_high),
                                       na.color ="transparent"), opacity = 0.4, group = "2021 최고가") %>% 
  addRasterImage(raster_hot, 
                 colors = colorNumeric(c("blue","green", "yellow","red"), values(raster_hot),
                                       na.color ="transparent"), opacity = 0.4, group = "2021 급등지") %>%
  addCircleMarkers(data = apt_price, lng =unlist(map(apt_price$geometry, 1)), lat =unlist(map(apt_price$geometry, 2)),
                        radius = 10, stroke = FALSE, fillOpacity = 0.6, fillColor = circle.colors, weight =apt_price$py,
                   clusterOptions = markerClusterOptions(iconCreateFunction=JS(avg.formula)))


