#'---
#'title: "Qualitative Risk Analysis"
#'author: "Kyungtak Kim"
#'date: '2020 3 25 '
#'output:
#'  html_document:
#'    keep_md: TRUE
#'   
#'    
#'  
#'---


#+ libaray, warning=FALSE, message=FALSE
library(rgdal)
library(sf)
library(lwgeom) #st_make_valid
library(tmap)


#' #### **열 명칭별 의미**
#' 모든 항목 및 지표들은 표준화 적용 완료
#' 
#' * X16_hazard : 16년도 hazard 지수(확률강우량)  
#' * X17_hazard : 17년도 hazard 지수(확률강우량)    
#' * X18_hazard : 18년도 hazard 지수(확률강우량)  
#' * X16_ex : 16년도 exposure 지수
#' * X17_ex : 17년도 exposure 지수 
#' * X16_ex_str : 16년도 총건축물수
#' * X16_ex_pop : 16년도 총인구수
#' * X16_ex_eco : 16년도 평균공시지가 
#' * X16_vul : 16년도 vulnerability 지수
#' * X17_vul : 17년도 vulnerability 지수
#' * X16_vul_phy : 16년도 노후건축물비율
#' * X16_vul_soc : 16년도 의존인구비율
#' * X16_cap : 16년도 capacity 지수
#' * X17_cap : 17년도 capacity 지수
#' * X18_cap : 18년도 capacity 지수
#' * X16_cap_phy : 16년도 방재시설 개수
#' * X16_cap_soc : 16년도 소방경찰관서수 밀도
#' * X16_cap_eco : 16년도 방재예산액
#' * X16_result : 16년도 홍수피해위험지수
#' * X17_result : 17년도 홍수피해위험지수
#'  
#' 
# 시군 shp file 불러오기
analysis <- rgdal::readOGR('input/analysis.shp')
analysis_sf <- st_as_sf(analysis)
# polygon error check
# st_is_valid(analysis_sf)
analysis_sf <- st_make_valid(analysis_sf)
# st_is_valid(analysis_sf)
# tmap loading 과 그림속도 증가를 위해 polygon simplify
analysis_simp <- st_simplify(analysis_sf, dTolerance = 50)


#' 속성 확인
# summary(analysis_simp)
# str(analysis_simp)


#+ fig.width=6, fig.height=6
tm_shape(analysis_simp)+
  tm_fill("region161")+
  tm_borders()+
  tmap_options(max.categories = 161)


#+ fig.width=12, fig.height=12
breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)
facets=c("X16_cap", "X17_cap", "X18_cap")
tm_shape(analysis_simp)+
  tm_polygons(facets, breaks=breaks)+
  tm_facets(nrow=2)

