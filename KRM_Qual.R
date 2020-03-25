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
library(tidyverse)


#' #### **열 명칭별 의미**
#' 모든 항목 및 지표들은 표준화 적용 완료
#' 
#' * Name : 영문 시군명 (161개)
#' * NameK : 국문 시군명 (161개)
#' * SGG : 시군코드 
#' * X16_hazard : 16년도 hazard 지수(확률강우량) X16~18 
#' * X16_ex : 16년도 exposure 지수 X16~17
#' * X16_ex_str : 16년도 총건축물수 (normal) X16~17
#' * X16_ex_pop : 16년도 총인구수 (noamal) X16~17
#' * X16_ex_eco : 16년도 평균공시지가 (noamal) X16~17
#' * X16_vul : 16년도 vulnerability 지수  X16~17
#' * X16_vul_phy : 16년도 노후건축물비율 (normal) X16~17
#' * X16_vul_soc : 16년도 의존인구비율 (normal) X16~17
#' * X16_cap : 16년도 capacity 지수 X16~18
#' * X16_cap_phy : 16년도 방재시설 개수 (normal) X16~17
#' * X16_cap_soc : 16년도 소방경찰관서수 밀도 (normal) X16~17
#' * X16_cap_eco : 16년도 방재예산액 (normal) X16~17
#' * X16_result : 16년도 홍수피해위험지수 X16~17
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
summary(analysis_simp)
str(analysis_simp)


#+ fig.width=6, fig.height=6
tm_shape(analysis_simp)+
  tm_fill("NameK")+
  tm_borders()+
  tmap_options(max.categories = 161)+
  tm_layout(legend.position = c("right", "bottom"), inner.margins = 0.05)


#' **분류**
result <- c("X16_result", "X17_result")
haz <- c("X16_hazard", "X17_hazard", "X18_hazard")
expo <- c("X16_ex", "X17_ex")
vul <- c("X16_vul", "X17_vul")
cap <- c("X16_cap", "X17_cap", "X18_cap")


#' # Risk = (Hazard * Exposure * Vulnerability ) / Capacity
#' **Risk 요소별 Normalization 후 결과 분포**
#+ fig.width=12, fig.height=6
breaks <-  c(0, 0.2, 0.4, 0.6, 0.8, 1)
tm_shape(analysis_simp)+
  tm_polygons(result, breaks=breaks, palette="Reds")+
  tm_layout(title = "홍수위험지수")+
  tm_facets(nrow=1)

#+ fig.width=12, fig.height=4
tm_shape(analysis_simp)+
  tm_polygons(haz, breaks=breaks, palette="Oranges")+
  tm_layout(title = "Hazard Index")+
  tm_facets(nrow=1)

#+ fig.width=12, fig.height=6
tm_shape(analysis_simp)+
  tm_polygons(expo, breaks=breaks, palette="Greens")+
  tm_layout(title="Exposure Index")+
  tm_facets(nrow=1)

#+ fig.width=12, fig.height=6
tm_shape(analysis_simp)+
  tm_polygons(vul, breaks=breaks)+
  tm_layout(title="Vulnerability Index")+
  tm_facets(nrow=1)

#+ fig.width=12, fig.height=4
tm_shape(analysis_simp)+
  tm_polygons(cap, breaks=breaks, palette="Blues")+
  tm_layout(title="Capacity Index")+
  tm_facets(nrow=1)



#' # Risk요소 161개 지자체 순위 
#' Geometry를 제거하고 분석을 시작하였다. HTML출력을 위해 각 출력마다의 `fig.width`
#' `fig.height`를 달리하였다. 
analysis_df <- st_drop_geometry(analysis_simp)

#+ fig.width=12, fig.height=25
# hazard 지수
analysis_df %>% 
  select(NameK, all_of(haz)) %>% 
  pivot_longer(all_of(haz), names_to="year", values_to="indicator") %>% 
  group_by(NameK) %>% 
  summarise(mean=mean(indicator)) %>% 
  ggplot(aes(
    x=reorder(NameK, mean, FUN=desc),
    y=mean))+
  geom_point()+
  coord_flip()+
  labs(title = "Hazard Index")


#+ fig.width=12, fig.height=25
# exposure 지수
analysis_df %>% 
  select(NameK, all_of(expo)) %>% 
  pivot_longer(all_of(expo), names_to="year", values_to="indicator") %>% 
  group_by(NameK) %>% 
  summarise(mean=mean(indicator)) %>% 
  ggplot(aes(
    x=reorder(NameK, mean, FUN=desc),
    y=mean))+
  geom_point()+
  coord_flip()+
  labs(title = "Exposure Index")


#+ fig.width=12, fig.height=25
# vulnerability 지수
analysis_df %>% 
  select(NameK, all_of(vul)) %>% 
  pivot_longer(all_of(vul), names_to="year", values_to="indicator") %>% 
  group_by(NameK) %>% 
  summarise(mean=mean(indicator)) %>% 
  ggplot(aes(
    x=reorder(NameK, mean, FUN=desc),
    y=mean))+
  geom_point()+
  coord_flip()+
  labs(title = "Vulnerability Index")


#+ fig.width=12, fig.height=25
# capacity 지수
analysis_df %>% 
  select(NameK, all_of(cap)) %>% 
  pivot_longer(all_of(cap), names_to="year", values_to="indicator") %>% 
  group_by(NameK) %>% 
  summarise(mean=mean(indicator)) %>% 
  ggplot(aes(
    x=reorder(NameK, mean, FUN=desc),
    y=mean))+
  geom_point()+
  coord_flip()+
  labs(title = "Capacity Index")


#+ fig.width=12, fig.height=25
# 홍수피해위험지수
analysis_df %>% 
  select(NameK, all_of(result)) %>% 
  pivot_longer(all_of(result), names_to="year", values_to="indicator") %>% 
  group_by(NameK) %>% 
  summarise(mean=mean(indicator)) %>% 
  ggplot(aes(
    x=reorder(NameK, mean, FUN=desc),
    y=mean))+
  geom_point()+
  coord_flip()+
  labs(title = "홍수피해위험지수")


#' # 각 indicator들의 분포
#+ fig.width=12, fig.height=25
# 총건축물
analysis_df %>% 
  select(NameK, c(X16_ex_str, X17_ex_str)) %>% 
  pivot_longer(c(X16_ex_str, X17_ex_str),
               names_to="year", values_to="indicator") %>% 
  group_by(NameK) %>% 
  ggplot(aes(NameK,indicator))+
  geom_col(aes(fill=year), position="dodge")+
  coord_flip()

  