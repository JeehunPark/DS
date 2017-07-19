## 라이브러리 로드
library(rmarkdown)
library(ggplot2)
library(ggmap)
library(dplyr)
library(plyr)
library(stringr)
library(pscl)

## 워킹디렉토리 설정
setwd("C:/Users/Administrator/DSS/R/Freeze/DataSet")

## 데이터 로드
original_1 = read.csv("01_파주_민원현황(2010년~현재).csv", stringsAsFactors = F)
original_2 = read.csv("02_파주-수도미터.csv", stringsAsFactors = F)
original_3 = read.csv("03_수도미터위치정보+매칭.csv", stringsAsFactors = F)
original_4 = read.csv("04_파주-급수관로.csv", stringsAsFactors = F)

weather_10 = read.csv("2010_파주_기온.csv", stringsAsFactors = F)
weather_11 = read.csv("2011_파주_기온.csv", stringsAsFactors = F)
weather_12 = read.csv("2012_파주_기온.csv", stringsAsFactors = F)
weather_13 = read.csv("2013_파주_기온.csv", stringsAsFactors = F)
weather_14 = read.csv("2014_파주_기온.csv", stringsAsFactors = F)
weather_15 = read.csv("2015_파주_기온.csv", stringsAsFactors = F)
weather_16 = read.csv("2016_파주_기온.csv", stringsAsFactors = F)
weather_17 = read.csv("2017_파주_기온.csv", stringsAsFactors = F)

## 데이터 전처리
# 1. 날씨 데이터 전처리
weather = rbind(weather_10[2:7], weather_11[2:7], weather_12[2:7], weather_13[2:7], 
                weather_14[2:7], weather_15[2:7], weather_16[2:7], weather_17[2:7])
weather$일시 = strptime(weather$일시, "%Y-%m-%d")
colnames(weather) = c("일시", "평균기온", "최저기온", "최저기온_시각", "최고기온", "최고기온_시각")

rm(weather_10, weather_11, weather_12, weather_13, weather_14, weather_15, weather_16, weather_17)

# 2. 기본데이터 전처리
head(original_1)
head(original_2)
head(original_3)
head(original_4)

##original_1 전처리
original_1$신청일시 = strptime(original_1$신청일시, "%Y-%m-%d")
original_1$수용가번호 = gsub("-", "", original_1$수용가번호)

count = count(original_1$수용가번호)[2:length(count(original_1$수용가번호)$x), ]
colnames(count) = c("수용가번호", "횟수")
original_1 = join(original_1, count, by="수용가번호")
original_1$횟수[is.na(original_1$횟수)] = 0

head(original_1)




length(original_2$수용가번호)
length(original_3$X좌표)
length(original_4$X)

original_4[count(original_4$관리번호)$freq>1, ]
original_3==original_4
