### 라이브러리 로드
library(rmarkdown)
library(ggplot2)
library(ggmap)
library(dplyr)
library(plyr)
library(stringr)

### 데이터 로드
setwd("C:/Users/Administrator/DSS/R/Freeze/DataSet")
original_1 = read.csv("01_파주_민원현황(2010년~현재).csv", stringsAsFactors = F)
original_2 = read.csv("02_파주-수도미터.csv", stringsAsFactors = F)
original_3 = read.csv("03_수도미터위치정보+매칭.csv", stringsAsFactors = F)
original_4 = read.csv("04_파주-급수관로.csv", stringsAsFactors = F)

### 데이터 전처리
original_4 = original_4[c(2:5)]
head(original_1)
head(original_2)
head(original_3)
head(original_4)


names(original_2) <- c('houseCode','address', 'address2', 'supplyStatus','businessType','meterCode',
                       'meterNum','meterGauge','use','smallBlock','installDate','supplyDate','rupturePack')
names(original_3) <- c('houseCode','waterWayCode','xCor','yCor','long','lat')
names(original_4) <- c('waterWayCode','pipeType','pipeGauge','length')

original_5 <- join(original_3, original_4, by = 'waterWayCode')
original_6 <- join(original_5, original_2, by = 'houseCode')

original_6$length <- as.double(original_6$length)

allData <- original_6

write.csv(x = allData, file = "allData.csv")


original_1$수용가번호 <- gsub("-", "", original_1$수용가번호)




ruptureSorting <- original_1[original_1$중분류 == '동파',]

head(ruptureSorting)


a = as.data.frame(table(ruptureSorting$수용가번호))


a$Var1 <- as.character(a$Var1)
a$Var1 = gsub("-", "", a$Var1)
head(a)

b = filter(a, Freq>4)
c = b[2:length(b), "Var1"]
c
str_replace(c, "-", "")
head(allData)
allData[, "houseCode"==c]

filter(a, Freq>=3)

View(a)
head(ruptureSorting)

table(original_1$중분류)
original_1[original_1$상태 == '반려',]$민원내용%>%
  grep("동파", ., value=T)




head(original_5)
paju = get_map(location = "paju, South Korea", zoom = 11, maptype = "roadmap")

ggmap(paju)+geom_point(data = original_3, aes(x=long, y=lat), size = 0.01)







head(pip)

pip%>%
  ggplot(aes(X좌표, Y좌표))

ggplot(iris, aes(Sepal.Length, Petal.width))+
  geom_point()
