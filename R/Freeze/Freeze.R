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

head(original_4)
head(original_2)
head(original_5)
### 데이터 전처리
original_4 = original_4[c(2:5)]

names(original_2) <- c('houseCode','address', 'address2', 'supplyStatus','businessType','meterCode',
                       'meterNum','meterGauge','use','smallBlock','installDate','supplyDate','rupturePack')
names(original_3) <- c('houseCode','waterWayCode','xCor','yCor','long','lat')
names(original_4) <- c('waterWayCode','pipeType','pipeGauge','length')

original_5 <- join(original_3, original_4, by = 'waterWayCode')
original_6 <- join(original_5, original_2, by = 'houseCode')

original_6$length <- as.double(original_6$length)

allData <- original_6

head(original_1)
head(original_2)
head(original_3)
head(original_4)


original_1$수용가번호 <- gsub("-", "", original_1$수용가번호)
ruptureSorting <- original_1[original_1$중분류 == '동파',]
ruptureSorting$수용가번호 <- as.numeric(ruptureSorting$수용가번호)
str(ruptureSorting)

a = as.data.frame(table(ruptureSorting$수용가번호))


a$Freq = 1
colnames(a)=c("houseCode", "rupture")
#a = a[2:length(a$houseCode), ]
as.numeric(a$houseCode)
head(a)
str(a$houseCode)

allData = join(allData, a, by = 'houseCode')

allData$rupture[is.na(allData$rupture), ] = 0


#write.csv(x = allData, file = "allData.csv")
allData <- read.csv("allData.csv", stringsAsFactors = F)


### 지도위에 그리기
paju = get_map(location = c(lon = 126.83, lat = 37.85), zoom = 11, maptype = "terrain")
ggmap(paju, extent = "device")+
  geom_point(data = original_3, aes(x=long, y=lat), size = 0.01)




str(allData)
allData$pipeGauge = factor(allData$pipeGauge)
allData$pipeType = factor(allData$pipeType)
allData$businessType = factor(allData$businessType)
allData$rupture = factor(allData$rupture)
allData$rupturePack = factor(allData$rupturePack)
allData$meterGauge = factor(allData$meterGauge)

glm("rupture ~ pipeType + pipeGauge + meterGauge + rupturePack + businessType", data = allData)

allData$rupture






head(pip)

pip%>%
  ggplot(aes(X좌표, Y좌표))

ggplot(iris, aes(Sepal.Length, Petal.width))+
  geom_point()
