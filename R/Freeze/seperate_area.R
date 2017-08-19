library(maptools)
library(ggplot2)
library(stringr)
library(sp)
library(dplyr)
library(rgdal)


setwd("C:/Users/a/Desktop/용도지역")
code = read.csv("code.csv")
head(code)


# 관리지역
setwd("C:/Users/a/Desktop/용도지역/국토계획-관리/")
use_care = readShapePoly("AL_00_D125_20170805.shp", delete_null_obj = T)
use_care_1 = fortify(use_care)
df_use_care = as.data.frame(use_care)
df_use_care = df_use_care["A1"]
df_use_care["id"] = as.character(0:(length(df_use_care$A1)-1))
df_use_care["sub"] = regmatches(df_use_care$A1, regexpr("(UQB).{3}", df_use_care$A1))
df_use_care = left_join(df_use_care, code, by="sub")

care = left_join(use_care_1, df_use_care, by="id")
write.csv(care, "care.csv")
head(care)

# 농림지역
setwd("C:/Users/a/Desktop/용도지역/국토계획-농림/")
use_farm = readShapePoly("AL_00_D126_20170805.shp", delete_null_obj = T)
use_farm_1 = fortify(use_farm)
df_use_farm = as.data.frame(use_farm)
df_use_farm = df_use_farm["A1"]
df_use_farm["id"] = as.character(0:(length(df_use_farm$A1)-1))
df_use_farm["sub"] = regmatches(df_use_farm$A1, regexpr("(UQC).{3}", df_use_farm$A1))
df_use_farm = left_join(df_use_farm, code, by="sub")

farm = left_join(use_farm_1, df_use_farm, by="id")
write.csv(farm, "farm.csv")
head(farm)


# 도시지역
setwd("C:/Users/a/Desktop/용도지역/국토계획-도시/")
use_city = readShapePoly("AL_00_D124_20170805.shp", delete_null_obj = T)
use_city_1 = fortify(use_city)
df_use_city = as.data.frame(use_city)
df_use_city = df_use_city["A1"]
df_use_city["id"] = as.character(0:(length(df_use_city$A1)-1))
df_use_city["sub"] = regmatches(df_use_city$A1, regexpr("(UQA).{3}", df_use_city$A1))
df_use_city = left_join(df_use_city, code, by="sub")

city = left_join(use_city_1, df_use_city, by="id")
write.csv(city, "city.csv")
head(city)


# 자연환경보전지역
setwd("C:/Users/a/Desktop/용도지역/국토계획-자연환경보전/")
use_nature = readShapePoly("AL_00_D127_20170805.shp", delete_null_obj = T)
use_nature_1 = fortify(use_nature)
df_use_nature = as.data.frame(use_nature)
df_use_nature = df_use_nature["A1"]
df_use_nature["id"] = as.character(0:(length(df_use_nature$A1)-1))
df_use_nature["sub"] = regmatches(df_use_nature$A1, regexpr("(UQD).{3}", df_use_nature$A1))
df_use_nature = left_join(df_use_nature, code, by="sub")

nature = left_join(use_nature_1, df_use_nature, by="id")
write.csv(nature, "nature.csv")
head(nature)


# 그리기-1(care:yellow, farm:brown, city: blue, nature: green)
ggplot()+
  geom_polygon(aes(use_care_1$long, use_care_1$lat, group=use_care_1$group), fill = "#dce504")+
  geom_polygon(aes(use_farm_1$long, use_farm_1$lat, group=use_farm_1$group), fill = "#412e00")+
  geom_polygon(aes(use_city_1$long, use_city_1$lat, group=use_city_1$group), fill = "#21b9f3")+
  geom_polygon(aes(use_nature_1$long, use_nature_1$lat, group=use_nature_1$group), fill = "#41de00")+
  geom_point(aes(original_3$X좌표, original_3$Y좌표), color = "red", size = 0.1)+
  coord_cartesian(xlim = c(170000, 205000), ylim = c(465000, 505000))+
  xlab("long")+
  ylab("lat")


# 그리기-2(care:yellow, farm:brown, city: blue, nature: green) 지역세분화
ggplot()+
  geom_polygon(aes(care$long, care$lat, group=care$group, fill = care$name))+
  geom_polygon(aes(farm$long, farm$lat, group=farm$group, fill = "#412e00"))+
  geom_polygon(aes(city$long, city$lat, group=city$group, fill = "#21b9f3"))+
  geom_polygon(aes(nature$long, nature$lat, group=nature$group, fill = "#41de00"))+
  geom_point(aes(original_3$X좌표, original_3$Y좌표), color = "red", size = 0.1)+
  coord_cartesian(xlim = c(170000, 205000), ylim = c(465000, 505000))+
  xlab("long")+
  ylab("lat")+
  labs(fill = "legend")

ggplot()+
  geom_polygon(aes(care$long, care$lat, group=care$group, fill = care$name))+
  geom_polygon(aes(farm$long, farm$lat, group=farm$group, fill = farm$name))+
  geom_polygon(aes(city$long, city$lat, group=city$group, fill = city$name))+
  geom_polygon(aes(nature$long, nature$lat, group=nature$group, fill = nature$name))+
  geom_point(aes(original_3$X좌표, original_3$Y좌표), color = "red", size = 0.1)+
  coord_cartesian(xlim = c(170000, 205000), ylim = c(465000, 505000))+
  xlab("long")+
  ylab("lat")+
  labs(fill = "legend")



over(coordinates(original_3[3:4]), care)

plot(coordinates(original_3[3:4]))
head(care)
corr


setwd("C:/Users/a/Desktop/용도지역/국토계획-관리/")
care = read.csv("care.csv")

setwd("C:/Users/a/Desktop/용도지역/국토계획-농림/")
farm = read.csv("farm.csv")

setwd("C:/Users/a/Desktop/용도지역/국토계획-도시/")
city = read.csv("city.csv")

setwd("C:/Users/a/Desktop/용도지역/국토계획-자연환경보전/")
nature = read.csv("nature.csv")


# 지역 판별
Spatial_point = SpatialPointsDataFrame(original_3[3:4], original_3[1])
head(Spatial_point)

det = over(Spatial_point, use_city)
det = det[1:3]
det = na.omit(det)

original_3["A0"] = 1:length(original_3$houseCode)
result = full_join(original_3, det, by="A0")
result$A1 = as.character(result$A1)
result["sub"] = regmatches(result$A1, regexpr("(UQA).{3}", result$A1))
df_use_city = left_join(df_use_city, code, by="sub")

length(det$A0)
length(result$houseCode)
View(result)
length(df_use_nature$A1)
length(result$houseCode)
length(original_3$houseCode)
str(result)
head(result)
head(det)
head(use_city)
head(original_3)
head(city)
city[city$name ==NA,]
result[result$A1==NA,]
head(det)
head(Spatial_point)
original_3[11521,]
Spatial_point[11521,]

ggplot()+
  geom_polygon(aes(city$long, city$lat, group=city$group, fill = city$name))+
  geom_point(aes(original_3[11521,]$xCor, original_3[11521,]$yCor), color = "red", size = 1)+
  coord_cartesian(xlim = c(170000, 205000), ylim = c(465000, 505000))+
  xlab("long")+
  ylab("lat")+
  labs(fill = "legend")

plot(use_city)

