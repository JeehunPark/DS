library(maptools)
library(ggplot2)
library(stringr)
library(sp)
library(dplyr)
library(rgdal)
library(proj4)


# 지역코드
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
result = result[1:8]
result["sub"] = str_extract(result$A1, "(UQA).{3}")

result = left_join(result, code, by="sub")

#
Spatial_point = SpatialPointsDataFrame(original_3[3:4], original_3[1], match.ID = "houseCode")
head(Spatial_point)

det = over(Spatial_point, use_city)
det = det[1:3]
det = na.omit(det)

original_3["A0"] = 1:length(original_3$houseCode)
result = full_join(original_3, det, by="A0")
result = result[1:8]
result["sub"] = str_extract(result$A1, "(UQA).{3}")

result = left_join(result, code, by="sub")

ggplot()+
  geom_polygon(aes(city$long, city$lat, group=city$group, fill = city$name), color="black")+
  geom_point(aes(result$xCor, result$yCor, color = result$name), size = 0.1)+
  coord_cartesian(xlim = c(170000, 180000), ylim = c(465000, 475000))+
  xlab("long")+
  ylab("lat")+
  labs(fill = "legend", color = "legend")



typeof(use_city)
head(result)
head(det)
proj_Bessel = "+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +units=m +no_defs"
proj_wgs = "+proj=tmerc +lat_0=38 +lon_0=127.0028902777778 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel +units=m +no_defs +towgs84=-115.80,474.99,674.11,1.16,-2.31,-1.63,6.43"
a = proj4::project(original_3[5:6], proj_Bessel)

proj4::ptransform(data = original_3[5:6], dst.proj = "TM")

str(use_city)
use_city@data[2]

z = use_city@polygons
z[2]

head(str(use_city))
head(det)
head(original_3)
conVert

sp::`.__T__over:sp`






convertCoordSystem <- function(long, lat, from.crs, to.crs){
  xy <- data.frame(long=long, lat=lat)
  coordinates(xy) <- ~long+lat
  
  from.crs <- CRS(from.crs)
  from.coordinates <- SpatialPoints(xy, proj4string=from.crs)
  
  to.crs <- CRS(to.crs)
  changed <- as.data.frame(SpatialPoints(spTransform(from.coordinates, to.crs)))
  names(changed) <- c("long", "lat")
  
  return(changed)
}
from.crs = "+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=GRS80 +units=m +no_defs"
to.crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
a = convertCoordSystem(original_3$xCor, original_3$yCor, from.crs, to.crs)
head(a)
original_3$xCor
