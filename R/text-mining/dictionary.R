library(dplyr)
library(httr)
library(rvest)
library(stringr)
library(data.table)

gc(verbose = T)

# 크롤러-1 -----
URL_1 = "http://www.materic.or.kr/dictionary/word/content.asp?id=65633&page=1&kch=&ech=&s_kinds=&s_word="
dic = vector()

for(i in 65633:2)
{
  
  URL_1 = modify_url(URL_1, query = list(id=i))
  dic_temp = read_html(URL_1, options = "RECOVER")%>%
    html_nodes(".td_02")%>%
    html_text()
  
  dic = append(x = dic, values = dic_temp)
  print(i)
}


# 크롤러-2 -----
URL_2 = "http://www.materic.or.kr/dictionary/word/list.asp?page=1&kch=&ech=&s_kinds=&s_word="
dic = vector()

for(i in 1:4334)
{
  URL_2 = modify_url(URL_2, query = list(page=i))
  dic_temp = read_html(URL_2, options = "RECOVER")%>%
    html_nodes(".dic_03")%>%
    html_nodes("td")%>%
    html_text()
  
  dic = append(x = dic, values = dic_temp)
  print(i)
}


write.csv(dic, "dic.csv", row.names = F)


# 한글단어만 추출 -----
dic_table = fread("dic.csv")

even = 1:69335*2

for(i in even)
{
  if(dic_table$x[i]=="")
  {
    dic_table$x[i]=dic_table$x[i-1]
  }
}


even_dic = dic_table[even]

View(even_dic)

dic = even_dic[!even_dic$x %in% "국문명"]
write.csv(dic$x, "dic_2.csv", row.names = F)
View(dic)

