# install.packages("jsonlite","dplyr","stringr")
library(jsonlite)
library(dplyr)
library(stringr)

setwd('C:\\Users\\AdityendraSuman\\Downloads\\')

x = read.csv("pincode_latest.csv") %>% 
  rename(C = Country, S = State, D = District, P = Pincode)

y1 = minify(toJSON(x))

nchar(y1)
writeLines(y1, "pincode_latest_json_short.txt")

#=======================

x <- x %>% 
  select(sort("C"), sort("S"), sort("D"), sort("P"))

cs <- x %>% select(C) %>% unique() %>% unlist()
cou = ""
for (c in cs){
  ss <- x %>% filter(C == c) %>% select(S) %>% unique() %>% unlist()
  sts = ""
  for (s in ss){
    ds <- x %>% filter(S == s) %>% select(D) %>% unique() %>% unlist()
    dis = ""
    for (d in ds){
      ps <- x %>% filter(D == d) %>% select(P) %>% unique() %>% unlist()
      pins = ""
      for (p in ps){
        pins <- paste0(pins, ',{"p":',p,'}')
      }
      pins <- str_sub(pins, start = 2)
      dis <- paste0(dis, ',{"d":"',d,'","P":[',pins,']}')
    }
    dis <- str_sub(dis, start = 2)
    sts <- paste0(sts, ',{"s":"',s,'","D":[',dis,']}')
  }
  sts <- str_sub(sts, start = 2)
  cou <- paste0(cou, ',{"c":"',c,'","S":[',sts,']}')
}
cou <- str_sub(cou, start = 2)

y2 = paste0('{"C":[',cou,']}')

nchar(y2)
writeLines(y2, "pincode_latest_json_long.txt")

#=======================

c_s <- x %>% 
  select (C, S) %>% 
  unique() %>% 
  group_by(C) %>% 
  summarise(S = paste(S,collapse = '","')) %>% 
  mutate(S = paste0('{"', S, '"}')) %>% 
  mutate(Type = "Country") %>% 
  select(Type, X = C, Y = S)

s_d <- x %>% 
  select (S, D) %>% 
  unique() %>% 
  group_by(S) %>% 
  summarise(D = paste(D,collapse = '","')) %>% 
  mutate(D = paste0('{"', D, '"}')) %>% 
  mutate(Type = "State") %>% 
  select(Type, X = S, Y = D)

d_p <- x %>% 
  select (D, P) %>% 
  unique() %>% 
  group_by(D) %>% 
  summarise(P = paste(P,collapse = ',')) %>% 
  mutate(P = paste0('{', P, '}')) %>% 
  mutate(Type = "District") %>% 
  select(Type, X = D, Y = P)

y3 <- rbind(c_s, s_d, d_p) %>% 
  write.csv("pincode_latest_map.csv", row.names = F)

#---------------------  
# {
#   "C":[
#         {
#           "c":"INDIA",
#           "S":[
#             {
#               "st":"BIHAR",
#               "D":[
#                 {
#                   "di":"GAYA",
#                   "P": [
#                     {"Pi": 123456},
#                     {"Pi": 123456},  
#                     ...
#                   ]
#                 },
#                 {"Di":},
#                 
#               ]
#             }
#           ]
#         },
#         
#         ...
#         
#    ]
# }

#-------------------------------------
"INDIA": ["S1": {"D1":{P1, P2, P3}, "D2":{P4, P5, P6}, ...}, "S2": {"D1":{P1, P2, P3}, "D2":{P4, P5, P6}, ...}]


