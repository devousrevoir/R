# 필요한 패키지 장착
library(dplyr)
library(ggplot2)
library(showtext)
library(readxl)

# 구매데이터 불러오기
Req = read.csv("C:/k_digital/source/r_source/R miniproject/req_data.csv")
View(Req)


## 날짜를 '-'단위로 자르기
date <- data.frame(do.call(rbind, strsplit(Req$pay_date, split = '-', fixed =T)))
View(date)

## 연도와 월을 붙이기
date <- data.frame(date = paste(date$X1, date$X2, sep = '-'))
View(date)

## 필요한 열의 정보만 가져와서 연결하기

Req <- data.frame(Req$age_group, date, Req$amount,
                  Req$pay_place, Req$large, Req$mid)

names(Req) <- c('age_group', 'date', 'amount', 'pay_place', 'large', 'mid')

View(Req)


# 카테고리별 구매 건수 변화 추이
Req_totcate <- Req %>% group_by(date, large, mid) %>% summarise(n = length(mid))
View(Req_totcate)

### mid : 스포츠/레저, 식사, 의료/건강
Req_sports <- Req_totcate %>% filter(large == '스포츠/레저' & date >= '2021-01' & date <= '2021-12')
Req_meal <- Req_totcate %>% filter(large == '식사' & date >= '2021-01' & date <= '2021-12')
Req_health <- Req_totcate %>% filter(large == '의료/건강' & date >= '2021-01' & date <= '2021-12')

# 스포츠/레저
Req_sports %>% ggplot() + 
  labs(title = '스포츠/레저') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n, group = mid, col = mid)) +
  geom_line(aes(x = date, y = n, group = mid, col = mid), size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 5500))

# 식사
Req_meal %>% ggplot() + 
  labs(title = '식사') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n, group = mid, col = mid)) +
  geom_line(aes(x = date, y = n, group = mid, col = mid), size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 25000))

# 의료/건강
Req_health %>% ggplot() + 
  labs(title = '의료/건강') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n, group = mid, col = mid)) +
  geom_line(aes(x = date, y = n, group = mid, col = mid), size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 5200))
