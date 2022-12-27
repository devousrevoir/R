### mid
Req_online <- Req_male_50 %>% filter(large == '온라인쇼핑') %>%
  group_by(date, mid) %>% summarise(n = length(mid)) %>% arrange(mid)

View(Req_online)

# 인터넷 쇼핑, 결제충전 두 항목의 6개월간 소비 건수 추이

# mid카테고리가 인터넷쇼핑인 소비 추이
## 2021 상반기 인터넷쇼핑
Req_online %>% filter(date >= '2021-01' & date <= '2021-06' & mid == '인터넷쇼핑') %>%
  ggplot() + geom_col(aes(date, n), fill = 'gray', col = 'white')

## 2022 상반기 인터넷쇼핑
Req_online %>% filter(date >= '2022-01' & date <= '2022-06' & mid == '인터넷쇼핑') %>%
  ggplot() + geom_col(aes(date, n), fill = 'gray', col = 'white')

# 2021 상반기 결제/충전
Req_online %>% filter(date >= '2021-01' & date <= '2021-06' & mid == '결제/충전') %>%
  ggplot() + geom_col(aes(date, n), fill = 'gray', col = 'white')

# 2022 상반기 결제/충전
Req_online %>% filter(date >= '2022-01' & date <= '2022-06' & mid == '결제/충전') %>%
  ggplot() + geom_col(aes(date, n), fill = 'gray', col = 'white')


# aes() : 괄호 안에 쓰게 되면 범례가 생성
# aes(), fill = 색깔, col = 테두리 색깔

# 카테고리별 구매 건수 변화 추이
Req_male_50_cate <- Req_male_50 %>% group_by(date, large) %>%
  summarise(n = length(large))
View(Req_male_50_cate)
class(Req_male_50_cate$date)

# 카테고리 -> mid로 필터
Req_male_50_midcate <- Req_male_50 %>% group_by(date, mid) %>%
  summarise(n = length(mid))
View(Req_male_50_midcate)
class(Req_male_50_midcate$date)

Req_male_50_midcate %>% filter(mid == '인터넷쇼핑')%>% ggplot() + 
  labs(title = '온라인쇼핑') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 7000)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")

Req_male_50_midcate %>% filter(mid == '결제/충전')%>% ggplot() + 
  labs(title = '온라인쇼핑') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 7000)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")