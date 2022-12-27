# 카테고리별 구매 건수 변화 추이
Req_male_50_cate <- Req_male_50 %>% group_by(date, mid) %>%
  summarise(n = length(mid))
View(Req_male_50_cate)
class(Req_male_50_cate$date)

### large

Req_male_50 <- data.frame(Req_male$age_group, date, Req_male$amount, 
                          Req_male$pay_place_name, Req_male$large_category)

names(Req_male_50) <- c('age_group', 'date', 'amount', 'pay_place', 'large')

View(Req_male_50)


### mid

Req_male_50_2 <- data.frame(Req_male$age_group, date, Req_male$amount, 
                            Req_male$pay_place_name, Req_male$mid_category)

names(Req_male_50_2) <- c('age_group', 'date', 'amount', 'pay_place', 'mid')

View(Req_male_50_2)

Req_male_50 <- Req_male_50 %>% arrange(age_group, date, large)
Req_male_50_2 <- Req_male_50_2 %>% arrange(age_group, date, mid)
View(Req_male_50)

# 6개월 단위 카테고리별 구매 건수 현황 - 식사, 인터넷쇼핑

Req_male_50_2  %>% filter(date >= '2021-01' & date <= '2021-06' & mid == '식사') %>%
  ggplot() + geom_bar(aes(mid, fill = date),color = 'purple', position = 'dodge')

Req_male_50_2  %>% filter(date >= '2022-01' & date <= '2022-06' & mid == '식사') %>%
  ggplot() + geom_bar(aes(date, fill = mid),color = 'purple', position = 'dodge')

# 카테고리별 구매 건수 변화 추이
Req_male_50_cate <- Req_male_50 %>% group_by(date, large) %>%
  summarise(n = length(large))
View(Req_male_50_cate)
class(Req_male_50_cate$date)



# mid - 식사, 온라인쇼핑 구분

meal <- distinct(Req_male_50 %>% filter (large == '식사') %>% select(mid))
View(meal)
# 고기, 기타, 배달, 분식, 뷔페, 식재료, 아시아음식, 양식, 
# 일반식당, 일식, 중식, 치킨, 패밀리레스토랑, 패스트푸드, 한식

shop <- distinct(Req_male_50 %>% filter (large == '온라인쇼핑') %>% select(mid))
View(shop)
# 결제/충전, 기타, 앱스토어, 인터넷쇼핑, 카드포인트, 홈쇼핑

# 식사 2021, 2022 상반기

Req_male_50_cate %>% filter(mid == '고기')%>% ggplot() + 
  labs(title = '식사 - 고기') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 10000)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")

Req_male_50_cate %>% filter(mid == '기타')%>% ggplot() + 
  labs(title = '식사 - 기타') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 10000)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")

Req_male_50_cate %>% filter(mid == '배달')%>% ggplot() + 
  labs(title = '식사 - 배달') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 10000)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")

Req_male_50_cate %>% filter(mid == '분식')%>% ggplot() + 
  labs(title = '식사 - 분식') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 10000)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")

Req_male_50_cate %>% filter(mid == '뷔페')%>% ggplot() + 
  labs(title = '식사 - 뷔페') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 10000)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")

Req_male_50_cate %>% filter(mid == '식재료')%>% ggplot() + 
  labs(title = '식사 - 식재료') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 10000)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")

Req_male_50_cate %>% filter(mid == '아시아음식')%>% ggplot() + 
  labs(title = '식사 - 아시아음식') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 10000)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")

Req_male_50_cate %>% filter(mid == '양식')%>% ggplot() + 
  labs(title = '식사 - 양식') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 10000)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")

Req_male_50_cate %>% filter(mid == '일반식당')%>% ggplot() + 
  labs(title = '식사 - 일반식당') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 10000)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")

Req_male_50_cate %>% filter(mid == '일식')%>% ggplot() + 
  labs(title = '식사 - 일식') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 10000)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")

Req_male_50_cate %>% filter(mid == '중식')%>% ggplot() + 
  labs(title = '식사 - 중식') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 10000)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")

Req_male_50_cate %>% filter(mid == '치킨')%>% ggplot() + 
  labs(title = '식사 - 치킨') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 10000)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")

Req_male_50_cate %>% filter(mid == '패밀리레스토랑')%>% ggplot() + 
  labs(title = '식사 - 패밀리레스토랑') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 10000)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")

Req_male_50_cate %>% filter(mid == '패스트푸드')%>% ggplot() + 
  labs(title = '식사 - 패스트푸드') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 10000)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")

Req_male_50_cate %>% filter(mid == '한식')%>% ggplot() + 
  labs(title = '식사 - 한식') +
  geom_col(width = 0.8,aes(x = date, y = n), fill = '#E3BFB7') +
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n), col = 'red') +
  geom_line(aes(x = date, y = n, group = 1), col = 'red', size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 10000)) +
  theme_void() +
  theme(
    axis.line.x = element_line(size = 1.2), 
    axis.text.x = element_text(margin = margin(.3,0,0,0, "cm"), size = 13),
    plot.margin = margin(.3,1,.7,1, "cm"),
    legend.position = "none")

# 온라인쇼핑

Req_male_50_cate %>% filter(mid == '온라인쇼핑')%>% ggplot() + 
  labs(title = '결제/충전') +
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