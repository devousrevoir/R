purchase_table$Sales_Unit <- as.numeric(purchase_table$Sales_Unit)

# 1. 결측치, 이상치 제거
purchase_1 <-  purchase_table %>% filter(MasterCategoryFullName != 'Unknown' & Price > 100 )
View(purchase_1)


# 2. 월별 구매 건수추이
## 날짜를 '-'단위로 자르기
class(purchase_1$Purchase_Date)
purchase_1$Purchase_Date <- as.character(purchase_1$Purchase_Date)
date <- data.frame(do.call(rbind, strsplit(purchase_1$Purchase_Date, split = '-', fixed =T)))
View(date)

## 연도와 월을 붙이기
date <- data.frame(date = paste(date$X1, date$X2, sep = '-'))
View(date)



# 3. Tot_price 변수 생성
Tot_price <- data.frame(Tot_price = purchase_1$Sales_Unit * purchase_1$Price)
View(Tot_price)



# 4. 카테고리 분류
# 카테고리를 s1 변수에 담기
s1 = data.frame(category = purchase_1$MasterCategoryFullName)
View(s1)
class(s1$category)

## 카테고리를 '->' 기준으로 자르기
s1_split <- data.frame(do.call(rbind, strsplit(s1$category,split = '->',fixed = T)))
View(s1_split)


## 5. 데이터셋 생성
## 필요한 열의 정보만 가져와서 연결하기기
purchase <- data.frame(date,
                       purchase_1$Sales_Unit,
                       purchase_1$Price,
                       Tot_price,
                       s1_split)
names(purchase) <- c('date', 'sales_Unit', 'price', 'tot', 'large', 'mid', 'small')
purchase <- purchase %>% arrange(date, large, mid, small)
View(purchase)



# =====================================================================

### 카테고리 분류
purchase_large <- purchase %>% 
  group_by(date, large) %>%
  summarise(n = length(large)) %>%
  arrange(large)

View(purchase_large)

# 카테고리별 구매 건수 변화 추이
purchase_large <- purchase %>%
  group_by(date, large) %>%
  summarise(n = length(large))
View(purchase_large)
class(purchase_large)


# 가구 수납 조명 보수
# 침구 데코 원예
# 치킨 초밥 베이커리
# 세제 탈취 제습
# 언더웨어 양말

# 가구 · 수납 · 조명 · 보수
# 침구 · 데코 · 원예
# 치킨 · 초밥 · 베이커리
# 세제 · 탈취 · 제습
# 언더웨어 · 양말

# 2019-10~2020-09

# large 별로 구분
# 1) 가구 · 수납 · 조명 · 보수 카테고리만 추출
purchase_fur <- purchase %>% filter(large == "가구 · 수납 · 조명 · 보수") %>%
  group_by(date, mid) %>% summarise(n = length(mid))

View(purchase_fur)

# 가구 · 수납 · 조명 · 보수
purchase_fur %>% filter(date >= '2019-10' & date <= '2020-09') %>%
  ggplot() + 
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n, group = mid, col = mid)) +
  geom_line(aes(x = date, y = n, group = mid, col = mid), size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0),limits = c(0,100))

# 2) 침구 · 데코 · 원예 카테고리만 추출
purchase_bed <- purchase %>% filter(large == "침구 · 데코 · 원예") %>%
  group_by(date, mid) %>% summarise(n = length(mid))

View(purchase_bed)

# 침구 · 데코 · 원예
purchase_bed %>% filter(date >= '2019-10' & date <= '2020-09') %>%
  ggplot() + 
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n, group = mid, col = mid)) +
  geom_line(aes(x = date, y = n, group = mid, col = mid), size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0),limits = c(0,100))

# 3) 치킨 · 초밥 · 베이커리 카테고리만 추출
purchase_food <- purchase %>% filter(large == "치킨 · 초밥 · 베이커리") %>%
  group_by(date, mid) %>% summarise(n = length(mid))

View(purchase_food)

# 치킨 · 초밥 · 베이커리
purchase_food %>% filter(date >= '2019-10' & date <= '2020-09') %>%
  ggplot() + 
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n, group = mid, col = mid)) +
  geom_line(aes(x = date, y = n, group = mid, col = mid), size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0),limits = c(0,100))

# 4) 세제 · 탈취 · 제습 카테고리만 추출
purchase_clean <- purchase %>% filter(large == "세제 · 탈취 · 제습") %>%
  group_by(date, mid) %>% summarise(n = length(mid))

View(purchase_clean)

# 세제 · 탈취 · 제습
purchase_clean %>% filter(date >= '2019-10' & date <= '2020-09') %>%
  ggplot() + 
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n, group = mid, col = mid)) +
  geom_line(aes(x = date, y = n, group = mid, col = mid), size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0),limits = c(0,100))

# 5) 언더웨어 · 양말 카테고리만 추출
purchase_socks <- purchase %>% filter(large == "언더웨어 · 양말") %>%
  group_by(date, mid) %>% summarise(n = length(mid))

View(purchase_socks)

# 언더웨어 · 양말
purchase_socks %>% filter(date >= '2019-10' & date <= '2020-09') %>%
  ggplot() + 
  geom_text(aes(x= date, y = n, label = n), vjust = -0.7) + 
  geom_point(aes(x= date, y = n, group = mid, col = mid)) +
  geom_line(aes(x = date, y = n, group = mid, col = mid), size = 1.0, alpha = 0.45) +
  scale_y_continuous(expand = expansion(mult = 0),limits = c(0,100))
