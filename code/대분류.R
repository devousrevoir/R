# 유통데이터 가져오기
dis = read.csv("C:/k_digital/source/r_source/R miniproject/[유통데이터 활용 경진대회] 상품정보 데이터.xlsx - 마스터상품.csv")
dis
View(dis)



# 상품분류명을 중복을 제거하고 가져오기
library(dplyr)
disvalue <- dis %>% select("상품분류명")
disvalue <- distinct(disvalue) %>% arrange("상품분류명")
View(disvalue)
str(disvalue)



# 상품분류명을 | 기준으로 자르기

disvalue_split <- data.frame(do.call(rbind, 
                                     strsplit(disvalue$상품분류명, split = '|',
                                              fixed = T)))
View(disvalue_split)



# 각 대분류의 분포를 막대그래프로로 확인
library(ggplot2)
qplot(data = disvalue_split, x = X1, geom = 'bar')


a1 <- disvalue_split %>% filter(X1 == "가공식품") %>% select(X1, X2)
a1

a1 %>% ggplot() + geom_bar(aes(X1, fill = X2), position = "dodge")

a2 <- disvalue_split %>% filter(X1 == "교육/문화용품") %>% select(X1, X2)
a2

a2 %>% ggplot() + geom_bar(aes(X1, fill = X2), position = "dodge")

a3 <- disvalue_split %>% filter(X1 == "기타상품") %>% select(X1, X2)
a3

a3 %>% ggplot() + geom_bar(aes(X1, fill = X2), position = "dodge")

a4 <- disvalue_split %>% filter(X1 == "디지털/가전") %>% select(X1, X2)
a4

a4 %>% ggplot() + geom_bar(aes(X1, fill = X2), position = "dodge")

a5 <- disvalue_split %>% filter(X1 == "신선식품") %>% select(X1, X2)
a5

a5 %>% ggplot() + geom_bar(aes(X1, fill = X2), position = "dodge")

a6 <- disvalue_split %>% filter(X1 == "의류/패션잡화") %>% select(X1, X2)
a6

a6 %>% ggplot() + geom_bar(aes(X1, fill = X2), position = "dodge")

a7 <- disvalue_split %>% filter(X1 == "의약품/의료기기") %>% select(X1, X2)
a7

a7 %>% ggplot() + geom_bar(aes(X1, fill = X2), position = "dodge")

a8 <- disvalue_split %>% filter(X1 == "인테리어") %>% select(X1, X2)
a8

a8 %>% ggplot() + geom_bar(aes(X1, fill = X2), position = "dodge")

a9 <- disvalue_split %>% filter(X1 == "일상용품") %>% select(X1, X2)
a9

a9 %>% ggplot() + geom_bar(aes(X1, fill = X2), position = "dodge")

a10 <- disvalue_split %>% filter(X1 == "전문스포츠/레저") %>% select(X1, X2)
a10

a10 %>% ggplot() + geom_bar(aes(X1, fill = X2), position = "dodge")



# 구매데이터 가져오기


# 구매데이터 - 연령별로 구분(20대부터 시작)

a <- Req[Req$age_group >= 20 & Req$age_group < 30, ]
b <- Req[Req$age_group >= 30 & Req$age_group < 40, ]
c <- Req[Req$age_group >= 40 & Req$age_group < 50, ]
d <- Req[Req$age_group >= 50 & Req$age_group < 60, ]
e <- Req[Req$age_group >= 60 , ]
View(Req)

# 20대

a <- a %>% arrange(age_group)
View(a)

# 20대 person별 합계

Person_Sum <- a %>% 
  group_by(person_) %>% 
  summarise(person_tot = sum(amount))

View(Person_Sum)
# 20대 합계 평균

a1 <- Person_Sum %>% 
  summarise(person_total = sum(person_tot),
            person_mean = mean(person_tot))

View(a1)

# 30대

b <- b %>% arrange(age_group)
View(b)

# 30대 person별 합계

Person_Sum <- b %>% 
  group_by(person_) %>% 
  summarise(person_tot = sum(amount))

View(Person_Sum)

# 30대 합계 평균

b1 <- Person_Sum %>% 
  summarise(person_total = sum(person_tot),
            person_mean = mean(person_tot))

View(b1)

# 40대

c <- c %>% arrange(age_group)
View(c)

# 40대 person별 합계

Person_Sum <- c %>% 
  group_by(person_) %>% 
  summarise(person_tot = sum(amount))

# 40대 합계 평균

c1 <- Person_Sum %>% 
  summarise(person_total = sum(person_tot),
            person_mean = mean(person_tot))

View(c1)

# 50대

d <- d %>% arrange(age_group)
View(d)

# 50대 person별 합계

Person_Sum <- d %>% 
  group_by(person_) %>% 
  summarise(person_tot = sum(amount))

# 50대 합계 평균

d1 <- Person_Sum %>% 
  summarise(person_total = sum(person_tot),
            person_mean = mean(person_tot))

View(d1)

# 60대

e <- e %>% arrange(age_group)
View(e)

# 60대 person별 합계

Person_Sum <- e %>% 
  group_by(person_) %>% 
  summarise(person_tot = sum(amount))

# 60대 합계 평균

e1 <- Person_Sum %>% 
  summarise(person_total = sum(person_tot),
            person_mean = mean(person_tot))

View(e1)



# 구매데이터 상세

install.packages('readxl')
library(readxl)

Req_info = read_excel("C:/k_digital/source/r_source/R miniproject/card_transaction_명세.xlsx")
View(Req_info)

# 결제데이터 상세

purchase_info = read_excel("C:/k_digital/source/r_source/R miniproject/purchase_transaction.xlsx", 
                           sheet = '테이블 명세')

View(purchase_info)

# 결제데이터

purchase_table = read_excel("C:/k_digital/source/r_source/R miniproject/purchase_transaction.xlsx", 
                            sheet = 'transaction')
View(purchase_table)