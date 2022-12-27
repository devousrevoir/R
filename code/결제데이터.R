# 결제데이터
purchase_table = read_excel("C:/k_digital/source/r_source/R miniproject/purchase_transaction.xlsx", sheet = 'transaction')
View(purchase_table)
# readxl 패키지 설치


# 카테고리를 s1 변수에 담기
s1 = table(purchase_table$MasterCategoryFullName)
str(s1)
s1 = as.data.frame(s1)
s1$Var1 = as.vector(s1$Var1)
str(s1)
View(s1)


## 카테고리를 '->' 기준으로 자르기
s1_split <- data.frame(do.call(rbind, strsplit(s1$Var1, split = '->', fixed = T)))
View(s1_split)
s1_split <- s1_split %>% arrange(X1, X2, X3)
# dplyr 패키지 설치


## 중위 카테고리 중복을 없애기
s1_split_mid <- s1_split %>% distinct(X2)
View(s1_split_mid)
