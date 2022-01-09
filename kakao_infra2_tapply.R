pacman::p_load(tidyverse, lubridate)

# 디렉토리 설정
setwd("C:/R/kakao_raw")
getwd()

# 데이터 불러오기
'kakao_infra.csv' %>% 
  read_csv(locale=locale('ko',encoding='euc-kr')) %>% 
  as_tibble() -> kakao_infra

############# 1. 총액 ###############

#필요한 컬럼만 선택 (갑청구일, 대분류, 중분류, 소분류, 월비용, 간접관리비율)
kakao_infra <- kakao_infra %>% 
  select(Date_A, Item1, Item2, Item3, Listprice,Mgmtratio) %>% 
  mutate(
    YEAR=year(Date_A),
    MONTH=month(Date_A),
    .before=Item1
  )

#컬럼속성 변경
kakao_infra$Listprice <- as.double(kakao_infra$Listprice)
#kakao_infra$갑청구일 <- as_date(ymd(kakao_infra$갑청구일))

#필요한 컬럼 생성 
kakao_infra <- kakao_infra %>% 
  mutate(YM=format(as.Date(Date_A), "%Y-%m"),
         .before=YEAR)

#계산된 열 생성
kakao_infra <- kakao_infra %>% 
  mutate(Mgmtcost=Listprice*Mgmtratio) %>% 
  mutate(Sum=Listprice+Mgmtcost) %>% 
  mutate(Sum08=Sum/100000000)


View(kakao_infra)

kakao_infra$YEAR <- as_factor(kakao_infra$YEAR)
levels(kakao_infra$YEAR)

kakao_infra$MONTH <- as_factor(kakao_infra$MONTH)
levels(kakao_infra$MONTH)

kakao_infra$Sum <- as.double(kakao_infra$Sum)

temp1 <- tapply(kakao_infra$Sum, INDEX=list(kakao_infra$Item1,kakao_infra$YEAR), FUN=sum, na.rm=TRUE)
temp1[10,1]<-0
margin.table(temp1, margin=1)
margin.table(temp1, margin=2)

prop.table(temp1, margin=1)
prop.table(temp1, margin=2)

addmargins(margin.table(temp1, margin=1))
addmargins(margin.table(temp1, margin=2))

addmargins(prop.table(temp1, margin=1), margin=1)
addmargins(prop.table(temp1, margin=1), margin=2)



temp2 <- tapply(kakao_infra$Sum, INDEX=list(kakao_infra$YEAR,kakao_infra$Item1), FUN=sum, na.rm=TRUE)
temp2[1,10]<-0
margin.table(temp2, margin=1)
margin.table(temp2, margin=2)

prop.table(temp2, margin=1)
prop.table(temp2, margin=2)

addmargins(margin.table(temp2, margin=1))
addmargins(margin.table(temp2, margin=2))

addmargins(prop.table(temp2, margin=1), margin=1)
addmargins(prop.table(temp2, margin=1), margin=2)
