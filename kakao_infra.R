pacman::p_load(tidyverse, lubridate, reshape, XLConnect, googlesheets4)

wb <- "https://docs.google.com/spreadsheets/d/1JOhz-tEs00_-F2RRBD-eA8aNYBf0v9f5Ami3zla_0HI/edit?usp=sharing"
gs4_auth(email = "jason.gang@kakaoenterprise.com")

############# 1. 각 raw data를 merge로 합치기  ###############

M_2022_ALL <- range_read(wb, range="2022_ALL!A1:L")
M_2021_ALL <- range_read(wb, range="2021_ALL!A1:L")
M_2020_ALL <- range_read(wb, range="2020_ALL!A1:L")

range_delete(wb, range="merge!A1:L", shift = "up")
df <- range_write(wb, data = M_2022_ALL, range="merge!A1")
df %>% sheet_append(M_2021_ALL) %>% sheet_append(M_2021_ALL)
 

kakao_infra <- range_read(wb, range="merge!A1:L")
kakao_infra$Date_A <- as.Date(kakao_infra$Date_A)
kakao_infra$Date_B <- as.Date(kakao_infra$Date_B)

############# 2. 측정값 열 추가 ###############

#필요한 컬럼만 선택 (갑청구일, 대분류, 중분류, 소분류, 월비용, 간접관리비율)
kakao_infra <- kakao_infra %>% 
  select(Date_A, Item1, Item2, Item3, Listprice,Mgmtratio) %>% 
  mutate(
    YEAR=year(Date_A),
    MONTH=month(Date_A),
    .before=Item1
  )

#필요한 컬럼 생성 
kakao_infra <- kakao_infra %>% 
  mutate(YM=format(as.Date(Date_A), "%Y-%m"),
         .before=YEAR)

#계산된 열 생성
kakao_infra <- kakao_infra %>% 
  mutate(Mgmtcost=Listprice*Mgmtratio) %>% 
  mutate(Sum=Listprice+Mgmtcost)

range_write(wb, data = kakao_infra, range="data!A1")
############# 3. 전체 비용 연도별 / 월별 요약 ###############

#연도별 총계 요약 만들기
kakao_infra_sum_y <- kakao_infra %>% 
  group_by(YEAR) %>% 
  summarise(Sum=sum(Sum,na.rm=T), .groups='drop')

kakao_infra_sum_y$Sum <- round(kakao_infra_sum_y$Sum,1)


#월별 총계 요약 만들기
kakao_infra_sum_m <- kakao_infra %>% 
  group_by(Date_A) %>% 
  summarise(Sum=sum(Sum,na.rm=T), .groups='drop')

kakao_infra_sum_m$Sum <- round(kakao_infra_sum_m$Sum,1)


############# 4. GPU 연도별 / 월별 요약 ###############

# 중분류가 GPU Cloud만 선택
kakao_infra_gpu <- kakao_infra %>% 
  filter(Item2=='GPU Cloud')

#연도별 GPU총계 표 만들기
kakao_infra_gpu_sum_y <- kakao_infra_gpu %>% 
  group_by(YEAR) %>% 
  summarise(Sum=sum(Sum,na.rm=T), .groups='drop')

kakao_infra_gpu_sum_y$Sum <- round(kakao_infra_gpu_sum_y$Sum,1)



#월별 GPU총계 요약 만들기
kakao_infra_gpu_sum_m <- kakao_infra_gpu %>% 
  group_by(Date_A) %>% 
  summarise(Sum=sum(Sum,na.rm=T), .groups='drop')

kakao_infra_gpu_sum_m$Sum <- round(kakao_infra_gpu_sum_m$Sum,1)


###### 5. 총액 VS. GPU 연도기준 ########

# 연도별 총계 - 연도별 GPU총계 합치기
kakao_infra_join_y <- left_join(kakao_infra_sum_y,kakao_infra_gpu_sum_y,by=c('YEAR'))

#비율 계산 컬럼추가
kakao_infra_join_y <- kakao_infra_join_y %>% 
  mutate(Sum.z=Sum.x-Sum.y) %>% 
  mutate(Sum.y.r = Sum.y/Sum.x) %>% 
  mutate(Sum.z.r = Sum.z/Sum.x)

#연도, GPU비율, 기타비율 컬럼만 가져오기
kakao_infra_join_y_r <- kakao_infra_join_y %>% 
  select(YEAR, Sum.y.r, Sum.z.r)

#연도, GPU금액, 기타금액 컬럼만 가져오기
kakao_infra_join_y_p <- kakao_infra_join_y %>% 
  select(YEAR, Sum.y, Sum.z)

#비율 데이터 long포맷으로 변경
kakao_infra_join_y_r_long<-kakao_infra_join_y_r %>% 
  pivot_longer(cols=Sum.y.r:Sum.z.r,names_to='Category',values_to="Value")

#금액 데이터 long포맷으로 변경
kakao_infra_join_y_p_long<-kakao_infra_join_y_p %>% 
  pivot_longer(cols=Sum.y:Sum.z,names_to='Category',values_to="Value")


###### 6. 총액 VS. GPU 월별 기준 ########

# 월별 총계 - 월별 GPU총계 합치기
kakao_infra_join_m <- left_join(kakao_infra_sum_m,kakao_infra_gpu_sum_m,by=c('Date_A'))

#비율 계산 컬럼추가
kakao_infra_join_m <- kakao_infra_join_m %>% 
  mutate(Sum.z=Sum.x-Sum.y) %>% 
  mutate(Sum.y.r = Sum.y/Sum.x) %>% 
  mutate(Sum.z.r = Sum.z/Sum.x)

#연도, GPU비율, 기타비율 컬럼만 가져오기
kakao_infra_join_m_r <- kakao_infra_join_m %>% 
  select(Date_A, Sum.y.r, Sum.z.r)

#비율 long포맷으로 변경
kakao_infra_join_m_r_long <- kakao_infra_join_m_r %>% 
  pivot_longer(cols=Sum.y.r:Sum.z.r,names_to='Category',values_to="Value")

kakao_infra_join_m_r_long$Value = round(kakao_infra_join_m_r_long$Value,1)


#연도, GPU금액, 기타금액 컬럼만 가져오기
kakao_infra_join_m_p <- kakao_infra_join_m %>% 
  select(Date_A, Sum.y, Sum.z)

#비율 long포맷으로 변경
kakao_infra_join_m_p_long <- kakao_infra_join_m_p %>% 
  pivot_longer(cols=Sum.y:Sum.z,names_to='Category',values_to="Value")


#### 상관분석 ####
cor<-cor(kakao_infra_join_m$Sum.x,kakao_infra_join_m$Sum.y,method="pearson")
cor<-as_tibble(cor)

### 단순회귀분석 ###
Kakao.lm <- lm(Sum.x ~ Sum.y, data=kakao_infra_join_m)
plot(kakao_infra_join_m$Sum.x ~ kakao_infra_join_m$Sum.y,
     col="cornflowerblue", pch=19,
     xlab="GPU", ylab="Total",
     main="GPU and Cost")
abline(Kakao.lm, col="salmon", lwd=2)

summary(Kakao.lm)
Kakao.lm.summary <- summary(Kakao.lm)
coef(Kakao.lm.summary)
anova(Kakao.lm)
confint(Kakao.lm.summary)
fitted(Kakao.lm)
resid(Kakao.lm)
kakao.new <- data.frame(Sum.y=c(172058521.6  ,172058521.6))
predict(Kakao.lm, newdata=kakao.new)
#연도별 총계 막대그래프
ggplot(kakao_infra_sum_y, aes(x=YEAR, y=Sum)) +
  geom_col() + scale_x_discrete(limits = c(2020, 2021)) +
  geom_text(label=kakao_infra_sum_y$Sum, hjust=0.5, vjust=1, color="white")

#월별 총계 막대그래프
ggplot(kakao_infra_sum_m, aes(x=Date_A, y=Sum)) +
  geom_col() + 
  geom_text(label=kakao_infra_sum_m$Sum, hjust=0.5, vjust=1, color="white", size=3)
#연도별 GPU총계 막대그래프
ggplot(kakao_infra_gpu_sum_y, aes(x=YEAR, y=Sum)) +
  geom_col() + scale_x_discrete(limits = c(2020, 2021)) +
  geom_text(label=kakao_infra_gpu_sum_y$Sum, hjust=0.5, vjust=0, color="Red", size=4)
#월별 GPU총계 막대그래프
ggplot(kakao_infra_gpu_sum_m, aes(x=Date_A, y=Sum)) +
  geom_col() + 
  geom_text(label=kakao_infra_gpu_sum_m$Sum, hjust=0.5, vjust=1, color="white", size=3)
ggplot(kakao_infra_join_y_r_long, aes(x=YEAR, y=Value, fill=Category)) + 
  geom_col() +
  scale_x_discrete(limits = c(2020,2021)) +
  theme(legend.position=c(0.08,0.85))
ggplot(kakao_infra_join_m_p_long, aes(x=Date_A, y=Value, fill=Category)) + 
  geom_col() +
  theme(legend.position=c(0.2,0.2))



Prestige.lm <- lm(income ~ education, data=Prestige)
class(Prestige.lm)
Prestige.lm

plot(Prestige$income ~ Prestige$education,
     col="cornflowerblue", pch=19,
     xlab="Education (years)", ylab="Income (dollars)",
     main="Education and Income")
abline(Prestige.lm, col="salmon", lwd=2)
