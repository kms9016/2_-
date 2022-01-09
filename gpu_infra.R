pacman::p_load(tidyverse, lubridate, reshape, XLConnect, googlesheets4)

wb <- "https://docs.google.com/spreadsheets/d/14BLFn9qaAsl4unkBKsPmSAEWgIoZSlGf837Y9fyzMNY/edit?usp=sharing"
gs4_auth(email = "jason.gang@kakaoenterprise.com")

############# 1. 각 raw data를 merge로 합치기  ###############

M_2021_ALL <- range_read(wb, range="2021_ALL!A1:H")
M_2020_ALL <- range_read(wb, range="2020_ALL!A1:H")
M_OP <- range_read(wb, range="Operation!A1:D")

range_delete(wb, range="merge!A1:H", shift = "up")
df <- range_write(wb, data = M_2021_ALL, range="merge!A1")
df %>% sheet_append(M_2020_ALL)


gpu_infra <- range_read(wb, range="merge!A1:H")
gpu_infra$Date <- as.Date(gpu_infra$Date)
M_OP$Date <- as.Date(M_OP$Date)

############# 2. 측정값 열 추가 ###############

#필요한 컬럼만 선택 (갑청구일, 대분류, 중분류, 소분류, 월비용, 간접관리비율)
gpu_infra <- gpu_infra %>% 
  select(Date, Workspace, Flavor, Amount_sec, Price_sec) %>% 
  mutate(
    YEAR=year(Date),
    MONTH=month(Date),
    .before=Workspace
  )

#필요한 컬럼 생성 
gpu_infra <- gpu_infra %>% 
  mutate(YM=format(as.Date(Date), "%Y-%m"),
         .before=YEAR)
gpu_infra$YM <- paste(gpu_infra$YM,"-01",sep="")
gpu_infra$YM <- as.Date(gpu_infra$YM)

#계산된 열 생성
gpu_infra <- gpu_infra %>% 
  mutate(Sum=Amount_sec*Price_sec)

range_write(wb, data = gpu_infra, range="data!A1")

############# 3. 연도별/월별 요약 ###############

#연도별 총계 요약 만들기
gpu_infra_sum_y <- gpu_infra %>% 
  group_by(YEAR) %>% 
  summarise(Sum=sum(Sum,na.rm=T), .groups='drop')


#월별 총계 요약 만들기
gpu_infra_sum_m <- gpu_infra %>% 
  group_by(YM) %>% 
  summarise(Sum=sum(Sum,na.rm=T), .groups='drop')

#GPU사용액과 운영비용 월별로 합치기
gpu_infra_sum_m <- left_join(gpu_infra_sum_m,M_OP,by=c('YM'='Date'))
gpu_infra_sum_m <- select(gpu_infra_sum_m,YM, GPU_Sum=Sum, OP_Sum=Price)

range_write(wb, data = gpu_infra_sum_m, range="data_mon!A1")
