library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggmap)

# working directory 변경
setwd('C:/Users/STUDENT/Desktop/비대면강의/데이터시각화/final data')

# 데이터 변수 할당
y05 <- read.csv('./서울시 연평균기온 2005년 위치정보 (좌표계_ WGS1984).csv')
y06 <- read.csv('./서울시 연평균기온 2006년 위치정보 (좌표계_ WGS1984).csv')
y07 <- read.csv('./서울시 연평균기온 2007년 위치정보 (좌표계_ WGS1984).csv')
y08 <- read.csv('./서울시 연평균기온 2008년 위치정보 (좌표계_ WGS1984).csv')

# 1번 고유번호 컬럼을 연도로
y05$`고유번호`<-2005
y06$`고유번호`<-2006
y07$`고유번호`<-2007
y08$`고유번호`<-2008

# 데이터 프레임 결합
seoul_1 <- rbind(y05,y06,y07,y08)

# seoul 컬럼명 변경
seoul <- rename(seoul_1,
                year=고유번호,
                o.code=관측소코드,
                o.name=관측소명,
                o.addr=주소,
                temp=평균기온,
                long=경도,
                lat=위도)
names(seoul)
head(seoul)
tail(seoul)

View(seoul)


# 2번 관측소별 2005~2008 평균기온 
temp1 <- seoul %>% 
    select(o.name,temp) %>% 
    group_by(o.name) %>% 
    summarise(temp_m = mean(temp))
temp1

# 3번 관측소별 평균기온
ggplot(temp1, aes(x=o.name, y=temp_m)) + 
    geom_col() +
    ggtitle("서울지역 연평균기온")

# 4번 평균기온이 높은 상위 3개 관측소, 하위 3개 관측소 
temp_a <- arrange(temp1, desc(temp_m))
head(temp_a, 3) # 상위 3개
tail(temp_a, 3) # 하위 3개
temp_a

# 5번 4년 간 서울지역 연평균 기온의 추이 
temp2 <- seoul %>% 
    select(year, temp) %>% 
    group_by(year) %>% 
    summarise(temp_m2 = mean(temp))

ggplot(temp2, aes(x=year, y=temp_m2)) + 
    geom_col() +
    ggtitle("연평균 기온 변화") # y 범위 2씩 조정

# 6번 관측소별 위치 지도에 표시하기 
# 구글 인증키 등록
register_google(key='AIzaSyDB_b-JmyjXr9MlWQhX6A46-eNxvy78fMg')
obser <- seoul %>% 
    select(o.name,long, lat)
obser
