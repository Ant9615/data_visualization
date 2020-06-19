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
    geom_col(position = "stack",fill='#0000FF') +
    scale_y_continuous(breaks = seq(0,14,2)) +
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
    geom_col(position = "stack", fill='red') +
    scale_y_continuous(breaks = seq(0,12,2)) +
    ggtitle("연평균 기온 변화") # y 범위 2씩 조정

# 6번 관측소별 위치 지도에 표시하기 
# 구글 인증키 등록
register_google(key='AIzaSyDB_b-JmyjXr9MlWQhX6A46-eNxvy78fMg')
# seoul 데이터에서 추출 후 summarise
obser <- seoul %>% 
    group_by(o.name) %>% 
    select(o.name, o.addr,long, lat,temp) %>% 
    summarise(lon = mean(long),
              lat = mean(lat),
              temp = (mean(temp)-11)^3)
head(obser)
tail(obser)
# 주소 데이터 추출 
head(y05)
juso <- data.frame(y05['주소'])
juso <- rename(juso,
               o.addr = 주소)
str(juso)
head(juso)
juso %>%  mutate(mu.col=as.character(o.addr)) -> juso
str(juso)

gc <- geocode(enc2utf8(juso$mu.col))
View(gc)
cen <- c(mean(obser$lon), mean(obser$lat))
map <- get_googlemap(center=cen,
                     maptype = "roadmap",
                     zoom=11, marker=gc)

ggmap(map, extent = "device")
gmap <- ggmap(map)

# 7번
map <- get_googlemap(center=cen,
                     maptype = "roadmap",
                     zoom=11)

gmap+geom_point(data = obser,
               aes(x=lon, y=lat, size=temp),
               alpha=0.3,
               colour=c("blue")) + 
        geom_text(data = obser,
                   aes(x=lon, y=lat),
                   size=3.5,
                   label=obser$o.name,
                  colour=c("navy"))
ggmap(map, extent = "device")
gmap <- ggmap(map)      
