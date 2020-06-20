library(ggplot2) # 그래프 출력을 위한 패키지 로드
library(tidyverse) # 전처리를 위한 패키지 로드
library(dplyr) # 전처리를 위한 패키지 로드
library(ggmap) # 구글 지도 import위한 패키지 로드

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
    select(o.name,temp) %>% # seoul data frame 중 관측소 명과 온도만  
    group_by(o.name) %>%  # 관측소 기준으로 그룹화
    summarise(temp_m = mean(temp)) # 관측소 별 평균온도 변수 출력
temp1

# 3번 관측소별 평균기온
ggplot(temp1, aes(x=o.name, y=temp_m)) +  # x축 관측소, y축 관측소 별 평균온도 변수 출력
    geom_col(fill='#0000FF') + # 그래프 색상은 파랑색으로 
    scale_y_continuous(breaks = seq(0,14,2)) + # y축 범위를 14까지 2씩 증가
    ggtitle("서울지역 연평균기온") # 그래프 타이틀

# 4번 평균기온이 높은 상위 3개 관측소, 하위 3개 관측소 
temp_a <- arrange(temp1, desc(temp_m)) # temp1 dataframe을 관측소 별 평균온도 기준으로 내림차순 
head(temp_a, 3) # 상위 3개
tail(temp_a, 3) # 하위 3개
temp_a

# 5번 4년 간 서울지역 연평균 기온의 추이 
temp2 <- seoul %>% 
    select(year, temp) %>%  # seoul dataframe의 year, temp만 선택
    group_by(year) %>% # 연도기준으로 그룹화
    summarise(temp_m2 = mean(temp)) # 그룹화 기준으로 평균값

ggplot(temp2, aes(x=year, y=temp_m2)) + # x축은 연도로 y축은 그룹화 기준으로 산출한 평균값
    geom_col( fill='red') + # 색상은 빨강
    scale_y_continuous(breaks = seq(0,12,2)) + # y축 범위를 0부터 12까지 2씩 증가
    ggtitle("연평균 기온 변화") # y 범위 2씩 조정

# 6번 관측소별 위치 지도에 표시하기 
# 구글 인증키 등록
register_google(key='AIzaSyDB_b-JmyjXr9MlWQhX6A46-eNxvy78fMg')
# seoul 데이터에서 추출 후 summarise
obser <- seoul %>% 
    group_by(o.name) %>%  # 관측소 기준으로 그룹화
    select(o.name, o.addr,long, lat,temp) %>%  # 관측소, 주소, 경도, 위도, 온도만 선택
    summarise(lon = mean(long), # 그룹화 기준 경도 평균
              lat = mean(lat), # 그룹화 기준 위도 평균
              temp = (mean(temp)-11)^3) # 그룹화 기준 평균(버블의 크기를 위해서 (평균-11)^3)
head(obser)
tail(obser)

# 주소 데이터 추출 
head(y05)
juso <- data.frame(y05['주소']) # 주소만 따로 부르기 위해서 y05 데이터의 주소 데이터만 부름
juso <- rename(juso,
               o.addr = 주소) # 주소 컬럼 명 변경
str(juso)
head(juso)
juso %>%  mutate(mu.col=as.character(o.addr)) -> juso # o.addr 데이터가 factor 형으로 되어 있어서 character형으로 변환한 변수 생성
str(juso)

gc <- geocode(enc2utf8(juso$mu.col)) # char 형으로 변환한 변수를 geocode로 인코딩
View(gc)
cen <- c(mean(obser$lon), mean(obser$lat)) # 중심값을 위해서 위도 경도 데이터 평균값 변수 생성
map <- get_googlemap(center=cen, # 중심값
                     maptype = "roadmap", # 지도는 roadmap으로
                     zoom=11, marker=gc) # 확대 11, 마커는 gc로

ggmap(map, extent = "device") # 격자 제거
gmap <- ggmap(map)

# 7번
map <- get_googlemap(center=cen, # 6번에서 생성한 중심값 
                     maptype = "roadmap", # 지도는 roadmap으로
                     zoom=11) # 확대는 11

gmap+geom_point(data = obser, # 6번에서 만든 obser 데이터
               aes(x=lon, y=lat, size=temp), # x축 경도, y축 위도, 버블 크기는 obser 데이터 온도 
               alpha=0.3, # 버블의 희미함의 정도는 0.3
               colour=c("blue")) +  # 색은 파랑색
        geom_text(data = obser, # 관측소 명 표현
                   aes(x=lon, y=lat), # x축 경도, y축 위도
                   size=3.5, # 글 크기는 3.5
                   label=obser$o.name, # 글은 관측소 명
                  colour=c("red")) # 색은 네이비 색(검은색으로 하면 표현이 잘 안됨)
ggmap(map, extent = "device")
gmap <- ggmap(map)      
gmap
