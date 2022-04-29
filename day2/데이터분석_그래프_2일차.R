library(ggplot2)
mpg=as.data.frame(ggplot2::mpg)
mpg
##########산점도
##1. 배경 설정하기
# x축 displ, y축 hwy로 지정해 배경 생성
ggplot(data = mpg, aes(x=displ,y=hwy))

##2. 그래프 추가하기
# 배경에 산점도 추가
ggplot(data = mpg, aes(x=displ,y=hwy)) + geom_point() # 포인트(점)으로 나타냄

##3. 축 범위를 조정하는 설정 추가하기
# x축 범위 3~6으로 지정
ggplot(data = mpg, aes(x=displ,y=hwy)) + geom_point() +xlim(3,6)

# x축 범위 3~6, y축 범위 10~30지정
ggplot(data = mpg, aes(x=displ,y=hwy)) + 
  geom_point() +
  xlim(3,6) +
  ylim(10,30) # 레이어는 x,y 축과 포인트 총 2개

######################################

ggplot(data = mpg, aes(x=cty,y=hwy))+ geom_point()

###################################


ggplot(data = midwest, aes(x=poptotal,y=popasian)) +
  geom_point()+
  xlim(0,500000) +
  ylim(0,10000)
######################################

#####막대그래프
# 데이터의 크기를 막대의 길이로 표현한 그래프

#1. 집단별 평균표 만들기
library(dplyr)

df_mpg=mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy=mean(hwy))

df_mpg

#2. 그래프 생성하기
# 구동방식별 평균 고속도로 연비
ggplot(data=df_mpg, aes(x=drv, y=mean_hwy)) +geom_col() # 막대그래프로 그려라

#3. 크기 순으로 정렬하기
ggplot(data=df_mpg, aes(x=reorder(drv, -mean_hwy), y=mean_hwy)) +geom_col()

######빈도 막대 그래프
#값의 개수(빈도)로 막대의 길이를 표현한 그래프
#x축 범주 변수, y축 빈도
ggplot(data=mpg, aes(x=drv)) + geom_bar() # 각 구동장치별로 몇개있는지

ggplot(data=mpg, aes(x=hwy)) + geom_bar()

# 평균막대 : geom_col()
# 빈도막대그래프: geom_bar()


# 조건에 맞는 표 생성######필터링 정제하는게 중요!!!!!
df=mpg %>% 
  filter(class=="suv") %>% 
  group_by(manufacturer) %>% 
  summarise(mean_cty=mean(cty)) %>% 
  arrange(desc(mean_cty)) %>% 
  head(5)

ggplot(data=df, aes(x=reorder(manufacturer,-mean_cty), y=mean_cty)) + geom_col()

# 빈도수 그래프 생성성
ggplot(data = mpg, aes(x = class)) +geom_bar()



