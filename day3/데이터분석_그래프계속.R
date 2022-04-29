library(dplyr)
library(readxl)
library(ggplot2)


head(economics)
str(economics)

#산점도
ggplot(data = economics, aes(x=date,y=unemploy)) + geom_point()


# 시계열 그래프(선그래프) : 시간에 따른 개인저축률의 변화
ggplot(data = economics, aes(x=date,y=psavert)) + geom_line()

# boxplot # 중앙값을 이용하여 비교분석
ggplot(data = mpg, aes(x=drv,y=hwy)) + geom_boxplot()

# 두개 같은 의미(|, %in%)
tmp=mpg %>% 
  filter(class=="compact" | class=="subcompact" | class=="suv")

tmp1=mpg %>% 
  filter(class %in% c("compact","subcompact","suv"))

# 차종별 도시연비 박스그래프 # 박스크기별 빈도수, 분포도 파악가능
ggplot(data = tmp, aes(x=class,y=cty)) + geom_boxplot()




