# 빠진 데이터 찾아라 : 결측치

#결측치 표기-대문자 NA

df=data.frame(sex=c("M","F",NA, "M","F"),
              score=c(5,4,3,4,NA)) # NA는 따옴표 없다.
df

# 결측치 확인
is.na(df) # na면 true

table(is.na(df)) # table 각 요소의 빈도수 확인

is.na(df$sex)
table(is.na(df$sex))

mean(df$score) # 결측치가 있으면 계산 자체 불가
sum(df$score)

library(dplyr)
df %>% filter(is.na(score)) # na가 있는 값만 필터링

df %>% 
  filter(!is.na(score)) #score에 na 제외한 값들만 필터링


df_nomiss=df %>% filter (!is.na(score))
mean(df_nomiss$score)

sum(df_nomiss$score)

# filter select 헷갈릴 때:
# 조건이 있으면 filter, 없으면 select

#score , sex 결측치 제외(행단위)
df_nomiss=df %>% filter (!is.na(score) & !is.na(sex)) # &: and
df_nomiss

# 결측치 하나라도 있으면 제거(행 자체 없앰)
df_nomiss2= na.omit(df) # 모든 변수에 결측치 없는 데이터 추출
df_nomiss2


# 일반적으로 컬럼 한개씩 결측치 있는지 파악

# 함수의 결측치 제외 기능 이용하기 - na.rm=T
mean(df$score, na.rm=T) # 결측치 제외하고 평균 산출

sum(df$score, na.rm=T) # 결측치 제외하고 합계 산출

exam
exam[c(3,8,15),"math"]=NA # math의 3,8,15행 요소 NA
exam

# 결측지 제외하고난 뒤 평균산출
exam %>% 
  summarise(mean_math= mean(math,na.rm=T)) # 혹시가 결측치 있다면 제외해줘!

################NA 값 대체하기
# 평균으로 대체
exam %>% 
  summarise(mean_math= mean(math,na.rm=T)) # 평균 55점임을 확인

#####중요!!!!!!!
exam$math=ifelse(is.na(exam$math),55,exam$math) # NA를 평균점수 55로대체
exam

head(mpg)
mpg[c(65,124,131,153,212),"hwy"]=NA

mpg %>% 
  summarise(mean_hwy=mean(hwy,na.rm=T)) # 평균 23.5

mpg %>% filter(is.na(hwy))

table(is.na(mpg$hwy))
table(is.na(mpg$drv))

mpg %>% 
  filter(!is.na(hwy))

mpg %>% filter(is.na(drv))



mpg %>% 
  filter(!is.na(hwy)) %>% # hwy 중 na 아닌 것만 걸러준다음
  group_by(drv) %>% # 그룹화하고
  summarise(mean_hwy = mean(hwy)) # 평균 나타냄

##############이상치 제거 - 정상범주에서 크게 벗어난 값

# 이상치 확인하기


#결측 처리하기 - sex
# sex가 3이면 NA 할당

outlier = data.frame(sex = c(1,2,1,3,2,1),
                     score = c(5,4,3,4,2,6))

outlier$sex = ifelse(outlier$sex ==3,NA, outlier$sex)

# 결측 처리하기- score
# sex가 1~5 아니면 NA 할당
outlier$score = ifelse(outlier$score >5,NA, outlier$score)

outlier %>% 
  filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score = mean(score))











#########이상치 제거하기-극단적인 값
# 정상범위 기준 정해서 벗어나면 결측 처리
# 논리적 판단 or 통계적 판단

# boxplot으로 극단치 기준 정해서 제거하기
mpg=as.data.frame(ggplot2::mpg)
mpg
boxplot(mpg$hwy)$stats # $stats: boxplot 통계치 출력력

#극단치 최상위값, 최하위값 알 수있다. # 12,37

#12~37 벗어나면 NA할당 (이상치를 NA로 할당)
mpg$hwy= ifelse(mpg$hwy <12 | mpg$hwy > 37, NA, mpg$hwy) # |: 또는
table(is.na(mpg$hwy))

# 결측치 제외하고 분석하기
mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy=mean(hwy,na.rm=T))


#####################################예제
#이상치 할당
mpg = as.data.frame(ggplot2::mpg)
mpg[c(10,14,58,93),"drv"]="K"
mpg[c(29,43,129,203),"cty"]=c(3,4,37,42)
###################################1
#drv 중 이상치 K를 결측치로 바꿔줌
table(mpg %>% select(drv))



mpg$drv= ifelse(mpg$drv=="K",NA, mpg$drv)
table(mpg$drv)

tmp=mpg %>% filter(!is.na(drv))
table(is.na(tmp$drv))


###################################2
#극단치를 결측치로 바꿔준다
boxplot(mpg$cty)$stats # 최하위, 최상위:9,26

mpg$cty=ifelse(mpg$cty<9 | mpg$cty>26, NA, mpg$cty)
table(is.na(mpg$cty))

boxplot(tmp1$cty) #boxplot은 NA값은 제외하고 나타낸다

tmp1=mpg %>% filter(!is.na(cty))

boxplot(tmp1$cty)

###################################3
#바꿔준 결측치를 제외하고 cty평균 나타냄
mpg %>% 
  filter(!is.na(drv) & !is.na(cty)) %>% 
  group_by(drv) %>% 
  summarise(mean_cty=mean(cty))







