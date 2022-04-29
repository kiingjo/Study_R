####데이터 분석 준비하기
# 패키지 준비하기

install.packages("foreign")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

# 데이터 준비하기
setwd("C:/Recture/day3")
# 데이터 불러오기
raw_welfare=read.spss(file="./Data/Koweps_hpc10_2015_beta1.sav",
                        to.data.frame=T)

# 복사본 만들기
welfare=raw_welfare

head(welfare)
tail(welfare)

# 변수명 바꾸기 ###중요 데이터를 내가 필요한 이름으로 변경
welfare= rename(welfare,
                sex=h10_g3,          #성별
                birth=h10_g4,        #태어난 연도
                marriage=h10_g10,    #혼인 상태
                religion=h10_g11,    #종교
                income=p1002_8aq1,   #월급
                code_job=h10_eco9,   #직종 코드
                code_region=h10_reg7)#지역코드
head(welfare)                
str(welfare)

###### 성별 변수 검토 및 전처리
#### 1-1변수 검토하기(성별)

class(welfare$sex)

# 이상치 확인하기
table(welfare$sex)

# 이상치 결측 처리
welfare$sex=ifelse(welfare$sex==9,NA,welfare$sex) # 결측치 없다

# 결측치 확인
table(is.na(welfare$sex))

# 성별 항목 이름 부여
welfare$sex=ifelse(welfare$sex==1,"male","female")
table(welfare$sex)

# 성별에 대한 빈도그래프
qplot(welfare$sex)

#### 1-2변수 검토하기(월급)

class(welfare$income)

summary(welfare$income)

qplot(welfare$income) # 급여별 빈도그래프
qplot(welfare$income) +xlim(0,1000)

#이상치 확인 
summary(welfare$income) # min:0 이상치 확인, +결측치도 확인가능

#이상치 결측처리 # %in% : 특정 값을 포함하는지
welfare$income=ifelse(welfare$income %in% c(0,9999), NA, welfare$income)

# 결측치 확인
table(is.na(welfare$income))

### 성별에 따른 월급 차이 분석
sex_income= welfare %>% 
        filter(!is.na(income)) %>%  # income에 결측치 제외시킨것! 원본에서 삭제시킨건 아니다
        group_by(sex) %>% 
        summarise(mean_income=mean(income))

sex_income

#성별별 평균월급
ggplot(data=sex_income, aes(x=sex,y=mean_income)) +geom_col()

### 나이와 월급의 관계
#"몇 살 때 월급을 가장 많이 받을까?

#1-1 변수 검토하기 (나이)
class(welfare$birth)

# 이상치 확인
summary(welfare$birth)
qplot(welfare$birth)

#결측치 확인
table(is.na(welfare$birth)) # 결측치 없다

#파생변수 만들기-나이
welfare$age=2015-welfare$birth+1
summary(welfare$age)

#나이별 월급 표
age_income=welfare %>% 
      filter(!is.na(income)) %>% 
      group_by(age) %>% 
     summarise(mean_income= mean(income))
head(age_income)

#그래프

ggplot(data=age_income, aes(x=age,y=mean_income)) +geom_line()


###연령대에 따른 월급차이
#"어떤 연령대의 월급이 가장 많을까?

# 파생변수 만들기-연령대
welfare=welfare %>% 
        mutate(ageg= ifelse(age<30,"young",
                            ifelse(age<=59, "middle","old")))

qplot(welfare$ageg)


# 연령대에 따른 월급 차이 분석하기
# 연령대별 월급 평균표 만들기
ageg_income=welfare %>% 
    filter(!is.na(income)) %>% 
    group_by(ageg) %>% 
    summarise(mean_income=mean(income))

ggplot(data=ageg_income, aes(x=ageg,y=mean_income)) +geom_col()

# 막대정렬 : 초년, 중년, 노년 나이순
ggplot(data=ageg_income, aes(x=ageg,y=mean_income)) +geom_col() +
    scale_x_discrete(limits = c("young","middle","old")) #정렬

# 연령대 및 성별 월급차이
#"성별 월급 차이는 연령대별로 다를까?"

#연령대 및 성별 월급 평균표 만들기
sex_income=welfare %>% 
    filter(!is.na(income)) %>% 
    group_by(ageg,sex) %>% 
    summarise(mean_income=mean(income))

# 그래프 만들기

ggplot(data=sex_income, aes(x=ageg,y=mean_income, fill=sex)) +
  geom_col() +
  scale_x_discrete(limits = c("young","middle","old"))

# 성별 막대 분리
ggplot(data=sex_income, aes(x=ageg,y=mean_income, fill=sex)) +
  geom_col(position="dodge") +
  scale_x_discrete(limits = c("young","middle","old"))


#sex_age=wefare %>% 
 #   filter(!is.na(income)) %>% 
  #  group_by(age
             
             
             
###### 직업별 월급차이
#"어떤직입이 월급을 많이 받을까?"

class(welfare$code_job)
table(welfare$code_job)

# 직업처리분류표

library(readxl)
list_job=read_excel("./Data/Koweps_Codebook.xlsx",col_names=T,sheet=2)

head(list_job)

welfare= left_join(welfare,list_job, id="code_job")

welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job)


job_income= welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))

head(job_income) 

# 상위 10개
top10=job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)
top10             

#그래프 만들기
ggplot(data=top10, aes(x=reorder(job,mean_income), y=mean_income)) +
  geom_col() +
  coord_flip()

# 하위 10개
bottom10=job_income %>% 
  arrange(mean_income) %>% 
  head(10)
bottom10   

#그래프
ggplot(data=bottom10, aes(x=reorder(job,mean_income), y=mean_income)) +
  geom_col() +
  coord_flip()

###성별 직업 빈도
#"성별로 어떤 직업이 가장 많을까?

# 남성 직업 빈도 상위 10개 추출
job_male=welfare %>% 
  filter(!is.na(job)& sex=="male") %>% 
  group_by(job) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
job_male

# 여성 직업 
job_female=welfare %>% 
  filter(!is.na(job)& sex=="female") %>% 
  group_by(job) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
job_female

# 그래프(남성)
ggplot(data = job_male, aes(x=reorder(job,n), y=n)) +
  geom_col() +
  coord_flip()


### 종교 유무에 따른 이혼율
#1-1종교 변수검토
class(welfare$religion)
table(welfare$religion)
welftable(welfare$religion)are$religion=ifelse(welfare$religion==1,"yes","no")
qplot(welfare$religion)

#1-2 변수
class(welfare$marriage)
table(welfare$marriage)

# 이혼 여부 변수 만들기
welfare$group_marriage = ifelse(welfare$marriage ==1,"marriage",
                                ifelse(welfare$marriage==3,"divorce",NA))
table(welfare$group_marriage)
qplot(welfare$group_marriage)

# 종교유무에 따른 이혼율
religion_marriage=welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion,group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100,1))

religion_marriage

divorce= religion_marriage %>% 
  filter(group_marriage == "divorce") %>% 
  select(religion,pct)

divorce

# 그래프
ggplot(data=divorce, aes(x = religion, y=pct))+geom_col()
  
#연령대별 이혼율 표 만들기
ageg_marriage=welfare %>% 
    filter(!is.na(group_marriage)) %>% 
    group_by(ageg,group_marriage) %>% 
    summarise(n=n()) %>% 
    mutate(tot_group = sum(n)) %>% 
    mutate(pct = round(n/tot_group*100,1))

ageg_marriage

# 초년 제외, 이혼추출
age_divorce=ageg_marriage %>% 
  filter(ageg !="young" & ageg_marriage=="divorce") %>% 
  select(ageg,pct)

#그래프
ggplot(data=ageg_divorce, aes(x = ageg, y=pct)) +geom_col()

# 연령대, 종교유무, 결혼상태별 비율표 만들기
ageg_religion_marriage= welfare %>% 
  filter(!is.na(group_marriage))&ageg !="young")%>%
  group_by(ageg,religion,group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100,1))

# 연령대 및 종교 유므별 이혼율 표 만들기
df_divorce = ageg_religion_marriage %>% 
  filter(group_marriage =="divorce") %>% 
  select(ageg,pct)

# 그래프
ggplot(data=df_divorce, aes(x = ageg, y=pct, fill=religion)) +
  geom_col(position="dodge")

### 지역별 연령대 비율
#"노년층이 많은 지역은 어디일까?"
#변수처리:지역
class(welfare$code_region)
table(welfare$code_region) 

list_region = data.frame(code_region=c(1:7),
                          region = c("서울",
                                     "수도권(인천/경기)",
                                     "부산/경남/울산",   
                                     "대구/경북",
                                     "대전/충남" ,
                                     "강원/충북"  ,             
                                     "광주/전남/전북/제주도"))

list_region

welfare=left_join(welfare, list_region, id="code_region" )

welfare %>% 
    select(code_region,region) %>% 
    head

region_ageg = welfare %>% 
  group_by(region,ageg) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100,2))
head(region_ageg)

#그래프
ggplot(data=region_ageg, aes(x =region, y=pct, fill=ageg)) +
  geom_col()+
  coord_flip()

#막대 정렬하기:노년층 비율 높은순
list_order_old = region_ageg %>% 
  filter(ageg=="old") %>% 
  arrange(pct)

list_order_old

#지역명 순서 변수 만들기
order = list_order_old$region
order

# 연령대 순으로 막대 색깔 나열하기
class(region_ageg$ageg)

levels(region_ageg$ageg)

region_ageg$ageg=factor(region_ageg$ageg, #범주형 타입 factor로 바꿈
                        level = c("old","middle","young"))
class(region_ageg$ageg)
levels(region_ageg$ageg)

#그래프
ggplot(data=region_ageg, aes(x =region, y=pct, fill=ageg)) +
  geom_col()+
  coord_flip()+
  scale_x_discrete(limits = order)
