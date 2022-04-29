library(dplyr)
library(readxl)


exam=read.csv("./Data/csv_exam.csv")
exam

exam %>% arrange(math) # math 오름차순 정렬
exam %>% arrange(desc(math)) # math 내림차순 정렬
exam %>% arrange(class, math) # class, math 오름차순

# 내장돼있는 함수
mpg=as.data.frame(ggplot2::mpg)
mpg
head(mpg,10)    

tmp=mpg %>% filter(manufacturer=="audi")
tmp1=tmp %>% select( manufacturer ,hwy )
head(tmp1 %>% arrange(desc(hwy)),5)

# 한줄로 나타내기
mpg %>% filter(manufacturer=="audi")%>% arrange(desc(hwy))%>% head(5)



# 새로운 컬럼 만들기
exam
#mutate는 추가가 아닌 조회함수
exam1=exam %>% # 출력위해 exam1 변수지정정
  mutate(total = math + english + science,
         mean = (math + english + science)/3,
         test= ifelse(science>=60,"pass","fail")) %>%head #조건절도 넣을수있다
exam1

head(mpg)
mpg1=mpg %>% 
  mutate( totalgas = cty + hwy) %>% head

##########################################

mpg1 %>% 
  mutate(meangas = totalgas/2) %>% head(3)

###########################################

mpg %>% 
  mutate(totalgas = cty + hwy,
         meangas = (cty + hwy)/2) %>% 
  head(3)

# 한줄안에 내가 선언한 변수 재사용 가능
mpg %>% 
  mutate(total = cty + hwy,
         mean = total/2) %>%
    arrange(desc(mean)) %>% 
              head(3)

# 요약하기

exam %>% 
  group_by(class) %>% # class별로 분리
    summarise(mean_math = mean(math)) # math 평균 산출

exam %>% 
  group_by(class) %>% # 반별로 묶기
  summarise(mean_math = mean(math), # summarise 요약
            sum_math = sum(math),
            median_math = median(math),
            n = n()) # class 빈도수

# n() : 빈도수
# median() : 중앙값


mpg %>% 
  group_by(manufacturer,drv) %>% # 회사, 구동방식 같은 것끼리 그룹화
    summarise(mean_cty = mean(cty),
              n = n()) %>%
      head(10)
######################################
mpg

mpg %>% 
  group_by(manufacturer) %>% 
  filter(class=="suv") %>% 
  mutate(total = (cty + hwy)/2) %>%
  summarise(mean_total= mean(total)) %>% 
  arrange(desc(mean_total)) %>% 
  head(5)

head(mpg)

#####################################################1

mpg %>% 
  group_by(class) %>% 
    summarise(mean_cty=mean(cty))

#####################################################2
mpg %>% 
  group_by(class) %>% 
  summarise(mean_cty=mean(cty))
  arrange(desc(mean_cty))
#####################################################3
mpg %>% 
  group_by(class) %>% 
  summarise(mean_hwy=mean(hwy)) %>% 
  arrange(desc(mean_hwy)) %>% 
            head(3)

#####################################################
mpg %>% 
  filter(class=="compact") %>% 
  group_by(manufacturer) %>% # 걸러낼거 쳐낸다음 그룹화하면 속도빠름
  summarise(n=n()) %>% # 빈도수
  arrange(desc(n))


test1=data.frame(id = c(1,2,3,4,5),
                midterm = c(60,70,80,90,85))

test2=data.frame(id = c(1,2,3,4,5),
                 final = c(70,83,65,95,80))

# join(조인)
total=left_join(test1,test2, by="id") # id기준으로 합쳐 total에 할당
total #출력

name=data.frame(class=c(1,2,3,4,5),
                teacher=c("kim","lee","park","choi","jung"))
  
name

exam

exam_new=left_join(exam,name, by="class")
exam_new

group_a=data.frame(id=c(1,2,3,4,5),
                   test = c(60,80,70,90,85))

group_b=data.frame(id=c(6,7,8,9,10),
                   test = c(70,83,65,95,80))

# 세로로 합치기(각 그룹당의 컬럼 갯수 같아야함)
group_all=bind_rows(group_a, group_b)
group_all


fuel=data.frame(fl=c("c","d","e","p","r"),
                price_fl=c(2.35,2.38,2.11,2.76,2.22),
                stringsAsFactors = F) # F가 원래 디폴트값

fuel

head(mpg)

# fl데이터에 따른 연료가격을 추가해라 (조인해라) 
tmp=left_join(mpg,fuel,by="fl")
tmp
############################################
# 원하는 컬럼만 뽑아내기(추출하기)
tmp %>% 
  select(model,fl,price_fl) %>% 
  head(5)






