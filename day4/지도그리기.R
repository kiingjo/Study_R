################지도 시각화
#11-1 미국 주별 강력 범죄율 단계 구분도 만들기
#패키지 준비하기
install.packages("ggiraphExtra")
library(ggiraphExtra)

str(USArrests)

head(USArrests) #지역이 인덱스로 되어있다

library(tibble)

# 행 이름을 state 변수로 바꿔 데이터 프레임 생성
# 인덱스를 컬럼으로 바꿔주는 방법
crime=rownames_to_column(USArrests, var = "state")
head(crime)

# state의 값을 소문자로 수정
crime$state= tolower(crime$state)

str(crime)

# 미국 주 지도 데이터 준비하기
library(ggplot2)
states_map = map_data("state")
str(states_map)

# 단계구분도 만들기
ggChoropleth(data=crime,         #지도에 표현할 데이터
             aes(fill=Murder,    #색깔로 표현할 변수
                 map_id=state),  #지역 기준 변수
             map=states_map)     #지도 데이터

# 인터렉티브 단계 구분도 만들기(조금 더 세련된 그래프)

ggChoropleth(data=crime,         
             aes(fill=Murder,   
                 map_id=state),  
             map=states_map,
             interactive = T)   # 다른점
#11-2 대한민국 시도별 인구, 결핵환자 수 단게 구분도 만들기
#대한민국 시도별 인구 단계 구분도 만들기

#패키지 준비하기
install.packages("stringi")

install.packages("devtools")

devtools::install_github("cardiomoon/kormaps2014")
library(kormaps2014)

#대한민국 시도별 인구 데이터 준비하기
str(changeCode(korpop1))
library(dplyr)

#컬럼명 바꾸기
korpop1 = rename(korpop1,
                  pop = 총인구_명,
                  name= 행정구역별_읍면동)

str(changeCode(korpop1))

#단계구분도 만들기
ggChoropleth(data=korpop1,     #지도에 사용할 데이터    
             aes(fill=pop,     #색깔로 표현할 컬럼(변수)
                 map_id=code,  #지역 기준 변수
                 tooltip=name),#지도 위에 표시할 지역명  
             map=kormap1,      #지도 데이터
             interactive = T)  #인터렉티브(좀더 깔끔하게 표현)


#대한민국 시도별 결핵 환자 수 단계 구분도 만들기
str(changeCode(tbc))
head(changeCode(tbc))

ggChoropleth(data=tbc,     #지도에 사용할 데이터    
             aes(fill=NewPts,     #색깔로 표현할 컬럼(변수)
                 map_id=code,  #지역 기준 변수
                 tooltip=name),#지도 위에 표시할 지역명  
             map=kormap1,      #지도 데이터
             interactive = T)  #인터렉티브(좀더 깔끔하게 표현)


##################################################################
str(changeCode(korpop1))
head(changeCode(korpop1))

str(changeCode(tbc))
head(changeCode(tbc))

korpop1_copy=korpop1
tbc_copy=tbc
kormap1_copy=kormap1

str(changeCode(korpop1_copy))

head(changeCode(korpop1_copy))
changeCode(korpop1_copy)$code

str(changeCode(tbc_copy))
tail(changeCode(tbc_copy))
changeCode(korpop1_copy) %>% 
  arrange(desc(시점)) %>% 
  head(1)
changeCode(tbc_copy)$NewPts
changeCode(tbc_copy)
table(is.na(changeCode(tbc_copy)$NewPts))
tmp4=changeCode(tbc_copy) %>% 
  filter(!is.na(NewPts))
tmp4$NewPts
#####################지역별 결핵환자 비율
# 조인으로 코드를 기준으로 결핵환자 합치기

tmp=changeCode(tbc_copy) %>%  #결측치 없으므로 필터 굳이 X
  filter(year=="2015") %>% 
  select(code,NewPts)
tmp


tmp1=left_join(changeCode(korpop1_copy),tmp,by="code")
str(tmp1)

tmp2= tmp1 %>% 
  mutate(ratio=as.numeric(NewPts)/as.numeric(pop)*10000)
tmp2 %>% 
  select(name,NewPts)

ggChoropleth(data=tmp2,     #지도에 사용할 데이터    
             aes(fill=ratio,     #색깔로 표현할 컬럼(변수)
                 map_id=code,  #지역 기준 변수
                 tooltip=name),#지도 위에 표시할 지역명  
             map=kormap1,      #지도 데이터
             interactive = T)  #인터렉티브(좀더 깔끔하게 표현)
###################################################################
#지역별 외국인 비율

str(tmp1)
tmp1=rename(tmp1,
            for_tot=외국인_계_명,
            family_tot=가구_계_가구)

tmp3=tmp1 %>%
 mutate(for_ratio=as.numeric(for_tot)/as.numeric(pop)*100)
str(tmp3)
ggChoropleth(data=tmp3,     #지도에 사용할 데이터    
             aes(fill=for_ratio,     #색깔로 표현할 컬럼(변수)
                 map_id=code,  #지역 기준 변수
                 tooltip=name),#지도 위에 표시할 지역명  
             map=kormap1,      #지도 데이터
             interactive = T)  #인터렉티브(좀더 깔끔하게 표현)
###################################################################  
#지역별 가구당 인원
tmp4=tmp1 %>% 
  mutate(family_mem=as.numeric(pop)/as.numeric(family_tot))

str(tmp4)
summary(tmp4$family_mem)

ggChoropleth(data=tmp4,     #지도에 사용할 데이터    
             aes(fill=family_mem,     #색깔로 표현할 컬럼(변수)
                 map_id=code,  #지역 기준 변수
                 tooltip=name),#지도 위에 표시할 지역명  
             map=kormap1,      #지도 데이터
             interactive = T)  #인터렉티브(좀더 깔끔하게 표현)
####################################################################
#년도별 평균결핵환자수(막대그래프)
str(changeCode(tbc_copy))
table(is.na(changeCode(tbc_copy)$NewPts))

year_NewPts=changeCode(tbc_copy) %>%
  filter(!is.na(NewPts)) %>% 
   group_by(name,year) %>% 
  summarise(mean_NewPts=mean(as.numeric(NewPts)))

ggplot(data=year_NewPts, aes(x = year, y=mean_NewPts)) +
  geom_col()
####################################################################
head(changeCode(tbc_copy))


tmp5 <- changeCode(tbc_copy) %>% 
  filter(!is.na(NewPts)) %>% 
  select(name,year,NewPts) %>% 
  group_by(name, as.numeric(year)) %>% 
  summarise(mean_NewPts = mean(as.numeric(NewPts)))
            
ggplot(data = tmp5, aes(x = as.numeric(year), y = mean_NewPts, col = name))+geom_line()


year_NewPts=changeCode(tbc_copy) %>%
  filter(!is.na(NewPts)) %>% 
  group_by(name,year) %>% 
  summarise(mean_NewPts=mean(as.numeric(NewPts)))


