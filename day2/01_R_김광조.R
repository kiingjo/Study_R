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
  select(name,year,NewPts) %>% 
  group_by(name,year) %>% 
  summarise(mean_NewPts=mean(as.numeric(NewPts)))

ggplot(data=year_NewPts, aes(x = year, y=mean_NewPts)) +
  geom_col()
####################################################################