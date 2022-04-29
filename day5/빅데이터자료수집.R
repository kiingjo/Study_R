# 빅데이터 자료수집 방법
#1)파일 데이터셋 자료수집
#- 공익적인 목적에서 제공하는 파일 데이터 셋을 통하여 자료를 수집
#- ex) 국가통계포털(검증된 데이터)
#2)웹스크래핑
#3)오픈 api기반 자료수집


####################1)파일 데이터셋 자료수집
getwd()

data=read.csv("./Data/전라남도_목포시_장애인_복지시설_20210802.csv",
              header=T, encoding="EUC-KR")

data
head(data)


######################2)웹스크래핑      
#순서: 웹스크레핑 대상 URL할당 ->웹문서 가져오기
#     ->특정 태그의 데이터 추출->데이터 정제->데이터프래임만들기

install.packages("rvest")
library(rvest)
install.packages("stringr")
library(stringr)

#1) 웹 스크래핑 대상 URL값 할당
url="https://www.bobaedream.co.kr/cyber/CyberCar.php?gubun=K&page=1"
url

#2) 웹 문서 가져오기
usedCar= read_html(url)
# 3) 특정 태그의 데이터 추출
# 가져온 usedCar에서 css가 ".product-item"인 것을 찾음.
carInfos <- html_nodes(usedCar, css=".product-item") 
head(carInfos)

# 차량 명칭 추출
title_tmp <- html_nodes(carInfos, css=".tit.ellipsis")
title_tmp

title=html_text(title_tmp) #텍스터화
title

title=str_trim(title) #공백제거 and 출력
title


# 차량 연식 추출
year_tmp = html_nodes(carInfos,css=".mode-cell.year")
year_tmp

year=html_text(year_tmp) #텍스터화
year

year=str_trim(year) #공백제거 and 출력
year

# 연료구분
fuel_tmp = html_nodes(carInfos,css=".mode-cell.fuel")
fuel_tmp

fuel=html_text(fuel_tmp) #텍스터화
fuel

fuel=str_trim(fuel) #공백제거 and 출력
fuel

#주행거리 추출
km_tmp = html_nodes(carInfos,css=".mode-cell.km")
km_tmp

km=html_text(km_tmp) #텍스터화
km

km=str_trim(km) #공백제거 and 출력
km

#판매가격 추출
price_tmp = html_nodes(carInfos,css=".mode-cell.price")
price_tmp

price=html_text(price_tmp) #텍스터화
price


price=str_trim(price) #공백제거 and 출력
price

price=str_replace(price,'\n','') #줄바꿈 삭제
price

# 차량 명칭으로부터 제조사 추출
maker=c()
maker


title

for(i in 1:length(title)){
maker=c(maker, unlist(str_split(title[i], ' '))[1]) #str_split 문자열 분리

}
maker



