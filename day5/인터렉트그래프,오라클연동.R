getwd()
setwd("C:/Recture/day5")
########plotly 패키지로 인터랙티브 그래프 만들기
#인터랙티브 그래프 만들기
#패키지 준비
install.packages("plotly")
library(plotly)

#ggplot으로 그래프만들기
library(ggplot2)
mpg=as.data.frame(ggplot2::mpg)
mpg
p=ggplot(data=mpg, aes(x=displ, y=hwy, col=drv)) + geom_point()

#인터렉티브 그래프 만들기
ggplotly(p)

#
p=ggplot(data=diamonds, aes(x=cut, fill=clarity))+ #x만 있을 땐 빈도
  geom_bar(position="dodge") #dodge:여러개 막내 생성

#dygraphs 패키지로 인터랙티브 시계열 그래프 만들기
#인터랙티브 시계열 그래프 만들기
#패키지 준비
install.packages("dygraphs")
library(dygraphs)

dconomics=ggplot2::economics
head(economics)

#시간 순서 속성을 지니는 xts데이터 타임으로 변경
library(xts)

eco=xts(economics$unemploy,order.by = economics$date)
head(eco)

#그래프 생성
dygraph(eco)

#날짜범위 선택가능 그래프
dygraph(eco) %>% dyRangeSelector()

##여러 값 표현하기
#저축률
eco_a=xts(economics$psavert,order.by = economics$date)

#실업자 수 
eco_b = xts(economics$unemploy/1000,order.by = economics$date )

#합치기

eco2=cbind(eco_a,eco_b)
colnames(eco2)=c("psavert","unemploy")
head(eco2)

dygraph(eco2) %>% dyRangeSelector()


#ojdbc.jar를 이요하여 데이터 접속을 위한 라이브러리

##-사전 설치 상황
##-jdk 설치 및 환경변수 등록하기

install.packages("RJDBC")
library(RJDBC)

# 오라클 드라이버 연결 경로 설정
driver=JDBC("oracle.jdbc.OracleDriver",
            classPath = "C:/DEV/Server/Oracle/product/12.2.0/dbhome_1/jdbc/libojdbc8.jar" )

driver

# 오라클 접속하기 # 서로를 연결하는 통로를 뚫어줬다
conn= dbConnect(driver,
                "jdbc:oracle:thin:@//localhost:1521/orcl",
                "Scott","tiger")


conn

conn= dbConnect(driver,
                "jdbc:oracle:thin:@//localhost:1521/orcl",
                "BUSAN","DBDB")

sql_in =paste("Insert into test",
              "(AA,BB,CC)",
              "values('a1','b1','c1')")
sql_in

in_stat=dbSendQuery(conn,sql_in) #dbSendQuery: 요청

############데이터 조건 [조회]하기
sql_sel = "Select * from test where AA='a1'"
sql_sel

getData = dbGetQuery(conn,sql_sel)
getData

getData$AA

str(getData)


#############**중요**필수- 무조건 오라클 접속 해제하기
dbDisconnect(conn)






