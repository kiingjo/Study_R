####텍스트마이닝(Text mining)
#문자로 된 데이터에서 정보얻어내는 분석 기법
#사람들이 어떤 이야기를 나누고 있는지 파악할 때 사용
#형태소분석:문장을 구성하는 어절들이 어떤 품사로 되어 있는지 분석
#분석절차
#-형태소 분석
#-명사, 동사 형용사 등 의미를 지닌 품사 단어 추출
#-빈도표 만들기
#-시각화(워드클라우드)

install.packages("usethis")
usethis::edit_r_environ() # 탭하나 뜬다 #Rtools와 java연결
#PATH="${RTOOLS40_HOME}\usr\bin;${PATH}" 이거 적기

Sys.which("make")

install.packages("rJava")
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))


library(KoNLP)
text <- "R은 통계 계산과 그래픽을 위한 프로그래밍 언어이자 소프트웨어 환경이자 프리웨어이다.[2] 뉴질랜드 오클랜드 대학의 로버트 젠틀맨(Robert Gentleman)과 로스 이하카(Ross Ihaka)에 의해 시작되어 현재는 R 코어 팀이 개발하고 있다. R는 GPL 하에 배포되는 S 프로그래밍 언어의 구현으로 GNU S라고도 한다. R는 통계 소프트웨어 개발과 자료 분석에 널리 사용되고 있으며, 패키지 개발이 용이해 통계 소프트웨어 개발에 많이 쓰이고 있다."
extractNoun(text)

#패키지로드
library(dplyr)
library(KoNLP)

# 사전 설정하기
useNIADic() #형태소 분석할 수 있는 문법이 들어있다

#데이터불러오기
txt=readLines("./Data/hiphop.txt", encoding = "UTF-8")
head(txt)


install.packages("stringr")
library(stringr)

# 특수문자 제거
txt=str_replace_all(txt,"\\W"," ") # 정규 표현식 
#한글과 영문을 제외한 나머지를 찾을 때 공백으로 찾겠다

# 가장 많이 사용된 단어 알아보기
# 명사 추출하기
extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다")

#가사에서 명사추출
nouns=extractNoun(txt)

# 추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount=table(unlist(nouns))


#데이터 프레임으로 변환
df_word = as.data.frame(wordcount,stringsAsFactors=F)


# 변수명 수정
df_word = rename(df_word,
                 word=Var1,
                 freq=Freq)

# 두 글자 이상 단어 추출

df_word=filter(df_word,nchar(word)>=2)

df_word=df_word %>% filter(nchar(word)>=2) # 위와같은의미



top_20=df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)

top_20

# 워드클라우드 만들기
# 패키지 준비
install.packages("wordcloud")
library(wordcloud)

pal=brewer.pal(8,"Dark2") #Dark2색상 목록중 8개 색상 사용

#워드클라우드 생성
set.seed(1234) #난수고정
wordcloud(words = df_word$word, # 난수 고정
          freq=df_word$freq,    # 단어
          min.freq = 2,         # 빈도
          max.words = 200,      # 최소 단어 빈도
          random.order = F,     # 고빈도 단어중앙배치
          rot.per = .1,         # 회전 단어 비율
          scale = c(4,0.3),     # 단어 크기 범위
          colors = pal)         # 색깔목록



###########국정원 트윗테스트 마이닝
#데이터 준비
#데이터 로드
twitter=read.csv("./Data/twitter.csv",
                 header=T,
                 stringsAsFactors = F,
                 fileEncoding = "UTF-8")
str(twitter)

#변수명 수정
twitter=rename(twitter,
               no = 번호,
               id = 계정이름,
               date = 작성일,
               tw = 내용)

str(twitter)

#특수문자 제거 -> 한글 깨짐 오류 미확인..
#twitter$tw=str_replace_all(twitter$tw,"\\W"," ")
#head(twitter)
# 명사 추출
nouns=extractNoun(twitter$tw)
# 추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount=table(unlist(nouns))

#데이터 프레임으로 변환
df_word = as.data.frame(wordcount,stringsAsFactors=F)

#변수명 수정
df_word = rename(df_word,
                 word=Var1,
                 freq=Freq)
str(df_word)
#두글자 이상 단어만 추출
df_word= filter(df_word, nchar(word)>=2)

#상위 20개 추출
top_20=df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)

# 단어 빈도 막대 그래프 만들기

library(ggplot2)

order = arrange(top_20,freq)$word

ggplot(data=top_20,aes(x=word,y=freq)) +
  ylim(0,2500) +
  geom_col() +
  coord_flip() + #수평하게 그림
  scale_x_discrete(limit = order) + # 빈도 순서 변수 기준 막대 정렬
  geom_text(aes(label=freq),hjust = -0.3)        # 빈도 표시



# 워드 클라우드 만들기
pal=brewer.pal(8,"Dark2")
set.seed(1234) #난수고정


wordcloud(words = df_word$word, # 난수 고정
          freq=df_word$freq,    # 단어
          min.freq = 10,        # 빈도
          max.words = 200,      # 최소 단어 빈도
          random.order = F,     # 고빈도 단어중앙배치
          rot.per = .1,         # 회전 단어 비율
          scale = c(6,0.2),     # 단어 크기 범위
          colors = pal)         # 색깔목록



#색상 바꾸기
pal=brewer.pal(9,"Blues")[5:9] # 이것만 바꿔주면 된다.


##################################################################
#대통령 연설문-1:김영삼

#데이터 불러오기

txt=readLines("./Data/president_speech/kim_young_sam.txt", encoding = "UTF-8")
               
str(txt)

# 특수문자 제거
txt=str_replace_all(txt,"\\W"," ") # 정규 표현식 
#한글과 영문을 제외한 나머지를 찾을 때 공백으로 찾겠다


#연설문에서 명사추출
nouns=extractNoun(txt)

# 추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount=table(unlist(nouns))


#데이터 프레임으로 변환
df_word = as.data.frame(wordcount,stringsAsFactors=F)


# 변수명 수정
df_word = rename(df_word,
                 word=Var1,
                 freq=Freq)

# 두 글자 이상 단어 추출

df_word=df_word %>% filter(nchar(word)>=2) # 위와같은의미


# 워드 클라우드 만들기
pal=brewer.pal(8,"Dark2")
set.seed(1234) #난수고정


wordcloud(words = df_word$word, # 난수 고정
          freq=df_word$freq,    # 단어
          min.freq = 2,        # 빈도
          max.words = 200,      # 최소 단어 빈도
          random.order = F,     # 고빈도 단어중앙배치
          rot.per = .1,         # 회전 단어 비율
          scale = c(6,0.2),     # 단어 크기 범위
          colors = pal)         # 색깔목록
##############################################################
#대통령 연설문-1:노무현

#데이터 불러오기

txt=readLines("./Data/president_speech/Roh Moo-hyun.txt", encoding = "UTF-8")

str(txt)

# 특수문자 제거
txt=str_replace_all(txt,"\\W"," ") # 정규 표현식 
#한글과 영문을 제외한 나머지를 찾을 때 공백으로 찾겠다


#연설문에서 명사추출
nouns=extractNoun(txt)

# 추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount=table(unlist(nouns))


#데이터 프레임으로 변환
df_word = as.data.frame(wordcount,stringsAsFactors=F)


# 변수명 수정
df_word = rename(df_word,
                 word=Var1,
                 freq=Freq)

# 두 글자 이상 단어 추출

df_word=df_word %>% filter(nchar(word)>=2) # 위와같은의미


# 워드 클라우드 만들기
pal=brewer.pal(8,"Dark2")
set.seed(1234) #난수고정


wordcloud(words = df_word$word, # 난수 고정
          freq=df_word$freq,    # 단어
          min.freq = 5,        # 빈도
          max.words = 200,      # 최소 단어 빈도
          random.order = F,     # 고빈도 단어중앙배치
          rot.per = .1,         # 회전 단어 비율
          scale = c(6,0.2),     # 단어 크기 범위
          colors = pal)         # 색깔목록
####################################################################
#대통령 연설문-1:김대중

#데이터 불러오기

txt=readLines("./Data/president_speech/kim_dea_joong.txt", encoding = "UTF-8")

str(txt)

# 특수문자 제거
txt=str_replace_all(txt,"\\W"," ") # 정규 표현식 
#한글과 영문을 제외한 나머지를 찾을 때 공백으로 찾겠다


#연설문에서 명사추출
nouns=extractNoun(txt)

# 추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount=table(unlist(nouns))


#데이터 프레임으로 변환
df_word = as.data.frame(wordcount,stringsAsFactors=F)


# 변수명 수정
df_word = rename(df_word,
                 word=Var1,
                 freq=Freq)

# 두 글자 이상 단어 추출

df_word=df_word %>% filter(nchar(word)>=2) # 위와같은의미


# 워드 클라우드 만들기
pal=brewer.pal(8,"Dark2")
set.seed(1234) #난수고정


wordcloud(words = df_word$word, # 난수 고정
          freq=df_word$freq,    # 단어
          min.freq = 10,        # 빈도
          max.words = 200,      # 최소 단어 빈도
          random.order = F,     # 고빈도 단어중앙배치
          rot.per = .1,         # 회전 단어 비율
          scale = c(6,0.2),     # 단어 크기 범위
          colors = pal)         # 색깔목록
##################################################################
#대통령 연설문-1:전두환

#데이터 불러오기

txt=readLines("./Data/president_speech/jun_doo_hyan.txt", encoding = "UTF-8")

str(txt)

# 특수문자 제거
txt=str_replace_all(txt,"\\W"," ") # 정규 표현식 
#한글과 영문을 제외한 나머지를 찾을 때 공백으로 찾겠다


#연설문에서 명사추출
nouns=extractNoun(txt)

# 추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount=table(unlist(nouns))


#데이터 프레임으로 변환
df_word = as.data.frame(wordcount,stringsAsFactors=F)


# 변수명 수정
df_word = rename(df_word,
                 word=Var1,
                 freq=Freq)

# 두 글자 이상 단어 추출

df_word=df_word %>% filter(nchar(word)>=2) # 위와같은의미

#상위 20개 
top_20= df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)
top_20

# 단어 빈도 막대 그래프 만들기

library(ggplot2)

order = arrange(top_20,freq)$word

ggplot(data=top_20,aes(x=word,y=freq)) +
  ylim(0,50) +
  geom_col() +
  coord_flip() + #수평하게 그림
  scale_x_discrete(limit = order) + # 빈도 순서 변수 기준 막대 정렬
  geom_text(aes(label=freq),hjust = -0.3)        # 빈도 표시


# 워드 클라우드 만들기
pal=brewer.pal(8,"Dark2")
set.seed(1234) #난수고정


wordcloud(words = df_word$word, # 난수 고정
          freq=df_word$freq,    # 단어
          min.freq = 10,        # 빈도
          max.words = 200,      # 최소 단어 빈도
          random.order = F,     # 고빈도 단어중앙배치
          rot.per = .1,         # 회전 단어 비율
          scale = c(6,0.2),     # 단어 크기 범위
          colors = pal)         # 색깔목록




