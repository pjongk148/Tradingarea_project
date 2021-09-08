
#필요 패키지 다운로드 
#install.packages("lubridate")
#install.packages("readxl")
#install.packages("forcats")

# 패키지 활성화
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(tidyr)
library(reshape2)
library(forcats)
options(scipen=99)
#데이터 추출

data <- read.csv("data/경기도 수원시_지역화폐 결제 정보_20201211.csv")
View(data)

data2 <- read_excel(path="data/코로나 확진자수 및 거리두기 단계.xlsx")
View(data2)

#전처리 과정


##### EDA #####

####단순 비교

### 코로나 전후
before <- data %>% filter(data$기준년월 < ymd("2020-05-01"))
after <-data %>% filter(data$기준년월 >= ymd("2020-05-01"))

table(after$업종명)-table(before$업종명)

## 코로나 전후
b_job <- before %>% group_by(업종명) %>% select(업종명,결제건수,결제금액) %>%
        summarise(total_pay=sum(결제금액),total_num=sum(결제건수))
View(b_job)

a_job <- after %>% group_by(업종명) %>% select(업종명,결제건수,결제금액) %>%
  summarise(total_pay=sum(결제금액),total_num=sum(결제건수))
View(a_job)

a_job[,"ab"]="after"
View(b_job)

b_job[,"ab"]="before"
per <- cbind(a_job[1],data.frame(a_job$total_pay/b_job$total_pay),data.frame(a_job$total_num/b_job$total_num))
per <- rename(per,per_pay=a_job.total_pay.b_job.total_pay,per_num=a_job.total_num.b_job.total_num)
View(per)
per3 <- per %>% arrange(desc(per_pay,per_num)) %>% head(3)
per3


View(a_job)

per2 <- melt(per, id.vars="업종명",variable.name="per_name",
                  value.name ="value")
View(per2)

ab_job <- rbind(b_job,a_job)
View(ab_job)
ab_total <- ab_job %>% select(ab,total_num,total_pay) %>% group_by(ab) %>% summarise(total_pay=sum(total_pay),total_num=sum(total_num))
a_job[,"num_per"] <- data.frame(a_job$total_num/b_job$total_num)


View(per)

ab_total

##### 1번 그래프: 코로나 전후 총 결제금액
ggplot(ab_total, aes(reorder(ab, total_pay), total_pay/100000000,fill=ab)) +geom_col() +
xlab("코로나 전후") + ylab("총 결제금액 (단위: 억)")

##### 2번 그래프: 코로나 전후 총 결제건수
ggplot(ab_total, aes(reorder(ab, total_num), total_num/10000,fill=ab)) +geom_col() +
  xlab("코로나 전후") + ylab("총 결제건수 (단위: 만)")

##### 3번 그래프 코로나 전후 업종별 결제금액 & 건수 변경
per22 <- per %>% arrange(desc(per_pay)) %>% head(5)
per222 <- per %>% arrange(desc(per_num)) %>% head(5)
per22
ggplot(per22, aes(reorder(업종명, per_pay), per_pay, fill=업종명)) +geom_col() +
  xlab("업종명") + ylab("결제금액 증감비율")
  
  
ggplot(per222, aes(reorder(업종명, per_num), per_num, fill=업종명)) +geom_col() +
  xlab("업종명") + ylab("결제건수 증감비율")

ggplot(per2, aes(x= 업종명, y= value, fill=per_name)) +geom_col() +
  ylab("전/후 비율")

b_total <- before %>% summarise(total=sum(before$결제금액))
b_num <- before %>% summarise(total=sum(before$결제건수))
cbind(a_job,a_job[2]-b_job[2])
mean(a_job[2]-b_job[2])
k <-a_job[2]-b_job[2]

k %>% select(total_job) %>% summarise(mean_k=mean(total_job))

##남녀 비교
View(before)


#전, 남
b_m <- before %>% filter(성별=="남")
b_m_total_pay <- b_m %>% group_by(업종명) %>% summarise(total_pay=sum(결제금액),total_num=sum(결제건수)) %>% arrange(desc(total_pay)) %>% head(5)
b_m_total_num <- b_m %>% group_by(업종명) %>% summarise(total_pay=sum(결제금액),total_num=sum(결제건수)) %>% arrange(desc(total_num)) %>% head(5)
b_m_total[,"ba"]="before"
View(b_m_total)

#전, 여
b_w <- before %>% filter(성별=="여")
b_w_total_pay <- b_w %>% group_by(업종명) %>% summarise(total_pay=sum(결제금액),total_num=sum(결제건수)) %>% arrange(desc(total_pay)) %>% head(5)
b_w_total_num <- b_w %>% group_by(업종명) %>% summarise(total_pay=sum(결제금액),total_num=sum(결제건수)) %>% arrange(desc(total_num)) %>% head(5)

b_w_total[,"ba"]="before"
View(b_w_total)

#후, 남
a_m <- after %>% filter(성별=="남")
a_m_total_pay <- a_m %>% group_by(업종명) %>% summarise(total_pay=sum(결제금액),total_num=sum(결제건수)) %>% arrange(desc(total_pay)) %>% head(5)
a_m_total_num <- a_m %>% group_by(업종명) %>% summarise(total_pay=sum(결제금액),total_num=sum(결제건수)) %>% arrange(desc(total_num)) %>% head(5)
a_m_total[,"ba"]="after"

#후, 여
a_w <- after %>% filter(성별=="여")
a_w_total_pay <- a_w %>% group_by(업종명) %>% summarise(total_pay=sum(결제금액),total_num=sum(결제건수)) %>% arrange(desc(total_pay)) %>% head(5)
a_w_total_num <- a_w %>% group_by(업종명) %>% summarise(total_pay=sum(결제금액),total_num=sum(결제건수)) %>% arrange(desc(total_num)) %>% head(5)


a_w_total_pay[,"ba"]="after"
a_w_total_num[,"ba"]="after"
a_m_total_pay[,"ba"]="after"
a_m_total_num[,"ba"]="after"
b_w_total_pay[,"ba"]="before"
b_w_total_num[,"ba"]="before"
b_m_total_pay[,"ba"]="before"
b_m_total_num[,"ba"]="before"




View(m_total_pay)

m_total_pay <- rbind(b_m_total_pay,a_m_total_pay)
m_total_num <- rbind(b_m_total_num,a_m_total_num)
w_total_pay <- rbind(b_w_total_pay,a_w_total_pay)
w_total_num <- rbind(b_w_total_num,a_w_total_num)
w_total_num
#코로나 전, 후 남녀 결제금액 확인
ggplot(m_total_pay, aes(reorder(x= 업종명, total_pay), y= total_pay/100000000,fill=ba %>% fct_rev() )) +geom_col(position ="dodge") +
  xlab("업종명") + ylab("결제금액(단위: 억)") + scale_fill_discrete(name='남, 결제금액')


ggplot(w_total_pay, aes(reorder(업종명, total_pay), total_pay/100000000, fill=ba %>% fct_rev())) +geom_col(position ="dodge") +
  xlab("업종명") + ylab("결제금액(단위: 억)") + scale_fill_discrete(name='여, 결제금액')

ggplot(m_total_num, aes(reorder(x= 업종명, total_num), y= total_num/10000,fill=ba %>% fct_rev() )) +geom_col(position ="dodge") +
  xlab("업종명") + ylab("결제금액(단위: 만)") + scale_fill_discrete(name='남, 결제건수')


ggplot(w_total_num, aes(reorder(업종명, total_num), total_num/10000, fill=ba %>% fct_rev())) +geom_col(position ="dodge") +
  xlab("업종명") + ylab("결제금액(단위: 만)") + scale_fill_discrete(name='여, 결제건수')

View(w_total_num)

wom_total <- rbind(b_w_total,a_w_total)
View(man_total)


## 코로나 전후 비교

#코로나 전후 전체매출

ggplot(total_payment) +geom_col()
barplot(total_payment)
lines(total_payment)


#총 건수 차이
total_num <- a_num-b_num

#업종별 차이
type_diff <- table(after$업종명)-table(before$업종명)

### 계절별로 분석

#계절별 최대 매출액
winter_pay <- data %>% filter(기준년월%in% c("2020-02-01","2020-12-01")) %>% group_by(업종명) %>% summarise(결제금액=sum(결제금액)) %>% arrange(desc(결제금액)) %>% head(5)
spring_pay <- data %>% filter(기준년월 %in% c("2020-03-01","2020-04-01","2020-05-01")) %>% group_by(업종명) %>%
  summarise(결제금액=sum(결제금액)) %>% arrange(desc(결제금액)) %>% head(5)
summer_pay <- data %>% filter(기준년월 %in% c("2020-06-01","2020-07-01","2020-08-01"))%>% group_by(업종명) %>%
  summarise(결제금액=sum(결제금액)) %>% arrange(desc(결제금액)) %>% head(5)
fall_pay <- data %>% filter(기준년월 %in% c("2020-09-01","2020-10-01","2020-11-01")) %>% group_by(업종명) %>%
  summarise(결제금액=sum(결제금액)) %>% arrange(desc(결제금액)) %>% head(5)

# 결제 건수
winter <- data %>% filter(기준년월%in% c("2020-02-01","2020-12-01")) %>% group_by(업종명) %>% summarise(total=sum(결제건수)) %>% arrange(desc(total)) %>% head(5)
spring <- data %>% filter(기준년월 %in% c("2020-03-01","2020-04-01","2020-05-01")) %>% group_by(업종명) %>%
  summarise(total=sum(결제건수)) %>% arrange(desc(total)) %>% head(5)
summer <- data %>% filter(기준년월 %in% c("2020-06-01","2020-07-01","2020-08-01"))%>% group_by(업종명) %>%
  summarise(total=sum(결제건수)) %>% arrange(desc(total)) %>% head(5)
fall <- data %>% filter(기준년월 %in% c("2020-09-01","2020-10-01","2020-11-01")) %>% group_by(업종명) %>%
  summarise(total=sum(결제건수)) %>% arrange(desc(total)) %>% head(5)

View(winter_pay)
View(winter_num)
winter

winter[,"season"]="겨울"
winter_pay[,"season"]="겨울"
spring_pay[,"season"]="봄"
spring[,"season"]="봄"
summer_pay[,"season"]="여름"
summer[,"season"]="여름"
fall_pay[,"season"]="가을"
fall[,"season"]="가을"

fall_pay
winter
spring
summer
fall
season <- rbind(winter,spring,summer,fall)
season_pay <- rbind(winter_pay,spring_pay,summer_pay,fall_pay)
season_pay
season.index
##계절별 업종 소비유형 분석
ggplot(season, aes(reorder(season,total),total/100000, fill=업종명 %>% fct_rev())) +geom_col(position ="dodge") +
  xlab("계절") + ylab("결제건수(단위: 십만") + scale_fill_discrete(name='계절')

ggplot(season_pay, aes(reorder(season,결제금액), 결제금액/100000000, fill=업종명 %>% fct_rev())) +geom_col(position ="dodge") +
  xlab("계절") + ylab("1건당 결제금액(단위: 억)") + scale_fill_discrete(name='계절')


# 시군구에 따른 비교
b_si_pay <- before %>% group_by(시군구명) %>% summarise(결제금액=sum(결제금액)) %>% arrange(desc(결제금액))

a_si_pay <- after %>% group_by(시군구명) %>% summarise(결제금액=sum(결제금액)) %>% arrange(desc(결제금액))

yong_pay_b <- before %>% filter(시군구명=="영통구") %>% select(업종명,결제금액) %>%summarise(결제금액=sum(결제금액)) %>% arrange(desc(결제금액))

yong_pay_a <- after %>% filter(시군구명=="영통구") %>% summarise(업종명=업종명,결제금액=sum(결제금액)) %>% arrange(desc(결제금액))

View(yong_pay_b)

View(b_si_pay)

ggplot(b_si_pay, aes(시군구명, 결제금액/100000000,fill=시군구명)) +geom_col() +
  xlab("지역") + ylab("1건당 결제금액(단위: 억)") +ylim(c(0,600))

ggplot(a_si_pay, aes(시군구명, 결제금액/100000000,fill=시군구명)) +geom_col() +
  xlab("지역") + ylab("1건당 결제금액(단위: 억)")

ggplot(season_pay, aes(reorder(season,결제금액), 결제금액/100000000, fill=업종명 %>% fct_rev())) +geom_col(position ="dodge") +
  xlab("계절") + ylab("1건당 결제금액(단위: 억)") + scale_fill_discrete(name='계절')

#겨울
table(winter$기준년월)
table(winter$시군구명)
table(winter$성별)
table(winter$연령대)
table(winter$업종명)

#봄
table(spring$기준년월)
table(spring$시군구명)
table(spring$성별)
table(spring$연령대)
table(spring$업종명)

#여름
table(summer$기준년월)
table(summer$시군구명)
table(summer$성별)
table(summer$연령대)
table(summer$업종명)

#가을
table(fall$기준년월)
table(fall$시군구명)
table(fall$성별)
table(fall$연령대)
table(fall$업종명)



# 월별

# 거리두기 단계에 따른 분석

#### 회귀분석

# 전체 확진자수

# 경기도 확진자 수

# 수원시 확진자수

#기본 5가지 변화사항, 그래프


#업종별 결제건당 최대매출, 그래프







##### 가맹점 데이터 분석 #####

library(stringr)

gaman <- read.csv("data/지역화폐가맹점현황.csv")
View(gaman)
table(gaman$업종명.종목명[str_extract(gaman$업종명.종목명.,"음식")=="음식"])
table(gaman$업종명.종목명)
#데이터 전처리: 업종명 변경
gaman$업종명.종목명. <- recode (gaman$업종명.종목명., "일반휴게음식서양음식"="일반휴게음식-서양음식")
gaman$업종명.종목명. <- recode (gaman$업종명.종목명., "일반휴게음식 - 서양음식"="일반휴게음식-서양음식")

gaman$업종명.종목명. <- recode (gaman$업종명.종목명., "일반휴게음식 - 일반한식"="일반휴게음식-일반한식")
gaman$업종명.종목명. <- recode (gaman$업종명.종목명., "일반휴게음식 / 일반한식"="일반휴게음식-일반한식")
gaman$업종명.종목명. <- recode (gaman$업종명.종목명., "일반휴게음식일반한식"="일반휴게음식-일반한식")

gaman$업종명.종목명. <- recode (gaman$업종명.종목명., "일반휴게음식한정식"="일반휴게음식-한정식")

gaman$업종명.종목명. <- recode (gaman$업종명.종목명., "일반휴게음식-일식?회집"="일반휴게음식-일식회집")
gaman$업종명.종목명. <- recode (gaman$업종명.종목명., "일반휴게음식일식?회집"="일반휴게음식-일식회집")
gaman$업종명.종목명. <- recode (gaman$업종명.종목명., "일반휴게음식 - 일식?회집"="일반휴게음식-일식회집")

gaman$업종명.종목명. <- recode (gaman$업종명.종목명., "일반휴게음식중국식"="일반휴게음식-중국식")

gaman$업종명.종목명. <- recode (gaman$업종명.종목명., "일반휴게음식위탁급식업"="일반휴게음식-위탁급식업")

gaman$업종명.종목명. <- recode (gaman$업종명.종목명., "일반휴게음식 - 스넥"="일반휴게음식-스넥")
gaman$업종명.종목명. <- recode (gaman$업종명.종목명., "일반휴게음식스넥"="일반휴게음식-스넥")

gaman$업종명.종목명. <- recode (gaman$업종명.종목명., "일반휴게음식주점"="일반휴게음식-주점")
gaman$업종명.종목명. <- recode (gaman$업종명.종목명., "일반휴게음식 - 주점"="일반휴게음식-주점")
View(gaman1)
gaman1 <- gaman
#이상치 제거
gaman1 <- subset(gaman1, str_extract(gaman1$소재지도로명주소,"수원")=="수원")




###

gaman_data_food <- gaman1 %>% select(상호명,업종명.종목명.,소재지도로명주소) %>% filter(str_extract(gaman1$업종명.종목명.,"음식")=="음식")











###
#
View(gaman1)
gaman_data <- gaman1 %>% select(상호명,업종명.종목명.,소재지도로명주소)
View(gaman_data_food)

View(gaman_data_y)
gaman_data_y <- gaman_data %>% filter(str_extract(gaman_data$소재지도로명주소,"영통동")=="영통동")
gaman_data_m <- gaman_data %>% filter(str_extract(gaman_data$소재지도로명주소,"매탄")=="매탄")
gaman_data_i <- gaman_data %>% filter(str_extract(gaman_data$소재지도로명주소,"인계동")=="인계동")
str(ingae)

str(gg)
str(gaman_data_y) #1020/3687
str(gaman_data_m) #1039
str(gaman_data_i) #1174/3898 
1020/3687
1039/3627 
1174/3898
gaman_data
View(gaman_data_food)
gg <- gaman_data_food %>% filter(str_extract(gaman_data_food$소재지도로명주소,"광교")=="광교")
View(gg)
youngtong <- gaman_data_food %>% filter(str_extract(gaman_data_food$소재지도로명주소,"영통")=="영통")
View(youngtong)
youngtong %>% filter(str_extract(gaman_data_food$소재지도로명주소,"영통동")=="영통동")

metan <- gaman_data_food %>% filter(str_extract(gaman_data_food$소재지도로명주소,"매탄")=="매탄")
View(metan)

View(metan %>% filter(str_extract(metan$상호명,"카페")=="카페"))

ingae <- gaman_data_food %>% filter(str_extract(gaman_data_food$소재지도로명주소,"인계동")=="인계동")
View(ingae)

###잡다한거
str(gaman_data_food %>% filter(str_extract(gaman_data_food$소재지도로명주소,"매산")=="매산"))#575
str(gaman_data_food %>% filter(str_extract(gaman_data_food$소재지도로명주소,"평동")=="평동"))#38
str(gaman_data_food %>% filter(str_extract(gaman_data_food$소재지도로명주소,"서둔")=="서둔"))#124


ggplot(data= youngtong, aes(업종명.종목명.)) +geom_bar() + ggtitle("영통동 음식점 현황") +theme(plot.title = element_text(face = "bold", size = 30, color = "darkblue"), axis.title = element_text(face = "bold", size = 20, color = "darkblue"))



ggplot(data= metan, aes(업종명.종목명.)) +geom_bar() + ggtitle("매탄동 음식점 현황")+theme(plot.title = element_text(face = "bold", size = 30, color = "darkblue"), axis.title = element_text(face = "bold", size = 20, color = "darkblue"))

ggplot(data= ingae, aes(업종명.종목명.)) +geom_bar() + ggtitle("인계동 음식점 현황")+theme(plot.title = element_text(face = "bold", size = 30, color = "darkblue"), axis.title = element_text(face = "bold", size = 20, color = "darkblue"))


str(ingoo)


#### 연령별 인구현황
ingoo <- read.csv("data/202012_연령별인구현황_연간.csv")
View(ingoo)

str(ingoo)
youngtonggoo <- ingoo[37,]

ingoo_y <-ingoo[c(43:45),]
ingoo_y <- summarise()
sum(ingoo_y$X2020년_거주자_20.29세)

ingoo_y <-ingoo_y %>%
  mutate_all(cumsum)
View(ingoo_y)
ingoo_y <- ingoo_y[3,]
ingoo_y$행정구역="영통동"

ingoo_m <-ingoo_m %>%
  mutate_all(cumsum)

ingoo_m <- ingoo_m[4,]
ingoo_m$행정구역="매탄동"
View(ingoo_m)

ingoo_i$행정구역 = "인계동"
View(ingoo_i)

View(ingoo_y)
ingoo_m <-ingoo[c(38:41),]
ingoo_i <-ingoo[35,]
ingoo_i
ingoo <- (rbind(ingoo_y,ingoo_m,ingoo_i))

View(ingoo_y)

View(ingoo_total)

ingoo_total <-  ingoo %>% filter(str_extract(ingoo$행정구역,"동")=="동") %>%
       group_by(행정구역) %>% summarise(전체인구=(X2020년_거주자_20.29세+X2020년_거주자_30.39세+X2020년_거주자_40.49세)) %>% arrange(desc(전체인구)) %>% head(5)

ingoo_20 <-  ingoo %>% filter(str_extract(ingoo$행정구역,"동")=="동") %>% select(행정구역,X2020년_거주자_20.29세) %>%
  group_by(행정구역) %>% arrange(desc(X2020년_거주자_20.29세)) %>% head(5)

ingoo_30 <- ingoo %>% filter(str_extract(ingoo$행정구역,"동")=="동") %>% select(행정구역,X2020년_거주자_30.39세) %>%
  group_by(행정구역) %>% arrange(desc(X2020년_거주자_30.39세)) %>% head(5)

ingoo_40 <- ingoo %>% filter(str_extract(ingoo$행정구역,"동")=="동") %>% select(행정구역,X2020년_거주자_40.49세) %>%
  group_by(행정구역) %>% arrange(desc(X2020년_거주자_40.49세)) %>% head(5)

ingoo_man <-  ingoo %>% filter(str_extract(ingoo$행정구역,"동")=="동") %>%
  group_by(행정구역) %>% summarise(남전체인구=(X2020년_남_거주자_20.29세+X2020년_남_거주자_30.39세+X2020년_남_거주자_40.49세)) %>% arrange(desc(남전체인구)) %>% head(5)

ingoo_woman <-  ingoo %>% filter(str_extract(ingoo$행정구역,"동")=="동") %>%
  group_by(행정구역) %>% summarise(여전체인구=(X2020년_여_거주자_20.29세+X2020년_여_거주자_30.39세+X2020년_여_거주자_40.49세)) %>% arrange(desc(여전체인구)) %>% head(5)

View(ingoo_20)

dimnames(ingoo_total)=list(row=c("율천동","원천동","광교1동","호매실동","권선2동"),col=names("인구수"))

View(ingoo_man)
sort(ingoo$X2020년_거주자_20.29세)
str(ingoo)
int(ingoo)
as.numeric(ingoo)

size_my_data_frame = dim(ingoo);

for(i in 2:size_my_data_frame[2]){
  
  imsi = as.numeric(ingoo[,i]);   
  
  imsi[is.na(imsi)] <- 0     #해당 열에서 NA 값이 있을 경우 0으로 변환
  
  ingoo[,i] <- imsi;
  
}

ggplot(ingoo_total, aes(reorder(행정구역,전체인구), 전체인구, fill=행정구역  %>% fct_rev())) +geom_col() +
  xlab("동별") + ylab("인구수") + scale_fill_discrete(name='동') +ggtitle("동별 총인구수")+theme(plot.title = element_text(face = "bold", size = 30, color = "darkblue"))

ggplot(ingoo_20, aes(reorder(행정구역,X2020년_거주자_20.29세), X2020년_거주자_20.29세, fill=행정구역  %>% fct_rev())) +geom_col() +
  xlab("동별") + ylab("인구수") + scale_fill_discrete(name='동') +ggtitle("동별 20대 인구수")+theme(plot.title = element_text(face = "bold", size = 30, color = "darkblue"))


ggplot(ingoo_30, aes(reorder(행정구역,X2020년_거주자_30.39세), X2020년_거주자_30.39세, fill=행정구역  %>% fct_rev())) +geom_col() +
  xlab("동별") + ylab("인구수") + scale_fill_discrete(name='동') +ggtitle("동별 30대 인구수")+theme(plot.title = element_text(face = "bold", size = 30, color = "darkblue"))


ggplot(ingoo_40, aes(reorder(행정구역,X2020년_거주자_40.49세), X2020년_거주자_40.49세, fill=행정구역  %>% fct_rev())) +geom_col() +
  xlab("동별") + ylab("인구수") + scale_fill_discrete(name='동') +ggtitle("동별 40대 인구수")+theme(plot.title = element_text(face = "bold", size = 30, color = "darkblue"))


ggplot(ingoo_man, aes(reorder(행정구역,남전체인구), 남전체인구, fill=행정구역  %>% fct_rev())) +geom_col() +
  xlab("동별") + ylab("인구수") + scale_fill_discrete(name='동') +ggtitle("동별 남자 인구수")+theme(plot.title = element_text(face = "bold", size = 30, color = "darkblue"))


ggplot(ingoo_woman, aes(reorder(행정구역,여전체인구), 여전체인구, fill=행정구역  %>% fct_rev())) +geom_col() +
  xlab("동별") + ylab("인구수") + scale_fill_discrete(name='동') +ggtitle("동별 여자 인구수")+theme(plot.title = element_text(face = "bold", size = 30, color = "darkblue"))




yoodong <- read_excel(path="data/역별 유동인구 현황 2020년12월말기준.xls")
View(yoodong)
total_s <- yoodong %>% select(구분,여객_승차인원) %>% arrange(desc(여객_승차인원)) %>% head(5)
total_g <-yoodong %>% select(구분,여객_강차인원) %>% arrange(desc(여객_강차인원)) %>% head(5)

yoodong2 <- yoodong[c(5,6,7,8,9),]

big3_s <- yoodong2 %>% select(구분,여객_승차인원) %>% arrange(desc(여객_승차인원)) %>% head(5)
big3_g <- yoodong2 %>% select(구분,여객_강차인원) %>% arrange(desc(여객_강차인원)) %>% head(5)

ggplot(total_s, aes(reorder(구분,여객_승차인원), 여객_승차인원, fill=구분  %>% fct_rev())) +geom_col() +
  xlab("승차역") + ylab("인원") + scale_fill_discrete(name='역') +ggtitle("전체 승차인원")+theme(plot.title = element_text(face = "bold", size = 30, color = "darkblue"))

ggplot(total_g, aes(reorder(구분,여객_강차인원), 여객_강차인원, fill=구분  %>% fct_rev())) +geom_col() +
  xlab("강차역") + ylab("인원") + scale_fill_discrete(name='역') +ggtitle("전체 강차인원")+theme(plot.title = element_text(face = "bold", size = 30, color = "darkblue"))

ggplot(big3_s, aes(reorder(구분,여객_승차인원), 여객_승차인원, fill=구분  %>% fct_rev())) +geom_col() +
  xlab("승차역") + ylab("인원") + scale_fill_discrete(name='역') +ggtitle("주요 3지역 승차인원")+theme(plot.title = element_text(face = "bold", size = 30, color = "darkblue"))

ggplot(big3_g, aes(reorder(구분,여객_강차인원), 여객_강차인원, fill=구분  %>% fct_rev())) +geom_col() +
  xlab("강차역") + ylab("인원") + scale_fill_discrete(name='역') +ggtitle("주요 3지역 승차인원")+theme(plot.title = element_text(face = "bold", size = 30, color = "darkblue"))

### 토지 지목별 현황
toji <- read.csv("data/토지지목별현황_20210527235436.csv")
View(toji)
names(toji) <- as.character(unlist(toji[1,]))
toji <- toji[2:61,]
toji %>% arrange(desc(주차장))
str(toji)

size_my_data_frame = dim(toji);
size_my_data_frame
for(i in 3:size_my_data_frame[2]){
  
  imsi = as.numeric(toji[,i]);   
  
  imsi[is.na(imsi)] <- 0     #해당 열에서 NA 값이 있을 경우 0으로 변환
  
  toji[,i] <- imsi;
  
}
toji$주차장[1]/toji$대지[1]
for(i in 1:60){
  toji[i,"주차장/대지"]=toji$주차장[i]/toji$대지[i]
  
}

for(i in 1:60){
ifelse(toji[i,"체육용지"]>0,toji[i,"체육용지"]==1,toji[i,"체육용지"]==0)
}

toji


str(toji)


toji_i <- toji %>% select(`행정구역(구동)별(2)`,대지,주차장,체육용지,`주차장/대지`) %>%filter(str_extract(`행정구역(구동)별(2)`,"인계동")=="인계동")
toji_y <- toji %>% select(`행정구역(구동)별(2)`,대지,주차장,체육용지,`주차장/대지`) %>%filter(str_extract(`행정구역(구동)별(2)`,"영통동")=="영통동")
toji_m <- toji %>% select(`행정구역(구동)별(2)`,대지,주차장,체육용지,`주차장/대지`) %>%filter(str_extract(`행정구역(구동)별(2)`,"매탄동")=="매탄동")

toji_3 <- rbind(toji_y,toji_m,toji_i)

toji_3

colnames(toji_3)[1] <- "행정구역"
ggplot(toji_3, aes(reorder(행정구역,주차장/대지), 주차장/대지, fill=행정구역  %>% fct_rev())) +geom_col() +
  xlab("동이름") + ylab("비율") + scale_fill_discrete(name='동') +ggtitle("주요 3지역 주차장/대지비율")+theme(plot.title = element_text(face = "bold", size = 30, color = "darkblue"))

ggplot(toji_3, aes(reorder(행정구역,주차장), 주차장, fill=행정구역  %>% fct_rev())) +geom_col() +
  xlab("동이름") + ylab("주차장크기") + scale_fill_discrete(name='동') +ggtitle("주요 3지역 주차장크기")+theme(plot.title = element_text(face = "bold", size = 30, color = "darkblue"))

ggplot(toji_3, aes(reorder(행정구역,체육용지), 주차장, fill=행정구역  %>% fct_rev())) +geom_col() +
  xlab("동이름") + ylab("주차장크기") + scale_fill_discrete(name='동') +ggtitle("주요 3지역 주차장크기")+theme(plot.title = element_text(face = "bold", size = 30, color = "darkblue"))

ggplot(toji_3, aes(reorder(행정구역,대지), 대지, fill=행정구역  %>% fct_rev())) +geom_col() +
  xlab("동이름") + ylab("대지면적") + scale_fill_discrete(name='동') +ggtitle("주요 3지역 대지면적")+theme(plot.title = element_text(face = "bold", size = 30, color = "darkblue"))

