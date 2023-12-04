# 기본 package 설정
library(tidyverse)
library(tidymodels)
library(rstatix)
library(skimr)
library(FSA)
library(ggpubr)

library(rpart)
library(rpart.plot)
library(caret)
library(tree)
# install.packages("")

# 데이터 불러오기
csi_tb <-read_csv("강서01 버스 이용 만족도 조사(응답).csv",
                  col_names = T,
                  locale = locale("ko", encoding = "euc-kr"),
                  na = ".") %>%
    mutate(성별 = factor(성별,
                       levels = c(1:2),
                       labels = c("남자", "여자")),
           학과 = factor(학과,
                         levels = c(1:8),
                         labels = c("신학", "사회복지", "G2빅데이터경영",
                                    "상담심리", "간호", "식품영양", "실용음악", "기타")),
           학년 = factor(학년,
                         levels = c(1:4),
                         labels = c("1학년", "2학년", "3학년", "4학년")),
           통학시간 = factor(C1,
                         levels = c(1:4),
                         labels = c("30분 이하", "30분 ~ 1시간",
                                    "1시간 ~ 1시간 30분", "2시간 이상")),
           통학횟수 = factor(C2,
                         levels = c(1:5),
                         labels = c("1일", "2일", "3일", "4일", "5일 이상")),
           이용횟수 = factor(D1,
                         levels = c(1:3),
                         labels = c("1회 ~ 2회", "3회 ~ 4회", "5회 이상")),
           이용시간대 = factor(D2,
                        levels = c(1:5),
                        labels = c("오전 9시 전", "오전 10시 ~ 12시",
                                   "오후 13시 ~ 15시", "오후 16시 ~ 18시", "오후 18시 후")),
           이용이유 = factor(D3,
                         levels = c(1:4),
                         labels = c("강서대학교, 지하철 역 근처에 하차할 수 있어서",
                                    "통학에 가장 적절한 교통수단이어서",
                                    "통학하는데 다른 마땅한 교통 수단이 없어서",
                                    "기타")
                         )
           )
str(csi_tb)
csi_tb

summary(csi_tb)


# 100점 만점으로 변환
csi_100_tb <- csi_tb %>%
rowwise() %>%
transmute(성별, 학과, 학년, 통학시간, 통학횟수, 이용횟수, 이용시간대, 이용이유,
          이용환경 = (mean(c(A1, A2, A3))-1)/4*100,
          배차간격 = (mean(c(A1))-1)/4*100,
          쾌적성 = (mean(c(A2))-1)/4*100,
          편리성 = (mean(c(A3))-1)/4*100,
          제공서비스 = (mean(c(B1, B2, B3, B4, B5))-1)/4*100,
          내부청결성 = (mean(c(B1))-1)/4*100,
          외부청결성 = (mean(c(B2))-1)/4*100,
          기사친절성 = (mean(c(B3))-1)/4*100,
          정차신뢰성 = (mean(c(B4))-1)/4*100,
          안정성 = (mean(c(B5))-1)/4*100,
          종합만족도 = (mean(c(A1, A2, A3, B1, B2, B3, B4, B5))-1)/4*100) %>%
  as_tibble()
  
csi_100_tb
str(csi_100_tb)


# 데이터 확인
Summarize(이용환경 ~ 성별, csi_100_tb)
Summarize(이용환경 ~ 학과, csi_100_tb)
Summarize(이용환경 ~ 학년, csi_100_tb)

Summarize(제공서비스 ~ 성별, csi_100_tb)
Summarize(제공서비스 ~ 학과, csi_100_tb)
Summarize(제공서비스 ~ 학년, csi_100_tb)

Summarize(이용환경 ~ 통학횟수+이용횟수, csi_100_tb)
Summarize(제공서비스 ~ 통학횟수+이용횟수, csi_100_tb)
Summarize(종합만족도 ~ 통학횟수+이용횟수, csi_100_tb)


# 응답자 특성
# 성별
csi_100_tb %>%
  freq_table(성별) %>%
  write_excel_csv("응답자_성별.xls", col_names = T)

# 학과
csi_100_tb %>%
  freq_table(학과) %>%
  write_excel_csv("응답자_학부.xls", col_names = T)

# 학년
csi_100_tb %>%
  freq_table(학년) %>%
  write_excel_csv("응답자_학년.xls", col_names = T)

# 통학시간+통학횟수
csi_100_tb %>%
  freq_table(통학시간, 통학횟수) %>%
  write_excel_csv("응답자_통학시간_통학횟수.xls", col_names = T)

# 이용횟수+이용시간대
csi_100_tb %>%
  freq_table(이용횟수, 이용시간대) %>%
  write_excel_csv("응답자_이용횟수_이용시간대.xls", col_names = T)

# 통학횟수+이용횟수
csi_100_tb %>%
  freq_table(통학횟수, 이용횟수) %>%
  write_excel_csv("응답자_통학횟수_이용횟수.xls", col_names = T)


ggbarplot(csi_100_tb,
          x = "학과",
          y = "종합만족도",
          add=c("mean_ci"),
          fill = "학과",
          legend = "right")
