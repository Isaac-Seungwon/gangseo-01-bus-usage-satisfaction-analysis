# 1. 기본 package 설정
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


# 2. 데이터 불러오기
csi_tb <-read_csv("강서01 버스 이용 만족도 조사.csv",
                  col_names = T,
                  locale = locale("ko", encoding = "euc-kr"),
                  na = ".") %>%
    mutate(학생 = factor(학생,
                         levels = c(1),
                         labels = c("강서대학생")),
           성별 = factor(성별,
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


# 3. 만족도를 100점 만점으로 변환
csi_100_tb <- csi_tb %>%
rowwise() %>%
transmute(학생, 성별, 학과, 학년, 통학시간, 통학횟수, 이용횟수, 이용시간대, 이용이유,
          이용환경 = (mean(c(A1, A2, A3))-1)/4*100,
          제공서비스 = (mean(c(B1, B2, B3, B4, B5))-1)/4*100,
          # 이용환경
          배차간격 = (mean(c(A1))-1)/4*100,
          쾌적성 = (mean(c(A2))-1)/4*100,
          편리성 = (mean(c(A3))-1)/4*100,
          # 제공서비스
          내부청결성 = (mean(c(B1))-1)/4*100,
          외부청결성 = (mean(c(B2))-1)/4*100,
          기사님친절성 = (mean(c(B3))-1)/4*100,
          정차선신뢰성 = (mean(c(B4))-1)/4*100,
          안정성 = (mean(c(B5))-1)/4*100) %>%
  as_tibble()
  
csi_100_tb
str(csi_100_tb)

# 종합만족도 계산
csi_total_tb1 <- csi_100_tb %>%
  select(배차간격:안정성) %>%
  rowwise() %>%
  mutate(종합만족도 = mean(c(배차간격, 쾌적성, 편리성,
                            내부청결성, 외부청결성, 기사님친절성, 정차선신뢰성, 안정성))) %>%
  as_tibble()
str(csi_total_tb)

# 종합만족도 데이터 추가
csi_100_tb[,20] <- csi_total_tb1[,9]
str(csi_100_tb1)


# 세부만족도 계산
csi_total_tb2 <- csi_100_tb %>%
  select(이용환경:제공서비스) %>%
  rowwise() %>%
  mutate(세부만족도 = mean(c(이용환경, 제공서비스))) %>%
  as_tibble()
str(csi_total_tb)

# 세부만족도 데이터 추가
csi_100_tb[,21] <- csi_total_tb2[,3]
str(csi_100_tb2)





# 4. 만족도 변환 결과 출력
# 만족도 ~ 전체 학생
df <- Summarize(이용환경 ~ 학생, csi_100_tb)
df[2,] <- Summarize(제공서비스 ~ 학생, csi_100_tb)
df[3,] <- Summarize(종합만족도 ~ 학생, csi_100_tb)
df[4,] <- Summarize(배차간격 ~ 학생, csi_100_tb)
df[5,] <- Summarize(쾌적성 ~ 학생, csi_100_tb)
df[6,] <- Summarize(편리성 ~ 학생, csi_100_tb)
df[7,] <- Summarize(내부청결성 ~ 학생, csi_100_tb)
df[8,] <- Summarize(외부청결성 ~ 학생, csi_100_tb)
df[9,] <- Summarize(기사님친절성 ~ 학생, csi_100_tb)
df[10,] <- Summarize(정차선신뢰성 ~ 학생, csi_100_tb)
df[11,] <- Summarize(안정성 ~ 학생, csi_100_tb)
write_excel_csv(df, file="1-1. 종합 강서01 버스 이용 만족도.csv", col_names = T)

# 만족도 ~ 성별
df <- Summarize(이용환경 ~ 성별, csi_100_tb)
df[3:4,] <- Summarize(제공서비스 ~ 성별, csi_100_tb)
df[5:6,] <- Summarize(종합만족도 ~ 성별, csi_100_tb)
df[7:8,] <- Summarize(배차간격 ~ 성별, csi_100_tb)
df[9:10,] <- Summarize(쾌적성 ~ 성별, csi_100_tb)
df[11:12,] <- Summarize(편리성 ~ 성별, csi_100_tb)
df[13:14,] <- Summarize(내부청결성 ~ 성별, csi_100_tb)
df[15:16,] <- Summarize(외부청결성 ~ 성별, csi_100_tb)
df[17:18,] <- Summarize(기사님친절성 ~ 성별, csi_100_tb)
df[19:20,] <- Summarize(정차선신뢰성 ~ 성별, csi_100_tb)
df[21:22,] <- Summarize(안정성 ~ 성별, csi_100_tb)
write_excel_csv(df, file="1-2. 성별별 강서01 버스 이용 만족도.csv", col_names = T)

# 만족도 ~ 학과
df <- Summarize(이용환경 ~ 학과, csi_100_tb)
df[8:14,] <- Summarize(제공서비스 ~ 학과, csi_100_tb)
df[15:21,] <- Summarize(종합만족도 ~ 학과, csi_100_tb)
df[22:28,] <- Summarize(배차간격 ~ 학과, csi_100_tb)
df[29:35,] <- Summarize(쾌적성 ~ 학과, csi_100_tb)
df[36:42,] <- Summarize(편리성 ~ 학과, csi_100_tb)
df[43:49,] <- Summarize(내부청결성 ~ 학과, csi_100_tb)
df[50:56,] <- Summarize(외부청결성 ~ 학과, csi_100_tb)
df[57:63,] <- Summarize(기사님친절성 ~ 학과, csi_100_tb)
df[64:70,] <- Summarize(정차선신뢰성 ~ 학과, csi_100_tb)
df[71:77,] <- Summarize(안정성 ~ 학과, csi_100_tb)
write_excel_csv(df, file="1-3. 학과별 강서01 버스 이용 만족도.csv", col_names = T)

# 만족도 ~ 학년
df <- Summarize(이용환경 ~ 학년, csi_100_tb)
df[5:8,] <- Summarize(제공서비스 ~ 학년, csi_100_tb)
df[9:12,] <- Summarize(종합만족도 ~ 학년, csi_100_tb)
df[13:16,] <- Summarize(배차간격 ~ 학년, csi_100_tb)
df[17:20,] <- Summarize(쾌적성 ~ 학년, csi_100_tb)
df[21:24,] <- Summarize(편리성 ~ 학년, csi_100_tb)
df[25:28,] <- Summarize(내부청결성 ~ 학년, csi_100_tb)
df[29:32,] <- Summarize(외부청결성 ~ 학년, csi_100_tb)
df[33:36,] <- Summarize(기사님친절성 ~ 학년, csi_100_tb)
df[37:40,] <- Summarize(정차선신뢰성 ~ 학년, csi_100_tb)
df[41:44,] <- Summarize(안정성 ~ 학년, csi_100_tb)
write_excel_csv(df, file="1-4. 학년별 강서01 버스 이용 만족도.csv", col_names = T)

# 만족도 ~ 교차
Summarize(이용환경 ~ 통학횟수+이용횟수, csi_100_tb)
Summarize(제공서비스 ~ 통학횟수+이용횟수, csi_100_tb)
Summarize(종합만족도 ~ 통학횟수+이용횟수, csi_100_tb)


# 5. 응답자 특성 출력
# 성별
csi_100_tb %>%
  freq_table(성별) %>%
  write_excel_csv("2-1. 응답자_성별.csv", col_names = T)

# 학과
csi_100_tb %>%
  freq_table(학과) %>%
  write_excel_csv("2-2. 응답자_학부.csv", col_names = T)

# 학년
csi_100_tb %>%
  freq_table(학년) %>%
  write_excel_csv("2-3. 응답자_학년.csv", col_names = T)

# 통학시간+통학횟수
csi_100_tb %>%
  freq_table(통학시간, 통학횟수) %>%
  write_excel_csv("2-4. 응답자_통학시간_통학횟수.csv", col_names = T)

# 이용횟수+이용시간대
csi_100_tb %>%
  freq_table(이용횟수, 이용시간대) %>%
  write_excel_csv("2-5. 응답자_이용횟수_이용시간대.csv", col_names = T)

# 통학횟수+이용횟수
csi_100_tb %>%
  freq_table(통학횟수, 이용횟수) %>%
  write_excel_csv("2-6. 응답자_통학횟수_이용횟수.csv", col_names = T)


# 6. 만족도 그래프
ggbarplot(csi_100_tb,
          x = "학과",
          y = "이용환경",
          add = c("mean_sd"),
          fill = "학과",
          ylim = c(0,100),
          legend = "right")

ggbarplot(csi_100_tb,
          x = "학과",
          y = "제공서비스",
          add = c("mean_sd"),
          fill = "학과",
          ylim = c(0,100),
          legend = "right")

ggbarplot(csi_100_tb,
          x = "학과",
          y = "종합만족도",
          add = c("mean_sd"),
          fill = "학과",
          ylim = c(0,100),
          legend = "right")


# 7. IPA 중요도 계산
# 세부만족도
ipa_1 <- csi_total_tb2 %>% 
  get_summary_stats(이용환경, 제공서비스,
                        show = c("mean")) %>%
  select(variable, mean) %>%
  filter(!(variable == "세부만족도")) %>%
  rename(항목 = variable, 만족도 = mean)

ipa_2 <- csi_total_tb2 %>%
  cor_test(var1 = 세부만족도,
           method = "pearson") %>%
  select(var2, cor) %>%
  mutate(wgt = cor/sum(cor)) %>%
  select(wgt) %>%
  rename(중요도 = wgt)

ipa_tb1 <- cbind(ipa_1, ipa_2)
ipa_tb1

# 종합만족도
ipa_3 <- csi_total_tb1 %>% 
  get_summary_stats(배차간격, 쾌적성, 편리성,
                    내부청결성, 외부청결성, 기사님친절성, 정차선신뢰성, 안정성,
                    show = c("mean")) %>%
  select(variable, mean) %>%
  filter(!(variable == "종합만족도")) %>%
  rename(항목 = variable, 만족도 = mean)

# 상관계수 이용
ipa_4 <- csi_total_tb1 %>%
  cor_test(var1 = 종합만족도,
           method = "pearson") %>%
  select(var2, cor) %>%
  mutate(wgt = cor/sum(cor)) %>%
  select(wgt) %>%
  rename(중요도 = wgt)

ipa_tb2 <- cbind(ipa_3, ipa_4)
ipa_tb2

# ipa_tb <- rbind(ipa_tb1, ipa_tb2)
ipa_tb1 %>%
  write_excel_csv("3-1. IPA 분석 종합만족도.csv", col_names = T)
ipa_tb2 %>%
  write_excel_csv("3-2. IPA 분석 세부만족도.csv", col_names = T)

# 8. IPA 그래프
ipa_tb1 %>%
  ggplot(mapping = aes(x = 중요도,
                       y = 만족도)) +
  geom_point(shape = 20, 
             size = 4,
             colour = "blue",
             show.legend = FALSE) +
  coord_cartesian(xlim = c(0.5 - 0.5, 0.5 + 0.5), # 중요도 평균 0.5
                  ylim = c(45.1875 - 45, 45.1875 + 45)) + #만족도 평균 45.1875
  geom_text(mapping = aes(label = 항목, 
                          size = 0, 
                          vjust = 0, 
                          hjust = -0.12),
            show.legend = FALSE) +
  geom_vline(xintercept = 0.5,
             size = 0.5,
             alpha = 0.2) +
  geom_hline(yintercept = 45.1875,
             size = 0.5,
             alpha = 0.2)

ipa_tb2 %>%
  ggplot(mapping = aes(x = 중요도,
                       y = 만족도)) +
  geom_point(shape = 20, 
             size = 4,
             colour = "red",
             show.legend = FALSE) + 
  coord_cartesian(xlim = c(0.125 - 0.05, 0.125 + 0.05), # 중요도 평균 0.125
                  ylim = c(48.11375 - 48, 48.11375 + 48)) + # 만족도 평균 48.11375
  geom_text(mapping = aes(label = 항목, 
                          size = 0,
                          vjust = 0, 
                          hjust = -0.12),
            show.legend = FALSE) +
  geom_vline(xintercept = 0.125,
             size = 0.5,
             alpha = 0.2) +
  geom_hline(yintercept = 48.11375,
             size = 0.5,
             alpha = 0.2)