library(dplyr)
library(agricolae)
library(pastecs)
library(tidyr)
library(tidyLPA)
library(ggplot2)
library(lsr)
library(nnet)
library(reshape2)
library(vcd)
library(flexmix)
library(mvtnorm)
library(survey)
library(effectsize)
library(kableExtra)
library(multcomp)
library(broom)
library(lavaan)
library(tidymodels)
library(glmnet)
library(xgboost)
library(nnet)
library(lme4)
library(emmeans)
library(tibble)
library(purrr)
library(pbkrtest)
library(lmerTest)


setwd("D:/Research/Couple_LPA/")
raw_data=read.csv("couple_raw_data_n300_20260219.csv",header=T,stringsAsFactor=F)

selected_meta = raw_data[, c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)]
selected_score = raw_data[, c(91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109)]

selected_score_zNorm = selected_score_zNorm <- selected_score %>% transmute(across(everything(), ~ scale(.)[,1], .names = "{.col}_zNorm"))

boxplot(
  list(
    Neuroticism     = selected_score$Neuroticism,
    Extraversion    = selected_score$Extraversion,
    Openness        = selected_score$Openness,
    Agreeableness   = selected_score$Agreeableness,
    Conscientiousness = selected_score$Conscientiousness
  ),
  main = "Big Five Personality Traits",   # 그래프 제목
  ylab = "Score",                          # y축 라벨
  col  = c("lightblue", "lightgreen", "lightpink", "lightyellow", "lavender"),
  ylim = c(0, 25)
)

boxplot(
  list(
    Neuroticism     = selected_score_zNorm$Neuroticism_zNorm,
    Extraversion    = selected_score_zNorm$Extraversion_zNorm,
    Openness        = selected_score_zNorm$Openness_zNorm,
    Agreeableness   = selected_score_zNorm$Agreeableness_zNorm,
    Conscientiousness = selected_score_zNorm$Conscientiousness_zNorm
  ),
  main = "Big Five Personality Traits",   # 그래프 제목
  ylab = "Score",                          # y축 라벨
  col = "Green"                        # 박스 색상
)

summary(selected_meta[,c(2)])
summary(selected_score[,])
stat.desc(selected_score[, ])
selected_meta %>% group_by(Gender) %>% summarise(n = n())
selected_meta %>% group_by(Age) %>% summarise(n = n())  
selected_meta %>% group_by(Age_2) %>% summarise(n = n())  
selected_meta %>% group_by(Marital) %>% summarise(n = n())
selected_meta %>% group_by(Parental) %>% summarise(n = n())
selected_meta %>% group_by(Education) %>% summarise(n = n())
selected_meta %>% group_by(Religion) %>% summarise(n = n())
selected_meta %>% group_by(Occupation) %>% summarise(n = n())
selected_meta %>% group_by(CoupleStatus) %>% summarise(n = n())
selected_meta %>% group_by(Income) %>% summarise(n = n())
selected_meta %>% group_by(CouplePeriod) %>% summarise(n = n())
selected_meta %>% group_by(NumCouple) %>% summarise(n = n())

############################################  correlation  ################################################

cor_matrix <- cor(selected_score_zNorm[,])
cormat <- round(cor(cor_matrix),2)
cor_melt <- melt(cormat, na.rm = TRUE)

ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + 
  scale_fill_gradient2(low = "#abd9e9", mid = "#FFFFFF", high = "#FF0000", midpoint = 0, space = "Lab", name="Correlation") +
  theme_classic()

lower_tri <- cormat
lower_tri[upper.tri(lower_tri)] <- NA #OR upper.tri function
cor_melt_lower <- melt(lower_tri, na.rm = TRUE)

cor_heatmap <- ggplot(cor_melt_lower, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + 
  scale_fill_gradient2(low = "#abd9e9", mid = "#FFFFFF", high = "#FF0000", midpoint = 0, space = "Lab", name="Correlation") +
  theme_classic() +
  theme(panel.grid.major = element_blank())
cor_heatmap

cor_heatmap +
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 4)

################################################  Merge & regroup ################################################################

selected_meta_regroup = selected_meta

selected_meta_regroup = selected_meta_regroup %>%  mutate(Marital = case_when(
  Marital == "1" ~ 1,
  Marital == "2" ~ 2,
  Marital == "3" ~ 1,
  Marital == "4" ~ 1
  ))

selected_meta_regroup = selected_meta_regroup %>%  mutate(Parental = case_when(
  Parental == "1" ~ 1,
  Parental == "2" ~ 2,
  Parental == "3" ~ 2,
  Parental == "4" ~ 2
))

selected_meta_regroup = selected_meta_regroup %>%  mutate(Education = case_when(
  Education == "2" ~ 1,
  Education == "3" ~ 1,
  Education == "4" ~ 2,
  Education == "5" ~ 3
))

selected_meta_regroup = selected_meta_regroup %>%  mutate(Religion = case_when(
  Religion == "1" ~ 1,
  Religion == "2" ~ 2,
  Religion == "3" ~ 2,
  Religion == "4" ~ 2,
  Religion == "5" ~ 2
))

selected_meta_regroup = selected_meta_regroup %>%  mutate(Occupation = case_when(
  Occupation == "1" ~ 1,
  Occupation == "2" ~ 2,
  Occupation == "3" ~ 3,
  Occupation == "4" ~ 3,
  Occupation == "5" ~ 2,
  Occupation == "6" ~ 4,
  Occupation == "7" ~ 5,
  Occupation == "8" ~ 4
))

selected_meta_regroup = selected_meta_regroup %>%  mutate(Income = case_when(
  Income == "1" ~ 1,
  Income == "2" ~ 1,
  Income == "3" ~ 2,
  Income == "4" ~ 2,
  Income == "5" ~ 3
))

selected_meta_regroup = selected_meta_regroup %>%  mutate(CouplePeriod = case_when(
  CouplePeriod == "1" ~ 1,
  CouplePeriod == "2" ~ 1,
  CouplePeriod == "3" ~ 1,
  CouplePeriod == "4" ~ 2,
  CouplePeriod == "5" ~ 2,
  CouplePeriod == "6" ~ 2,
  CouplePeriod == "7" ~ 2,
  CouplePeriod == "8" ~ 3,
  CouplePeriod == "9" ~ 4,
  CouplePeriod == "10" ~ 4
))

selected_meta_regroup = selected_meta_regroup %>%  mutate(NumCouple = case_when(
  NumCouple == "1" ~ 1,
  NumCouple == "2" ~ 1,
  NumCouple == "3" ~ 2,
  NumCouple == "4" ~ 2,
  NumCouple == "5" ~ 3
))

table(selected_meta_regroup$Gender)
prop.table(table(selected_meta_regroup$Gender))*100.0

table(selected_meta_regroup$Age_2)
prop.table(table(selected_meta_regroup$Age_2))*100.0

table(selected_meta_regroup$Marital)
prop.table(table(selected_meta_regroup$Marital))*100.0

table(selected_meta_regroup$Parental)
prop.table(table(selected_meta_regroup$Parental))*100.0

table(selected_meta_regroup$Education)
prop.table(table(selected_meta_regroup$Education))*100.0

table(selected_meta_regroup$Religion)
prop.table(table(selected_meta_regroup$Religion))*100.0

table(selected_meta_regroup$Occupation)
prop.table(table(selected_meta_regroup$Occupation))*100.0

table(selected_meta_regroup$Income)
prop.table(table(selected_meta_regroup$Income))*100.0

table(selected_meta_regroup$CoupleStatus)
prop.table(table(selected_meta_regroup$CoupleStatus))*100.0

table(selected_meta_regroup$CouplePeriod)
prop.table(table(selected_meta_regroup$CouplePeriod))*100.0

table(selected_meta_regroup$NumCouple)
prop.table(table(selected_meta_regroup$NumCouple))*100.0

############################################################ LPA Zscore #####################################################

selected_var = selected_score_zNorm[, 7:11]
lpa_result_vvi = estimate_profiles(df = selected_var, n_profiles = 1:9, variances = "varying", covariances = "zero")

comp_lpa_result_vvi = compare_solutions(lpa_result_vvi)
comp_lpa_result_vvi$fits
comp_lpa_result_vvi$best

lpa_result_vvi_1 = estimate_profiles(df = selected_var, n_profiles = 1, variances = "varying", covariances = "zero")
lpa_result_vvi_2 = estimate_profiles(df = selected_var, n_profiles = 2, variances = "varying", covariances = "zero")
lpa_result_vvi_3 = estimate_profiles(df = selected_var, n_profiles = 3, variances = "varying", covariances = "zero")
lpa_result_vvi_4 = estimate_profiles(df = selected_var, n_profiles = 4, variances = "varying", covariances = "zero")
lpa_result_vvi_5 = estimate_profiles(df = selected_var, n_profiles = 5, variances = "varying", covariances = "zero")
lpa_result_vvi_6 = estimate_profiles(df = selected_var, n_profiles = 6, variances = "varying", covariances = "zero")
lpa_result_vvi_7 = estimate_profiles(df = selected_var, n_profiles = 7, variances = "varying", covariances = "zero")
lpa_result_vvi_8 = estimate_profiles(df = selected_var, n_profiles = 8, variances = "varying", covariances = "zero")

fig_vvi3 = plot_profiles(lpa_result_vvi_3, add_line = TRUE, ci = NULL, sd = FALSE, rawdata = FALSE) 
fig_vvi3 + aes(linewidth = 1.2, size = 0.1) + scale_linewidth_identity() + theme_classic() + scale_y_continuous(limits = c(-1.0, 1.0), breaks = seq(-1.,1.0,0.25)) +
  guides(
    colour = guide_legend(override.aes = list(linewidth = 1.2, size = 4)),
    shape  = guide_legend(override.aes = list(size = 4)),
    fill   = guide_legend(override.aes = list(size = 4))
  ) +
  theme(
    legend.key.size = unit(1.2, "cm"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

fig_vvi4 = plot_profiles(lpa_result_vvi_4, add_line = TRUE, ci = NULL, sd = FALSE, rawdata = FALSE) 
fig_vvi4 + aes(linewidth = 1.2, size = 0.1) + scale_linewidth_identity() + theme_classic() + scale_y_continuous(limits = c(20, 60), breaks = seq(20,60,5))

lpa_result_vvi_4$model_2_class_4$fit
lpa_result_vvi_3$model_2_class_3$fit

lpa_result_vvi_4_est = get_estimates(lpa_result_vvi_4)
lpa_result_vvi_4_group = get_data(lpa_result_vvi_4)

lpa_result_vvi_3_est = get_estimates(lpa_result_vvi_3)
lpa_result_vvi_3_group = get_data(lpa_result_vvi_3)
selected_meta_regroup_lpa = cbind(selected_meta_regroup, lpa_result_vvi_3_group$Class)
selected_score_lpa = cbind(selected_score, lpa_result_vvi_3_group$Class)
selected_score_zNorm_lpa = cbind(selected_score_zNorm, lpa_result_vvi_3_group$Class)

colnames(selected_meta_regroup_lpa)[13] = 'Class'
colnames(selected_score_zNorm_lpa)[20] = 'Class'
colnames(selected_score_lpa)[20] = 'Class'

table(selected_meta_regroup_lpa$Class)
summary(selected_score_zNorm_lpa)
summary(selected_score_lpa)


######################################################### LPA Orin ###########################################################

# selected_var_orin = selected_score[, 7:11]
# lpa_result_vvi_orin = estimate_profiles(df = selected_var, n_profiles = 1:9, variances = "varying", covariances = "zero")
# 
# comp_lpa_result_vvi_orin = compare_solutions(lpa_result_vvi_orin)
# comp_lpa_result_vvi_orin$fits
# comp_lpa_result_vvi_orin$best
# 
# lpa_result_vvi_1_orin = estimate_profiles(df = selected_var_orin, n_profiles = 1, variances = "varying", covariances = "zero")
# lpa_result_vvi_2_orin = estimate_profiles(df = selected_var_orin, n_profiles = 2, variances = "varying", covariances = "zero")
# lpa_result_vvi_3_orin = estimate_profiles(df = selected_var_orin, n_profiles = 3, variances = "varying", covariances = "zero")
# lpa_result_vvi_4_orin = estimate_profiles(df = selected_var_orin, n_profiles = 4, variances = "varying", covariances = "zero")
# lpa_result_vvi_5_orin = estimate_profiles(df = selected_var_orin, n_profiles = 5, variances = "varying", covariances = "zero")
# lpa_result_vvi_6_orin = estimate_profiles(df = selected_var_orin, n_profiles = 6, variances = "varying", covariances = "zero")
# lpa_result_vvi_7_orin = estimate_profiles(df = selected_var_orin, n_profiles = 7, variances = "varying", covariances = "zero")
# lpa_result_vvi_8_orin = estimate_profiles(df = selected_var_orin, n_profiles = 8, variances = "varying", covariances = "zero")
# 
# fig_vvi4_orin = plot_profiles(lpa_result_vvi_4_orin, add_line = TRUE, ci = NULL, sd = FALSE, rawdata = FALSE) 
# fig_vvi4_orin + aes(linewidth = 1.2, size = 0.1) + scale_linewidth_identity() + theme_classic() + scale_y_continuous(limits = c(20, 60), breaks = seq(20,60,5))
# 
# lpa_result_vvi_4_orin$model_2_class_4$fit
# 
# lpa_result_vvi_4_est_orin = get_estimates(lpa_result_vvi_4_orin)
# lpa_result_vvi_4_group_orin = get_data(lpa_result_vvi_4_orin)
# selected_meta_regroup_lpa_orin = cbind(selected_meta_regroup, lpa_result_vvi_4_group_orin$Class)
# selected_score_lpa_orin = cbind(selected_score, lpa_result_vvi_4_group_orin$Class)
# #selected_score_zNorm_lpa_orin = cbind(selected_score_zNorm, lpa_result_vvi_4_group_orin$Class)
# 
# table(lpa_result_vvi_4_group_orin$Class)
# table(lpa_result_vvi_4_group$Class)
# 
# colnames(selected_meta_regroup_lpa_orin)[12] = 'Class'
# #colnames(selected_score_zNorm_lpa_orin)[20] = 'Class'
# colnames(selected_score_lpa_orin)[20] = 'Class'
# 
# table(selected_meta_regroup_lpa_orin$Class)
# #summary(selected_score_zNorm_lpa_orin)
# summary(selected_score_lpa_orin)

####################################################### Class Stat ########################################################

selected_score_zNorm_lpa$Class <- factor(selected_score_zNorm_lpa$Class)

# 2) class를 제외한 수치형 변수만 자동 선택
num_vars <- names(selected_score_zNorm_lpa)[
  sapply(selected_score_zNorm_lpa, is.numeric) & names(selected_score_zNorm_lpa) != "Class"
]

# 3) 통계함수
desc_fun <- function(x) {
  c(
    N      = sum(!is.na(x)),
    Mean   = mean(x, na.rm = TRUE),
    SD     = sd(x, na.rm = TRUE),
    Min    = min(x, na.rm = TRUE),
    Q1     = as.numeric(quantile(x, 0.25, na.rm = TRUE)),
    Median = median(x, na.rm = TRUE),
    Q3     = as.numeric(quantile(x, 0.75, na.rm = TRUE)),
    Max    = max(x, na.rm = TRUE)
  )
}

# 4) class별로 변수마다 기초통계 계산 -> 깔끔한 데이터프레임으로
out_list <- lapply(num_vars, function(v) {
  tmp <- by(selected_score_zNorm_lpa[[v]], selected_score_zNorm_lpa$Class, desc_fun)
  tmp_df <- as.data.frame(do.call(rbind, tmp))
  tmp_df$Variable <- v
  tmp_df$Class <- rownames(tmp_df)
  rownames(tmp_df) <- NULL
  tmp_df[, c("Variable","Class","N","Mean","SD","Min","Q1","Median","Q3","Max")]
})

desc_by_class <- do.call(rbind, out_list)

# 5) 보기 좋게 정렬/출력
desc_by_class <- desc_by_class[order(desc_by_class$Variable, desc_by_class$Class), ]
print(desc_by_class, row.names = FALSE)


selected_score_lpa$Class <- factor(selected_score_lpa$Class)

# 2) class를 제외한 수치형 변수만 자동 선택
num_vars <- names(selected_score_lpa)[
  sapply(selected_score_lpa, is.numeric) & names(selected_score_lpa) != "Class"
]

# 3) 통계함수
desc_fun <- function(x) {
  c(
    N      = sum(!is.na(x)),
    Mean   = mean(x, na.rm = TRUE),
    SD     = sd(x, na.rm = TRUE),
    Min    = min(x, na.rm = TRUE),
    Q1     = as.numeric(quantile(x, 0.25, na.rm = TRUE)),
    Median = median(x, na.rm = TRUE),
    Q3     = as.numeric(quantile(x, 0.75, na.rm = TRUE)),
    Max    = max(x, na.rm = TRUE)
  )
}

# 4) class별로 변수마다 기초통계 계산 -> 깔끔한 데이터프레임으로
out_list <- lapply(num_vars, function(v) {
  tmp <- by(selected_score_lpa[[v]], selected_score_lpa$Class, desc_fun)
  tmp_df <- as.data.frame(do.call(rbind, tmp))
  tmp_df$Variable <- v
  tmp_df$Class <- rownames(tmp_df)
  rownames(tmp_df) <- NULL
  tmp_df[, c("Variable","Class","N","Mean","SD","Min","Q1","Median","Q3","Max")]
})

desc_by_class <- do.call(rbind, out_list)

# 5) 보기 좋게 정렬/출력
desc_by_class <- desc_by_class[order(desc_by_class$Variable, desc_by_class$Class), ]
print(desc_by_class, row.names = FALSE)

####################################################### Anova to class #####################################################

# ------------------------
# 전처리
# ------------------------
selected_score_zNorm_lpa$Class <- as.factor(selected_score_zNorm_lpa$Class)

anova_results <- data.frame()
scheffe_results <- data.frame()

var_list <- names(selected_score_zNorm_lpa)[1:19]

# ------------------------
# 변수별 루프
# ------------------------
for (var in var_list) {
  
  fml <- as.formula(paste(var, "~ Class"))
  aov_model <- aov(fml, data = selected_score_zNorm_lpa)
  s <- summary(aov_model)[[1]]
  
  eta2 <- effectsize::eta_squared(aov_model, partial = FALSE)$Eta2[1]
  
  anova_results <- rbind(
    anova_results,
    data.frame(
      Variable = var,
      F_value = s$`F value`[1],
      df1 = s$Df[1],
      df2 = s$Df[2],
      p_value = s$`Pr(>F)`[1],
      eta2 = eta2
    )
  )
  
  # 그룹 평균을 이름 없이 숫자 벡터로 계산
  group_means <- as.numeric(tapply(
    selected_score_zNorm_lpa[[var]],
    selected_score_zNorm_lpa$Class,
    mean
  ))
  
  n_group <- as.numeric(table(selected_score_zNorm_lpa$Class))
  k <- length(n_group)
  
  MSerror <- s$`Mean Sq`[2]
  df_error <- s$Df[2]
  
  # ------------------------
  # Scheffé 사후검정 (순서 기반 비교명)
  # ------------------------
  for (i in 1:(k - 1)) {
    for (j in (i + 1):k) {
      
      mean_diff <- group_means[i] - group_means[j]
      se_diff <- sqrt(MSerror * (1/n_group[i] + 1/n_group[j]))
      
      F_crit <- qf(0.95, df1 = k - 1, df2 = df_error)
      scheffe_crit <- sqrt((k - 1) * F_crit) * se_diff
      
      p_val <- 1 - pf(
        (mean_diff^2) / (se_diff^2 * (1 / (k - 1))),
        k - 1,
        df_error
      )
      
      # ❗ 이름이 없으므로 Group 번호로 생성
      comparison_label <- paste0("Group ", i, " - Group ", j)
      
      scheffe_results <- rbind(
        scheffe_results,
        data.frame(
          Variable   = var,
          Comparison = comparison_label,
          Difference = mean_diff,
          Lower_CI   = mean_diff - scheffe_crit,
          Upper_CI   = mean_diff + scheffe_crit,
          p_value    = p_val,
          stringsAsFactors = FALSE
        )
      )
    }
  }
}

# ------------------------
# 출력
# ------------------------
cat("\n========== ANOVA 및 Eta² 결과 ==========\n")
print(anova_results %>% mutate(across(where(is.numeric), round, 6)), row.names = FALSE)

cat("\n========== Scheffé 사후검정 결과 ==========\n")
print(scheffe_results %>% mutate(across(where(is.numeric), round, 4)), row.names = FALSE)


################################################### Anova to class Orin ####################################################
 
# ------------------------
# 전처리
# ------------------------
selected_score_lpa$Class <- as.factor(selected_score_lpa$Class)

anova_results <- data.frame()
scheffe_results <- data.frame()

var_list <- names(selected_score_lpa)[1:19]

# ------------------------
# 변수별 루프
# ------------------------
for (var in var_list) {
  
  fml <- as.formula(paste(var, "~ Class"))
  aov_model <- aov(fml, data = selected_score_lpa)
  s <- summary(aov_model)[[1]]
  
  eta2 <- effectsize::eta_squared(aov_model, partial = FALSE)$Eta2[1]
  
  anova_results <- rbind(
    anova_results,
    data.frame(
      Variable = var,
      F_value = s$`F value`[1],
      df1 = s$Df[1],
      df2 = s$Df[2],
      p_value = s$`Pr(>F)`[1],
      eta2 = eta2
    )
  )
  
  # 그룹 평균을 이름 없이 숫자 벡터로 계산
  group_means <- as.numeric(tapply(
    selected_score_lpa[[var]],
    selected_score_lpa$Class,
    mean
  ))
  
  n_group <- as.numeric(table(selected_score_lpa$Class))
  k <- length(n_group)
  
  MSerror <- s$`Mean Sq`[2]
  df_error <- s$Df[2]
  
  # ------------------------
  # Scheffé 사후검정 (순서 기반 비교명)
  # ------------------------
  for (i in 1:(k - 1)) {
    for (j in (i + 1):k) {
      
      mean_diff <- group_means[i] - group_means[j]
      se_diff <- sqrt(MSerror * (1/n_group[i] + 1/n_group[j]))
      
      F_crit <- qf(0.95, df1 = k - 1, df2 = df_error)
      scheffe_crit <- sqrt((k - 1) * F_crit) * se_diff
      
      p_val <- 1 - pf(
        (mean_diff^2) / (se_diff^2 * (1 / (k - 1))),
        k - 1,
        df_error
      )
      
      # ❗ 이름이 없으므로 Group 번호로 생성
      comparison_label <- paste0("Group ", i, " - Group ", j)
      
      scheffe_results <- rbind(
        scheffe_results,
        data.frame(
          Variable   = var,
          Comparison = comparison_label,
          Difference = mean_diff,
          Lower_CI   = mean_diff - scheffe_crit,
          Upper_CI   = mean_diff + scheffe_crit,
          p_value    = p_val,
          stringsAsFactors = FALSE
        )
      )
    }
  }
}

# ------------------------
# 출력
# ------------------------
cat("\n========== ANOVA 및 Eta² 결과 ==========\n")
print(anova_results %>% mutate(across(where(is.numeric), round, 6)), row.names = FALSE)

cat("\n========== Scheffé 사후검정 결과 ==========\n")
print(scheffe_results %>% mutate(across(where(is.numeric), round, 4)), row.names = FALSE)


############################################# Chi'square test & ratio #########################################################

table(selected_meta_regroup_lpa$Class)
prop.table(table(selected_meta_regroup_lpa$Class))*100.0

table_Gender = table(selected_meta_regroup_lpa$Gender, selected_meta_regroup_lpa$Class)
print(table_Gender)
prop.table(table_Gender)*100.0

table_Age = table(selected_meta_regroup_lpa$Age_2, selected_meta_regroup_lpa$Class)
print(table_Age)
prop.table(table_Age)*100.0

table_Marital = table(selected_meta_regroup_lpa$Marital, selected_meta_regroup_lpa$Class)
print(table_Marital)
prop.table(table_Marital) *100.0

table_Parental = table(selected_meta_regroup_lpa$Parental, selected_meta_regroup_lpa$Class)
print(table_Parental)
prop.table(table_Parental) *100.0

table_Education = table(selected_meta_regroup_lpa$Education, selected_meta_regroup_lpa$Class)
print(table_Education)
prop.table(table_Education)*100.0

table_Religion = table(selected_meta_regroup_lpa$Religion, selected_meta_regroup_lpa$Class)
print(table_Religion)
prop.table(table_Religion)*100.0

table_Occupation = table(selected_meta_regroup_lpa$Occupation, selected_meta_regroup_lpa$Class)
print(table_Occupation)
prop.table(table_Occupation)*100.0

table_Income = table(selected_meta_regroup_lpa$Income, selected_meta_regroup_lpa$Class)
print(table_Income)
prop.table(table_Income)*100.0

table_CoupleStatus = table(selected_meta_regroup_lpa$CoupleStatus, selected_meta_regroup_lpa$Class)
print(table_CoupleStatus)
prop.table(table_CoupleStatus)*100.0

table_CouplePeriod = table(selected_meta_regroup_lpa$CouplePeriod, selected_meta_regroup_lpa$Class)
print(table_CouplePeriod)
prop.table(table_CouplePeriod)*100.0

table_NumCouple = table(selected_meta_regroup_lpa$NumCouple, selected_meta_regroup_lpa$Class)
print(table_NumCouple)
prop.table(table_NumCouple)*100.0

selected_meta_regroup_lpa$Class = as.factor(selected_meta_regroup_lpa$Class)

chi_Gender = chisq.test(table_Gender)
print(chi_Gender)
cramersV(table_Gender)

chi_Age = chisq.test(table_Age)
print(chi_Age)
cramersV(table_Age)

chi_Marital = chisq.test(table_Marital)
print(chi_Marital)
cramersV(table_Marital)

chi_Parental = chisq.test(table_Parental)
print(chi_Parental)
cramersV(table_Parental)

chi_Education = chisq.test(table_Education)
print(chi_Education)
cramersV(table_Education)

chi_Religion = chisq.test(table_Religion)
print(chi_Religion)
cramersV(table_Religion)

chi_Occupation = chisq.test(table_Occupation)
print(chi_Occupation)
cramersV(table_Occupation)

chi_Income= chisq.test(table_Income)
print(chi_Income)
cramersV(table_Income)

chi_CoupleStatus = chisq.test(table_CoupleStatus)
print(chi_CoupleStatus)
cramersV(table_CoupleStatus)

chi_CouplePeriod = chisq.test(table_CouplePeriod)
print(chi_CouplePeriod)
cramersV(table_CouplePeriod)

chi_NumCouple = chisq.test(table_NumCouple)
print(chi_NumCouple)
cramersV(table_NumCouple)

selected_meta_regroup_lpa$Class <- as.factor(selected_meta_regroup_lpa$Class)

# 변수 목록
variables <- c("Gender", "Age_2", "Marital", "Parental", "Education", "Religion", "Occupation", "Income", "CoupleStatus", "CouplePeriod", "NumCouple")

# 결과 저장용 리스트
chi_results <- list()

# 각 변수에 대해 카이제곱 결과와 cramér's V 저장
for (v in variables) {
  tbl <- table(selected_meta_regroup_lpa[[v]], selected_meta_regroup_lpa$Class)
  chi <- chisq.test(tbl)
  chi_summary <- tidy(chi)
  cramer <- assocstats(tbl)$cramer
  
  chi_results[[v]] <- data.frame(
    Variable = v,
    Chi_Square = round(chi_summary$statistic, 3),
    df = chi_summary$parameter,
    p_value = round(chi_summary$p.value, 4),
    Cramers_V = round(cramer, 3)
  )
}

# 하나의 데이터프레임으로 결합
chi_table <- bind_rows(chi_results)

# 콘솔에 출력
print(chi_table)

##################################################  BCH 4-class  #########################################################
# 
# posterior_probs <- lpa_result_vvi_3$model_2_class_3$model$z  # 242 x 3 행렬
# #posterior_probs <- lpa_result_vvi_4$model_2_class_4$model$z  # 242 x 3 행렬
# ext <- selected_score_zNorm  # 외생변수
# group_factor <- factor(apply(posterior_probs, 1, which.max), levels = 1:4)
# 
# # 2. BCH 기반 회귀분석 수행
# results <- list()
# for (k in 1:4) {
#   weight_k <- posterior_probs[, k]
#   data_k <- data.frame(ext, group_factor = group_factor)
#   
#   design_k <- svydesign(ids = ~1, weights = ~weight_k, data = data_k)
#   
#   model_list <- list()
#   test_list <- list()
#   
#   for (var_name in colnames(ext)) {
#     fml <- as.formula(paste(var_name, "~ group_factor"))
#     
#     # 오류 처리 포함
#     model <- try(svyglm(fml, design = design_k), silent = TRUE)
#     if (!inherits(model, "try-error")) {
#       test <- try(regTermTest(model, ~group_factor), silent = TRUE)
#       model_list[[var_name]] <- model
#       test_list[[var_name]] <- if (!inherits(test, "try-error")) test else NULL
#     } else {
#       model_list[[var_name]] <- NULL
#       test_list[[var_name]] <- NULL
#     }
#   }
#   
#   results[[k]] <- list(model = model_list, test = test_list)
# }
# 
# # 3. 회귀결과 요약표 생성 + 각 클래스별 평균 및 표준편차 추가
# summary_table <- data.frame()
# 
# for (k in 1:3) {
#   for (var_name in names(results[[k]]$model)) {
#     model_obj <- results[[k]]$model[[var_name]]
#     test_obj <- results[[k]]$test[[var_name]]
#     
#     # 회귀계수 추출
#     if (!is.null(model_obj)) {
#       coef_vec <- coef(model_obj)
#       coef2 <- if ("group_factor2" %in% names(coef_vec)) round(coef_vec["group_factor2"], 3) else NA
#       coef3 <- if ("group_factor3" %in% names(coef_vec)) round(coef_vec["group_factor3"], 3) else NA
#       coef4 <- if ("group_factor4" %in% names(coef_vec)) round(coef_vec["group_factor4"], 3) else NA
#     } else {
#       coef2 <- NA
#       coef3 <- NA
#       coef4 <- NA
#     }
#     
#     # Wald Chi² 결과
#     if (!is.null(test_obj)) {
#       wald <- if (!is.null(test_obj$Ftest)) round(unname(test_obj$Ftest), 3) else NA_real_
#       df   <- if (!is.null(test_obj$df)) paste0(test_obj$df, ", ", round(test_obj$ddf)) else NA_character_
#       pval <- if (!is.null(test_obj$p)) round(unname(test_obj$p), 4) else NA_real_
#     } else {
#       wald <- NA_real_
#       df   <- NA_character_
#       pval <- NA_real_
#     }
#     
#     # 클래스별 평균과 표준편차 계산
#     ext_values <- ext[[var_name]]
#     class_idx <- which(group_factor == k)
#     class_values <- ext_values[class_idx]
#     class_mean <- round(mean(class_values, na.rm = TRUE), 3)
#     class_sd <- round(sd(class_values, na.rm = TRUE), 3)
#     
#     # 결과 테이블에 추가
#     summary_table <- rbind(summary_table, data.frame(
#       Class = k,
#       Variable = var_name,
#       Coef_group2 = coef2,
#       Coef_group3 = coef3,
#       Coef_group4 = coef4,
#       Wald_Chi2 = wald,
#       df = df,
#       p_value = pval,
#       Class_Mean = class_mean,
#       Class_SD = class_sd
#     ))
#   }
# }
# 
# # 4. 결과 확인 또는 저장
# print(summary_table)

##################################################  BCH 3-class  #########################################################

posterior_probs <- lpa_result_vvi_3$model_2_class_3$model$z  # 242 x 3 행렬
#posterior_probs <- lpa_result_vvi_4$model_2_class_4$model$z  # 242 x 3 행렬
ext <- selected_score_zNorm  # 외생변수
group_factor <- factor(apply(posterior_probs, 1, which.max), levels = 1:3)

# 2. BCH 기반 회귀분석 수행
results <- list()
for (k in 1:3) {
  weight_k <- posterior_probs[, k]
  data_k <- data.frame(ext, group_factor = group_factor)
  
  design_k <- svydesign(ids = ~1, weights = ~weight_k, data = data_k)
  
  model_list <- list()
  test_list <- list()
  
  for (var_name in colnames(ext)) {
    fml <- as.formula(paste(var_name, "~ group_factor"))
    
    # 오류 처리 포함
    model <- try(svyglm(fml, design = design_k), silent = TRUE)
    if (!inherits(model, "try-error")) {
      test <- try(regTermTest(model, ~group_factor), silent = TRUE)
      model_list[[var_name]] <- model
      test_list[[var_name]] <- if (!inherits(test, "try-error")) test else NULL
    } else {
      model_list[[var_name]] <- NULL
      test_list[[var_name]] <- NULL
    }
  }
  
  results[[k]] <- list(model = model_list, test = test_list)
}

# 3. 회귀결과 요약표 생성 + 각 클래스별 평균 및 표준편차 추가
summary_table <- data.frame()

for (k in 1:3) {
  for (var_name in names(results[[k]]$model)) {
    model_obj <- results[[k]]$model[[var_name]]
    test_obj <- results[[k]]$test[[var_name]]
    
    # 회귀계수 추출
    if (!is.null(model_obj)) {
      coef_vec <- coef(model_obj)
      coef2 <- if ("group_factor2" %in% names(coef_vec)) round(coef_vec["group_factor2"], 3) else NA
      coef3 <- if ("group_factor3" %in% names(coef_vec)) round(coef_vec["group_factor3"], 3) else NA
      coef4 <- if ("group_factor4" %in% names(coef_vec)) round(coef_vec["group_factor4"], 3) else NA
    } else {
      coef2 <- NA
      coef3 <- NA
      coef4 <- NA
    }
    
    # Wald Chi² 결과
    if (!is.null(test_obj)) {
      wald <- if (!is.null(test_obj$Ftest)) round(unname(test_obj$Ftest), 3) else NA_real_
      df   <- if (!is.null(test_obj$df)) paste0(test_obj$df, ", ", round(test_obj$ddf)) else NA_character_
      pval <- if (!is.null(test_obj$p)) round(unname(test_obj$p), 4) else NA_real_
    } else {
      wald <- NA_real_
      df   <- NA_character_
      pval <- NA_real_
    }
    
    # 클래스별 평균과 표준편차 계산
    ext_values <- ext[[var_name]]
    class_idx <- which(group_factor == k)
    class_values <- ext_values[class_idx]
    class_mean <- round(mean(class_values, na.rm = TRUE), 3)
    class_sd <- round(sd(class_values, na.rm = TRUE), 3)
    
    # 결과 테이블에 추가
    summary_table <- rbind(summary_table, data.frame(
      Class = k,
      Variable = var_name,
      Coef_group2 = coef2,
      Coef_group3 = coef3,
      Wald_Chi2 = wald,
      df = df,
      p_value = pval,
      Class_Mean = class_mean,
      Class_SD = class_sd
    ))
  }
}

# 4. 결과 확인 또는 저장
print(summary_table)

##################################################  BCH Orin 3-class  #########################################################

options(survey.lonely.psu = "adjust")

wald_bch_with_mean_sd <- function(posterior_probs, ext, var_list = NULL) {
  
  if (!is.matrix(posterior_probs)) posterior_probs <- as.matrix(posterior_probs)
  n <- nrow(posterior_probs)
  K <- ncol(posterior_probs)
  
  if (nrow(ext) != n) {
    stop("행 수 불일치: ext의 nrow와 posterior_probs의 nrow가 다릅니다. (LPA에 사용된 행 정렬을 먼저 맞추세요.)")
  }
  
  if (is.null(var_list)) var_list <- names(ext)
  var_list <- intersect(var_list, names(ext))
  var_list <- var_list[sapply(ext[var_list], is.numeric)]
  
  ## long 데이터 생성: 개인 i를 K번 복제
  id_long    <- rep(seq_len(n), times = K)
  Class_long <- factor(rep(seq_len(K), each = n), levels = seq_len(K))
  w_long     <- as.vector(posterior_probs)  # class1 n개 -> class2 n개 -> ...
  
  long_df <- ext[id_long, var_list, drop = FALSE]
  long_df$id    <- id_long
  long_df$Class <- Class_long
  long_df$w     <- w_long
  
  ## survey design (개인 id를 PSU로)
  des <- svydesign(ids = ~id, weights = ~w, data = long_df)
  
  ## 대비행렬(omnibus): (mu2-mu1)=0, (mu3-mu1)=0, ... (muK-mu1)=0
  make_L_omni <- function(K) {
    L <- matrix(0, nrow = K - 1, ncol = K)
    for (k in 2:K) {
      L[k - 1, 1] <- -1
      L[k - 1, k] <-  1
    }
    L
  }
  
  ## 쌍별 대비벡터: mu_i - mu_j
  make_c_pair <- function(K, i, j) {
    cvec <- rep(0, K)
    cvec[i] <-  1
    cvec[j] <- -1
    cvec
  }
  
  desc_tbl     <- data.frame()
  omnibus_tbl  <- data.frame()
  pairwise_tbl <- data.frame()
  
  for (v in var_list) {
    
    ## -------- Profile별 Mean / SD (가중치 기반) --------
    mean_vec <- rep(NA_real_, K)
    sd_vec   <- rep(NA_real_, K)
    n_eff    <- rep(NA_real_, K)
    
    for (k in 1:K) {
      des_k <- subset(des, Class == k)
      fk <- as.formula(paste0("~`", v, "`"))
      
      m_k <- try(svymean(fk, design = des_k, na.rm = TRUE), silent = TRUE)
      v_k <- try(svyvar (fk, design = des_k, na.rm = TRUE), silent = TRUE)
      
      if (!inherits(m_k, "try-error")) mean_vec[k] <- as.numeric(coef(m_k))
      if (!inherits(v_k, "try-error")) {
        var_est <- as.numeric(coef(v_k))
        sd_vec[k] <- if (!is.na(var_est) && var_est >= 0) sqrt(var_est) else NA_real_
      }
      
      ## 기대 표본크기(=가중치 합; posterior 기반이면 expected class size)
      n_eff[k] <- sum(long_df$w[long_df$Class == k], na.rm = TRUE)
    }
    
    desc_tbl <- rbind(
      desc_tbl,
      data.frame(
        Variable = v,
        Profile  = seq_len(K),
        N_eff    = n_eff,
        Mean     = mean_vec,
        SD       = sd_vec,
        stringsAsFactors = FALSE
      )
    )
    
    ## -------- Wald χ²를 위한 svyglm: coef = 각 프로파일 평균 --------
    fml <- as.formula(paste0("`", v, "` ~ 0 + Class"))
    fit <- try(svyglm(fml, design = des), silent = TRUE)
    if (inherits(fit, "try-error")) next
    
    b <- coef(fit)
    V <- vcov(fit)
    
    ## coef 정렬(Class1..ClassK)
    cn <- paste0("Class", seq_len(K))
    if (!all(cn %in% names(b))) next
    b <- b[cn]
    V <- V[cn, cn, drop = FALSE]
    
    if (anyNA(b) || anyNA(V)) next
    
    ## -------- Omnibus Wald χ² (df=K-1) --------
    L <- make_L_omni(K)
    Lb <- as.matrix(L %*% b)
    LVLt <- L %*% V %*% t(L)
    
    x <- tryCatch(
      solve(LVLt, Lb),
      error = function(e) qr.solve(LVLt, Lb)
    )
    W_omni <- as.numeric(t(Lb) %*% x)
    df_omni <- K - 1
    p_omni <- pchisq(W_omni, df = df_omni, lower.tail = FALSE)
    
    omnibus_tbl <- rbind(
      omnibus_tbl,
      data.frame(
        Variable  = v,
        Wald_Chi2 = W_omni,
        df        = df_omni,
        p_value   = p_omni,
        stringsAsFactors = FALSE
      )
    )
    
    ## -------- Pairwise Wald χ² (df=1) + Mean/SD 함께 --------
    for (i in 1:(K - 1)) {
      for (j in (i + 1):K) {
        
        cvec <- make_c_pair(K, i, j)
        est  <- as.numeric(sum(cvec * b))
        varc <- as.numeric(t(cvec) %*% V %*% cvec)
        
        if (is.na(varc) || varc <= 0) next
        
        W_pair <- (est^2) / varc
        p_pair <- pchisq(W_pair, df = 1, lower.tail = FALSE)
        
        pairwise_tbl <- rbind(
          pairwise_tbl,
          data.frame(
            Variable  = v,
            Contrast  = paste0("Profile ", i, " vs ", j),
            Mean_i    = mean_vec[i],
            SD_i      = sd_vec[i],
            Mean_j    = mean_vec[j],
            SD_j      = sd_vec[j],
            Estimate  = est,
            Wald_Chi2 = W_pair,
            df        = 1,
            p_value   = p_pair,
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }
  
  ## 정렬
  desc_tbl     <- desc_tbl[order(desc_tbl$Variable, desc_tbl$Profile), ]
  omnibus_tbl  <- omnibus_tbl[order(omnibus_tbl$p_value), ]
  pairwise_tbl <- pairwise_tbl[order(pairwise_tbl$Variable, pairwise_tbl$p_value), ]
  
  list(desc = desc_tbl, omnibus = omnibus_tbl, pairwise = pairwise_tbl)
}

############################################################
## 실행 (3-class)
############################################################

posterior_probs <- lpa_result_vvi_3$model_2_class_3$model$z  # N x 3

## (권장) LPA에 실제로 사용된 행으로 ext 정렬(필요한 경우에만)
lpa_used <- get_data(lpa_result_vvi_3)
idx <- suppressWarnings(as.integer(rownames(lpa_used)))
if (!anyNA(idx) && length(idx) == nrow(posterior_probs)) {
  ext_z   <- selected_score_zNorm[idx, , drop = FALSE]
  ext_raw <- selected_score[idx, , drop = FALSE]
} else {
  ext_z   <- selected_score_zNorm
  ext_raw <- selected_score
}

## z-score 결과
res_z <- wald_bch_with_mean_sd(posterior_probs, ext_z)

print(res_z$desc %>% transform(
  N_eff = round(N_eff, 2),
  Mean  = round(Mean, 3),
  SD    = round(SD, 3)
))

print(res_z$omnibus %>% transform(
  Wald_Chi2 = round(Wald_Chi2, 3),
  p_value   = round(p_value, 4)
))

print(res_z$pairwise %>% transform(
  Mean_i    = round(Mean_i, 3),
  SD_i      = round(SD_i, 3),
  Mean_j    = round(Mean_j, 3),
  SD_j      = round(SD_j, 3),
  Estimate  = round(Estimate, 3),
  Wald_Chi2 = round(Wald_Chi2, 3),
  p_value   = round(p_value, 4)
))

## 원점수 결과
res_raw <- wald_bch_with_mean_sd(posterior_probs, ext_raw)

print(res_raw$desc %>% transform(
  N_eff = round(N_eff, 2),
  Mean  = round(Mean, 3),
  SD    = round(SD, 3)
))

print(res_raw$omnibus %>% transform(
  Wald_Chi2 = round(Wald_Chi2, 3),
  p_value   = round(p_value, 4)
))

print(res_raw$pairwise %>% transform(
  Mean_i    = round(Mean_i, 3),
  SD_i      = round(SD_i, 3),
  Mean_j    = round(Mean_j, 3),
  SD_j      = round(SD_j, 3),
  Estimate  = round(Estimate, 3),
  Wald_Chi2 = round(Wald_Chi2, 3),
  p_value   = round(p_value, 4)
))

##########################################################  Prediction  #######################################################################

tidymodels::tidymodels_prefer()
set.seed(20260224)

## =========================================================
## 1) Data
## =========================================================
selected_big5_zNorm_lpa <- selected_score_zNorm_lpa[, c(7, 8, 9, 10, 11, 20)]
selected_big5_zNorm_lpa$Class <- as.factor(selected_big5_zNorm_lpa$Class)
selected_big5_zNorm_lpa <- selected_big5_zNorm_lpa[!is.na(selected_big5_zNorm_lpa$Class), ]

if (nlevels(selected_big5_zNorm_lpa$Class) != 3) {
  stop(paste0("Outcome must have exactly 3 classes. Current: ",
              nlevels(selected_big5_zNorm_lpa$Class)))
}

outcome_col <- "Class"
ref_levels <- levels(selected_big5_zNorm_lpa[[outcome_col]])

## =========================================================
## 2) Repeated CV: 5-fold x 20 repeats = 100 resamples
##    - id: fold index (1~5)
##    - id2: repeat index (1~20)
##    - resample_id: rep+fold를 합친 고유 ID
## =========================================================
folds <- rsample::vfold_cv(
  selected_big5_zNorm_lpa,
  v = 5,
  repeats = 20,
  strata = Class
) %>%
  dplyr::mutate(resample_id = paste0("rep", id2, "_fold", id))

## =========================================================
## 3) Recipe
## =========================================================
rec <- recipes::recipe(Class ~ ., data = selected_big5_zNorm_lpa) %>%
  recipes::step_impute_median(recipes::all_numeric_predictors()) %>%
  recipes::step_impute_mode(recipes::all_nominal_predictors()) %>%
  recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = TRUE) %>%
  recipes::step_zv(recipes::all_predictors()) %>%
  recipes::step_normalize(recipes::all_numeric_predictors())

## =========================================================
## 4) Model specs (Multiclass)
## =========================================================
enet_spec <- parsnip::multinom_reg(penalty = 0.01, mixture = 0.50) %>%
  parsnip::set_engine("glmnet")

xgb_spec <- parsnip::boost_tree(
  trees = 800,
  tree_depth = 6,
  learn_rate = 0.05,
  loss_reduction = 0.0,
  min_n = 10,
  sample_size = 0.8,
  mtry = 0.8
) %>%
  parsnip::set_engine(
    "xgboost",
    objective = "multi:softprob",
    num_class = 3,
    eval_metric = "mlogloss",
    counts = FALSE
  ) %>%
  parsnip::set_mode("classification")

mlp_spec <- parsnip::mlp(hidden_units = 16, penalty = 0.01, epochs = 300) %>%
  parsnip::set_engine("nnet", MaxNWts = 20000, trace = FALSE) %>%
  parsnip::set_mode("classification")

## =========================================================
## 5) Workflows
## =========================================================
wf_enet <- workflows::workflow() %>% workflows::add_recipe(rec) %>% workflows::add_model(enet_spec)
wf_xgb  <- workflows::workflow() %>% workflows::add_recipe(rec) %>% workflows::add_model(xgb_spec)
wf_mlp  <- workflows::workflow() %>% workflows::add_recipe(rec) %>% workflows::add_model(mlp_spec)

## =========================================================
## 6) Metric utilities (class/prob 분리)
## =========================================================
metric_class <- yardstick::metric_set(
  yardstick::accuracy,
  yardstick::precision,
  yardstick::recall,
  yardstick::f_meas
)

metric_prob <- yardstick::metric_set(
  yardstick::mn_log_loss
)

calc_metrics_from_preds <- function(pred_df, outcome_col) {
  if (!(".pred_class" %in% names(pred_df))) stop("Missing .pred_class in prediction data.")
  pred_df[[outcome_col]] <- factor(pred_df[[outcome_col]], levels = ref_levels)
  
  prob_cols <- grep("^\\.pred_", names(pred_df), value = TRUE)
  prob_cols <- setdiff(prob_cols, ".pred_class")
  if (length(prob_cols) == 0) stop("No probability columns (.pred_*) found for mn_log_loss.")
  
  m_class <- metric_class(
    pred_df,
    truth = !!rlang::sym(outcome_col),
    estimate = .pred_class,
    estimator = "macro"
  )
  
  m_prob <- metric_prob(
    pred_df,
    truth = !!rlang::sym(outcome_col),
    !!!rlang::syms(prob_cols)
  )
  
  dplyr::bind_rows(m_class, m_prob)
}

## =========================================================
## 7) Fold-level TRAIN + TEST metrics (100 resamples)
## =========================================================
compute_fold_metrics_train_test_rep <- function(wf, folds, model_name, outcome_col = "Class") {
  
  one_resample <- function(split, resample_id) {
    train_dat <- rsample::analysis(split)
    test_dat  <- rsample::assessment(split)
    
    train_dat[[outcome_col]] <- factor(train_dat[[outcome_col]], levels = ref_levels)
    test_dat[[outcome_col]]  <- factor(test_dat[[outcome_col]],  levels = ref_levels)
    
    fit_wf <- parsnip::fit(wf, data = train_dat)
    
    # TRAIN preds
    tr_preds <- dplyr::bind_cols(
      train_dat %>% dplyr::select(!!rlang::sym(outcome_col)),
      predict(fit_wf, new_data = train_dat, type = "class"),
      predict(fit_wf, new_data = train_dat, type = "prob")
    )
    
    # TEST preds
    te_preds <- dplyr::bind_cols(
      test_dat %>% dplyr::select(!!rlang::sym(outcome_col)),
      predict(fit_wf, new_data = test_dat, type = "class"),
      predict(fit_wf, new_data = test_dat, type = "prob")
    )
    
    m_train <- calc_metrics_from_preds(tr_preds, outcome_col) %>% dplyr::mutate(set = "train")
    m_test  <- calc_metrics_from_preds(te_preds, outcome_col) %>% dplyr::mutate(set = "test")
    
    dplyr::bind_rows(m_train, m_test) %>%
      dplyr::mutate(resample = resample_id, model = model_name) %>%
      dplyr::select(resample, set, model, .metric, .estimate)
  }
  
  purrr::map2_dfr(folds$splits, folds$resample_id, one_resample)
}

fold_metrics_tt <- dplyr::bind_rows(
  compute_fold_metrics_train_test_rep(wf_enet, folds, "ElasticNet", outcome_col),
  compute_fold_metrics_train_test_rep(wf_xgb,  folds, "XGBoost",    outcome_col),
  compute_fold_metrics_train_test_rep(wf_mlp,  folds, "MLP",        outcome_col)
) %>%
  dplyr::mutate(
    model = factor(model, levels = c("ElasticNet", "XGBoost", "MLP")),
    set   = factor(set, levels = c("train", "test"))
  )

print(fold_metrics_tt)

## =========================================================
## 8) Summary (train/test 모두) + 소수점 4째자리 (100 resamples 기준)
## =========================================================
metric_summary_tt <- fold_metrics_tt %>%
  dplyr::group_by(set, model, .metric) %>%
  dplyr::summarise(
    mean = round(mean(.estimate, na.rm = TRUE), 4),
    sd   = round(sd(.estimate, na.rm = TRUE), 4),
    .groups = "drop"
  )

print(metric_summary_tt)

## =========================================================
## 9) ANOVA + Tukey post-hoc (train/test 각각) + p-value + 95% CI
##    - 반복 CV에서는 랜덤효과를 (1|resample)로 두는 것이 자연스럽습니다.
## =========================================================
run_anova_posthoc <- function(metric_name, data, which_set = c("train", "test"),
                              df_method = c("Satterthwaite", "Kenward-Roger")) {
  
  which_set <- match.arg(which_set)
  df_method <- match.arg(df_method)
  
  dat <- data %>% dplyr::filter(set == which_set, .metric == metric_name)
  
  # 1) lmerTest로 적합 (클래스가 lmerModLmerTest가 됨)
  fit <- lmerTest::lmer(.estimate ~ model + (1 | resample), data = dat, REML = TRUE)
  
  # 2) Omnibus: stats::anova()를 써야 F와 p가 안정적으로 출력됨
  omnibus <- stats::anova(fit, ddf = df_method)   # <-- F value, Pr(>F) 나옵니다
  
  # 3) Post-hoc: Tukey + p + 95% CI
  emm <- emmeans::emmeans(
    fit, ~ model,
    lmer.df = ifelse(df_method == "Kenward-Roger", "kenward-roger", "satterthwaite")
  )
  posthoc <- emmeans::contrast(emm, method = "pairwise", adjust = "tukey")
  
  posthoc_tbl <- as.data.frame(posthoc)
  ci_tbl <- as.data.frame(confint(posthoc, level = 0.95))
  
  # CI 컬럼명은 버전별로 달라서 자동 탐색
  low_col  <- names(ci_tbl)[grepl("lower|lcl", names(ci_tbl), ignore.case = TRUE)][1]
  high_col <- names(ci_tbl)[grepl("upper|ucl", names(ci_tbl), ignore.case = TRUE)][1]
  if (is.na(low_col) || is.na(high_col)) stop("CI columns not found in confint(posthoc) output.")
  
  ci_tbl2 <- ci_tbl %>%
    dplyr::select(contrast, !!low_col, !!high_col) %>%
    dplyr::rename(CI_low = !!low_col, CI_high = !!high_col)
  
  posthoc_out <- dplyr::left_join(posthoc_tbl, ci_tbl2, by = "contrast")
  
  # 숫자 컬럼 소수점 4자리(단 p-value는 그대로)
  num_cols <- names(posthoc_out)[vapply(posthoc_out, is.numeric, logical(1))]
  num_cols <- setdiff(num_cols, c("p.value", "p.value."))
  posthoc_out[num_cols] <- lapply(posthoc_out[num_cols], function(x) round(x, 4))
  
  list(set = which_set, metric = metric_name, omnibus = omnibus, posthoc_out = posthoc_out)
}

###########################################################################################################################
## =========================================================
## ANOVA omnibus 결과를 한 표로 모으기 (TRAIN + TEST)
## 전제: results_train, results_test 가 이미 생성되어 있음
## =========================================================

run_anova_posthoc_F <- function(metric_name, data, which_set = c("train", "test"),
                                df_method = c("Satterthwaite", "Kenward-Roger")) {
  
  which_set <- match.arg(which_set)
  df_method <- match.arg(df_method)
  
  dat <- data %>% dplyr::filter(set == which_set, .metric == metric_name)
  
  fit <- lmerTest::lmer(.estimate ~ model + (1 | resample), data = dat, REML = TRUE)
  
  # ★ 여기서 F + Pr(>F) 나옵니다 (핵심)
  omnibus <- stats::anova(fit, ddf = df_method)
  
  emm <- emmeans::emmeans(
    fit, ~ model,
    lmer.df = ifelse(df_method == "Kenward-Roger", "kenward-roger", "satterthwaite")
  )
  posthoc <- emmeans::contrast(emm, method = "pairwise", adjust = "tukey")
  
  posthoc_tbl <- as.data.frame(posthoc)
  ci_tbl <- as.data.frame(confint(posthoc, level = 0.95))
  
  low_col  <- names(ci_tbl)[grepl("lower|lcl", names(ci_tbl), ignore.case = TRUE)][1]
  high_col <- names(ci_tbl)[grepl("upper|ucl", names(ci_tbl), ignore.case = TRUE)][1]
  if (is.na(low_col) || is.na(high_col)) stop("CI columns not found in confint(posthoc) output.")
  
  ci_tbl2 <- ci_tbl %>%
    dplyr::select(contrast, !!low_col, !!high_col) %>%
    dplyr::rename(CI_low = !!low_col, CI_high = !!high_col)
  
  posthoc_out <- dplyr::left_join(posthoc_tbl, ci_tbl2, by = "contrast")
  
  num_cols <- names(posthoc_out)[vapply(posthoc_out, is.numeric, logical(1))]
  num_cols <- setdiff(num_cols, c("p.value", "p.value."))
  posthoc_out[num_cols] <- lapply(posthoc_out[num_cols], function(x) round(x, 4))
  
  list(set = which_set, metric = metric_name, omnibus = omnibus, posthoc_out = posthoc_out)
}

metrics_to_test <- unique(fold_metrics_tt$.metric)

results_train <- metrics_to_test %>%
  purrr::set_names() %>%
  purrr::map(~ run_anova_posthoc_F(.x, fold_metrics_tt, which_set = "train", df_method = "Satterthwaite"))

results_test <- metrics_to_test %>%
  purrr::set_names() %>%
  purrr::map(~ run_anova_posthoc_F(.x, fold_metrics_tt, which_set = "test", df_method = "Satterthwaite"))

options(width = 200)
options(tibble.print_max = Inf, tibble.print_min = Inf)
options(dplyr.width = Inf)

## =========================================================
## 1) (train/test) omnibus 결과를 자동 감지해서 1개 표로 합치기
##    - F-test면: df1/df2/F/p
##    - LRT면: df1(=Df)/Chisq/p
## =========================================================
pick_first <- function(nms, patterns) {
  for (pat in patterns) {
    hit <- nms[grepl(pat, nms, ignore.case = TRUE)]
    if (length(hit) > 0) return(hit[1])
  }
  NA_character_
}

omnibus_to_row <- function(omnibus_obj, set_name, metric_name) {
  a <- as.data.frame(omnibus_obj)
  nms <- names(a)
  
  # ---- Case A: LRT (Chisq) ----
  if (any(grepl("Pr\\(>Chisq\\)", nms, ignore.case = TRUE))) {
    chisq_col <- pick_first(nms, c("^Chisq$"))
    df_col    <- pick_first(nms, c("^Df$"))
    p_col     <- pick_first(nms, c("Pr\\(>Chisq\\)"))
    
    # Chisq가 NA가 아닌 행(보통 full model)을 선택
    idx <- which(!is.na(a[[chisq_col]]))
    if (length(idx) == 0) idx <- nrow(a)
    row <- a[idx[1], , drop = FALSE]
    
    return(tibble::tibble(
      set       = set_name,
      metric    = metric_name,
      test_type = "LRT(Chisq)",
      df1       = as.numeric(row[[df_col]]),
      df2       = NA_real_,
      stat      = as.numeric(row[[chisq_col]]),
      p_value   = as.numeric(row[[p_col]])
    ))
  }
  
  # ---- Case B: F-test (lmerTest/stats::anova 결과) ----
  if (any(grepl("Pr\\(>F\\)", nms, ignore.case = TRUE))) {
    a$term <- rownames(a)
    idx <- which(grepl("^model$|model", a$term, ignore.case = TRUE))
    if (length(idx) == 0) idx <- 1L
    row <- a[idx[1], , drop = FALSE]
    
    numdf_col <- pick_first(nms, c("^NumDF$", "numdf", "^Df$"))
    dendf_col <- pick_first(nms, c("^DenDF$", "dendf", "ddf"))
    f_col     <- pick_first(nms, c("^F", "F value", "F\\.value"))
    p_col     <- pick_first(nms, c("Pr\\(>F\\)"))
    
    return(tibble::tibble(
      set       = set_name,
      metric    = metric_name,
      test_type = "F-test",
      df1       = as.numeric(row[[numdf_col]]),
      df2       = as.numeric(row[[dendf_col]]),
      stat      = as.numeric(row[[f_col]]),
      p_value   = as.numeric(row[[p_col]])
    ))
  }
  
  # ---- Fallback ----
  tibble::tibble(
    set       = set_name,
    metric    = metric_name,
    test_type = NA_character_,
    df1       = NA_real_,
    df2       = NA_real_,
    stat      = NA_real_,
    p_value   = NA_real_
  )
}

anova_table <- dplyr::bind_rows(
  purrr::imap_dfr(results_train, ~ omnibus_to_row(.x$omnibus, "train", .y)),
  purrr::imap_dfr(results_test,  ~ omnibus_to_row(.x$omnibus, "test",  .y))
) %>%
  dplyr::mutate(
    stat    = round(stat, 4),
    p_value = signif(p_value, 4)
  ) %>%
  dplyr::arrange(metric, set)

cat("\n\n==============================\nOMNIBUS TESTS (TRAIN + TEST)\n==============================\n")
anova_table_disp <- anova_table %>%
  dplyr::mutate(
    stat = ifelse(is.na(stat), NA_character_, formatC(stat, format = "f", digits = 4)),
    p_value = ifelse(is.na(p_value), NA_character_, format.pval(p_value, digits = 4, eps = 1e-300))
  )

print(anova_table_disp, n = Inf, width = Inf)

## =========================================================
## 2) (선택) Tukey post-hoc도 한 표로 모아서 콘솔에 출력
## =========================================================
posthoc_table <- dplyr::bind_rows(
  purrr::imap_dfr(results_train, ~ dplyr::mutate(tibble::as_tibble(.x$posthoc_out), set = "train", metric = .y)),
  purrr::imap_dfr(results_test,  ~ dplyr::mutate(tibble::as_tibble(.x$posthoc_out), set = "test",  metric = .y))
) %>%
  dplyr::select(set, metric, dplyr::everything())

cat("\n\n==============================\nPOST-HOC (TUKEY) (TRAIN + TEST)\n==============================\n")
print(posthoc_table, n = Inf, width = Inf)
## =========================================================
## 10) Boxplots (TEST set, 100 resamples) - 5개 그림
## =========================================================

plot_df <- fold_metrics_tt %>%
  dplyr::filter(set == "test") %>%
  dplyr::mutate( model = factor(model, levels = c("ElasticNet", "XGBoost", "MLP")), .metric = as.character(.metric) )
metrics_5 <- c("accuracy", "precision", "recall", "f_meas", "mn_log_loss")
metrics_5 <- metrics_5[metrics_5 %in% unique(plot_df$.metric)]

for (m in metrics_5) {
  p <- ggplot2::ggplot(
    plot_df %>% dplyr::filter(.metric == m),
    ggplot2::aes(x = model, y = .estimate, fill = model)
  ) +
    ggplot2::geom_boxplot(width = 0.65, outlier.shape = NA, alpha = 0.85,staplewidth = 0.5) +
    ggplot2::scale_fill_manual(values = c(ElasticNet = "#4C78A8", XGBoost = "#F58518", MLP = "#54A24B")) +
    ggplot2::labs(
      title = paste0("Test-set ", m, " (5-fold × 20 repeats = 100)"),
      x = "Model",
      y = m
    ) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(face = "bold"),
      axis.line  = ggplot2::element_line(linewidth = 0.6),
      axis.ticks = ggplot2::element_line(linewidth = 0.6),
      axis.text  = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 11)
    )
  
  if (m == "mn_log_loss") {
    p <- p +
      ggplot2::coord_cartesian(ylim = c(0.1, 0.75)) +
      ggplot2::scale_y_continuous(breaks = seq(0.1, 0.7, by = 0.2))
  } else {
    p <- p +
      ggplot2::coord_cartesian(ylim = c(0.68, 1.0)) +
      ggplot2::scale_y_continuous(breaks = seq(0.7, 1.0, by = 0.1))
  }
  
  print(p)
}

##################################################################### histogram ################################################

plot_df <- fold_metrics_tt %>%
  dplyr::filter(set == "test") %>%
  dplyr::mutate(
    model = factor(model, levels = c("ElasticNet", "XGBoost", "MLP")),
    .metric = as.character(.metric)
  )

metrics_5 <- c("accuracy", "precision", "recall", "f_meas", "mn_log_loss")
metrics_5 <- metrics_5[metrics_5 %in% unique(plot_df$.metric)]

for (m in metrics_5) {
  
  summary_df <- plot_df %>%
    dplyr::filter(.metric == m) %>%
    dplyr::group_by(model) %>%
    dplyr::summarise(
      n          = sum(!is.na(.estimate)),
      mean_value = mean(.estimate, na.rm = TRUE),
      sem_value  = sd(.estimate, na.rm = TRUE) / sqrt(n),
      upper      = mean_value + sem_value,
      .groups = "drop"
    )
  
  p <- ggplot2::ggplot(
    summary_df,
    ggplot2::aes(x = model, y = mean_value, fill = model)
  ) +
    ggplot2::geom_col(
      width = 0.65,
      alpha = 0.85,
      color = "black",
      linewidth = 0.6
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean_value, ymax = upper),
      width = 0.2,
      linewidth = 0.6
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        ElasticNet = "#4E79A7",
        XGBoost = "#E15759",
        MLP = "#59A14F"
      )) +
    ggplot2::labs(
      title = paste0("Test-set ", m, " (5-fold × 20 repeats = 100)"),
      x = "Model",
      y = m
    ) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(face = "bold"),
      axis.line  = ggplot2::element_line(linewidth = 1.0),
      axis.ticks = ggplot2::element_line(linewidth = 1.0),
      axis.text  = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 11)
    )
  
  if (m == "mn_log_loss") {
    p <- p +
      ggplot2::coord_cartesian(ylim = c(0.03, 0.65)) +
      ggplot2::scale_y_continuous(breaks = seq(0.0, 0.6, by = 0.2))
  } else if (m == "accuracy") {
    p <- p +
      ggplot2::coord_cartesian(ylim = c(0.52, 1.0)) +
      ggplot2::scale_y_continuous(breaks = seq(0.5, 1.0, by = 0.1))
  } else {
    p <- p +
      ggplot2::coord_cartesian(ylim = c(0.52, 1.0)) +
      ggplot2::scale_y_continuous(breaks = seq(0.5, 1.0, by = 0.1))
  }
  
  print(p)
}

####################################################################################################################################
############################################################
# Sequential mediation SEM (lavaan) with bootstrap CI
# Class -> (PD, DD, GD) -> (Integrating, Compromising) -> CoupleSatisfaction
############################################################
# 
# # 2) 필수 변수 존재/결측 점검
# vars_needed <- c("Class", "PD", "DD", "GD", "Integrating", "Compromising", "CoupleSatisfaction")
# missing_vars <- setdiff(vars_needed, names(selected_score_lpa))
# if (length(missing_vars) > 0) stop(paste("데이터에 다음 변수가 없습니다:", paste(missing_vars, collapse = ", ")))
# 
# # Class를 factor로 변환 (레벨 순서: 1,2,3 가정)
# selected_score_lpa <- selected_score_lpa %>%
#   mutate(
#     Class = as.factor(Class)
#   )
# 
# # 레벨이 1/2/3이 아닐 수도 있으니 확인
# if (nlevels(selected_score_lpa$Class) != 3) {
#   stop(paste0("Class 레벨이 3개가 아닙니다. 현재 레벨 수: ", nlevels(selected_score_lpa$Class),
#               " / 레벨: ", paste(levels(selected_score_lpa$Class), collapse = ", ")))
# }
# 
# # 3) 기준집단 설정: 보통 가장 취약/낮은 집단을 기준으로 둡니다.
# # 사용자님 결과 패턴상 Group 2가 취약형에 가까웠으므로 기본값으로 2를 기준으로 둡니다.
# # 필요하면 ref_class를 "1" 또는 "3"으로 바꾸세요.
# ref_class <- "2"
# if (!(ref_class %in% levels(selected_score_lpa$Class))) stop("ref_class가 Class 레벨에 없습니다.")
# 
# selected_score_lpa <- selected_score_lpa %>%
#   mutate(Class = relevel(Class, ref = ref_class))
# 
# # 더미 생성: 기준집단(ref_class) 대비 나머지 두 집단
# # 예: ref=2이면 D1= (Class==1), D3=(Class==3)
# levs <- levels(selected_score_lpa$Class)              # c("2","1","3") 같은 형태일 수 있음(기준이 첫 레벨)
# other_levs <- setdiff(levs, ref_class)
# 
# # 더미 변수명 자동 생성
# dname1 <- paste0("D_", other_levs[1])
# dname2 <- paste0("D_", other_levs[2])
# 
# selected_score_lpa[[dname1]] <- ifelse(selected_score_lpa$Class == other_levs[1], 1, 0)
# selected_score_lpa[[dname2]] <- ifelse(selected_score_lpa$Class == other_levs[2], 1, 0)
# 
# # 4) (권장) 관찰변수 표준화 여부: 선택사항
# # 해석을 표준화계수로 할 거면 굳이 z변환 안 하셔도 됩니다(standardized=TRUE로 제공 가능).
# # 아래는 원점수 그대로 진행합니다.
# 
# # 5) lavaan 모형 정의
# # ActiveAcceptance: 3지표 잠재변수 (식별: PD의 loading=1)
# # PositiveConflict: 2지표 잠재변수 (안정성 위해 두 loading을 1로 고정해 "동일가중" 요인으로 둠)
# # 구조: D_* -> ActiveAcceptance -> PositiveConflict -> CoupleSatisfaction
# #      또한 D_* -> PositiveConflict, D_* -> CoupleSatisfaction 직접경로를 포함(부분매개; 필요 시 제거 가능)
# 
# model_seq <- paste0("
#   # ---------------------------
#   # Measurement model
#   # ---------------------------
#   ActiveAcc =~ 1*PD + DD + GD
#   PosConflict =~ 1*Integrating + 1*Compromising
# 
#   # ---------------------------
#   # Structural model
#   # ---------------------------
#   ActiveAcc ~ a1*", dname1, " + a2*", dname2, "
#   PosConflict ~ b*ActiveAcc + d1*", dname1, " + d2*", dname2, "
#   CoupleSatisfaction ~ c*PosConflict + e*ActiveAcc + f1*", dname1, " + f2*", dname2, "
# 
#   # ---------------------------
#   # Indirect effects (sequential)
#   # D -> ActiveAcc -> PosConflict -> Satisfaction
#   # ---------------------------
#   ind_seq_", dname1, " := a1*b*c
#   ind_seq_", dname2, " := a2*b*c
# 
#   # Optional: D -> ActiveAcc -> Satisfaction (if e path retained)
#   ind_AA_", dname1, " := a1*e
#   ind_AA_", dname2, " := a2*e
# 
#   # Optional: D -> PosConflict -> Satisfaction (if d paths retained)
#   ind_PC_", dname1, " := d1*c
#   ind_PC_", dname2, " := d2*c
# 
#   # Total effects
#   tot_", dname1, " := f1 + ind_seq_", dname1, " + ind_AA_", dname1, " + ind_PC_", dname1, "
#   tot_", dname2, " := f2 + ind_seq_", dname2, " + ind_AA_", dname2, " + ind_PC_", dname2, "
# ")
# 
# # 6) 모형 적합 (부트스트랩 CI)
# set.seed(1234)
# fit_seq <- sem(
#   model_seq,
#   data = selected_score_lpa,
#   estimator = "ML",
#   missing = "fiml",
#   se = "bootstrap",
#   bootstrap = 5000
# )
# 
# # 7) 결과 출력: 적합도 + 표준화 추정치
# summary(fit_seq, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
# 
# # 8) 간접효과/총효과만 깔끔하게 (percentile bootstrap CI)
# pe <- parameterEstimates(fit_seq, standardized = TRUE, ci = TRUE, level = 0.95)
# pe_ind <- pe %>%
#   dplyr::filter(grepl("^ind_|^tot_", label)) %>%
#   dplyr::select(label, est, se, z, pvalue, ci.lower, ci.upper, std.all)
# print(pe_ind)
# 
# # 9) (선택) BCa CI (lavaan 버전에 따라 boot.ci.type 지원)
# # 지원되면 아래가 가장 좋은 CI입니다.
# pe_bca <- try(parameterEstimates(fit_seq, standardized = TRUE, ci = TRUE, level = 0.95,
#                                  boot.ci.type = "bca.simple"), silent = TRUE)
# if (!inherits(pe_bca, "try-error")) {
#   pe_ind_bca <- pe_bca %>%
#     dplyr::filter(grepl("^ind_|^tot_", label)) %>%
#     dplyr::select(label, est, se, z, pvalue, ci.lower, ci.upper, std.all)
#   print(pe_ind_bca)
# } else {
#   message("현재 lavaan/버전 환경에서 bca.simple CI가 지원되지 않거나 에러가 발생했습니다. percentile CI 결과를 사용하세요.")
# }
# 
# # 10) (권장) 대안 모형: PositiveConflict를 잠재변수 대신 관찰 합산/평균으로 두고 더 단순하게
# # 2지표 요인이 마음에 걸리면 이 버전이 더 안정적일 수 있습니다.
# selected_score_lpa <- selected_score_lpa %>% mutate(PosConflict_obs = (Integrating + Compromising)/2)
# 
# model_seq_obs <- paste0("
#   ActiveAcc =~ 1*PD + DD + GD
# 
#   PosConflict_obs ~ b*ActiveAcc + d1*", dname1, " + d2*", dname2, "
#   CoupleSatisfaction ~ c*PosConflict_obs + e*ActiveAcc + f1*", dname1, " + f2*", dname2, "
# 
#   ind_seq_", dname1, " := a1*b*c
#   ind_seq_", dname2, " := a2*b*c
#   ActiveAcc ~ a1*", dname1, " + a2*", dname2, "
# 
#   ind_AA_", dname1, " := a1*e
#   ind_AA_", dname2, " := a2*e
#   ind_PC_", dname1, " := d1*c
#   ind_PC_", dname2, " := d2*c
# 
#   tot_", dname1, " := f1 + ind_seq_", dname1, " + ind_AA_", dname1, " + ind_PC_", dname1, "
#   tot_", dname2, " := f2 + ind_seq_", dname2, " + ind_AA_", dname2, " + ind_PC_", dname2, "
# ")
# 
# set.seed(1234)
# fit_seq_obs <- sem(
#   model_seq_obs,
#   data = selected_score_lpa,
#   estimator = "ML",
#   missing = "fiml",
#   se = "bootstrap",
#   bootstrap = 5000
# )
# 
# summary(fit_seq_obs, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
# 
# pe2 <- parameterEstimates(fit_seq_obs, standardized = TRUE, ci = TRUE, level = 0.95)
# pe2_ind <- pe2 %>%
#   dplyr::filter(grepl("^ind_|^tot_", label)) %>%
#   dplyr::select(label, est, se, z, pvalue, ci.lower, ci.upper, std.all)
# print(pe2_ind)

