# Couple LPA Analysis and Latent Profile Prediction
# GitHub-ready reproducible version

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tidyLPA)
  library(ggplot2)
  library(reshape2)
  library(survey)
  library(effectsize)
  library(broom)
  library(tidymodels)
  library(glmnet)
  library(xgboost)
  library(nnet)
  library(lme4)
  library(lmerTest)
  library(emmeans)
  library(tibble)
  library(purrr)
  library(vcd)
})

tidymodels::tidymodels_prefer()
options(survey.lonely.psu = "adjust")

# -----------------------------------------------------------------------------
# Paths
# -----------------------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x
script_path <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
repo_root <- if (!is.na(script_path)) normalizePath(file.path(dirname(script_path), ".."), mustWork = FALSE) else getwd()
if (!dir.exists(file.path(repo_root, "data"))) repo_root <- getwd()

data_path <- file.path(repo_root, "data", "couple_raw_data_n300_20260219_public.csv")
fig_dir   <- file.path(repo_root, "figures")
res_dir   <- file.path(repo_root, "results")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(res_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# Load data
# -----------------------------------------------------------------------------
raw_data <- read.csv(data_path, header = TRUE, stringsAsFactors = FALSE, na.strings = c("", "NA", "#NULL!"))

meta_vars <- c("Gender", "Age_2", "Marital", "Parental", "Education", "Religion", "Occupation", "CoupleStatus", "Income", "CouplePeriod", "NumCouple", "NumCouple_2")
score_vars <- c("ADCRS", "TD", "PD", "RD", "GD", "DD", "Extraversion", "Neuroticism", "Openness", "Agreeableness", "Conscientiousness", "Compromising", "Integrating", "Obliging", "Avoiding", "Dominating", "PositiveStrategy", "NegativeStrategy", "CoupleSatisfaction")
big5_vars <- c("Extraversion", "Neuroticism", "Openness", "Agreeableness", "Conscientiousness")

selected_meta <- raw_data %>% select(any_of(meta_vars))
selected_score <- raw_data %>% select(all_of(score_vars)) %>% mutate(across(everything(), as.numeric))
selected_score_z <- selected_score %>% transmute(across(everything(), ~ as.numeric(scale(.x)), .names = "{.col}_z"))

# -----------------------------------------------------------------------------
# Descriptive summaries
# -----------------------------------------------------------------------------
desc_scores <- selected_score %>% summarise(across(everything(), list(n = ~sum(!is.na(.x)), mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE), min = ~min(.x, na.rm = TRUE), max = ~max(.x, na.rm = TRUE))))
write.csv(desc_scores, file.path(res_dir, "descriptive_score_summary.csv"), row.names = FALSE)

meta_summary <- selected_meta %>% summarise(across(everything(), ~ paste(names(table(.x, useNA = "ifany")), as.integer(table(.x, useNA = "ifany")), sep = ":", collapse = "; ")))
write.csv(meta_summary, file.path(res_dir, "descriptive_meta_summary.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# Figures: Big Five boxplots and correlation heatmap
# -----------------------------------------------------------------------------
p_big5_raw <- selected_score %>%
  select(all_of(big5_vars)) %>%
  pivot_longer(everything(), names_to = "Trait", values_to = "Score") %>%
  ggplot(aes(x = Trait, y = Score)) +
  geom_boxplot() +
  labs(title = "Big Five Personality Traits", x = NULL, y = "Raw score") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(fig_dir, "big5_raw_boxplot.png"), p_big5_raw, width = 7, height = 5, dpi = 300)

p_big5_z <- selected_score_z %>%
  select(paste0(big5_vars, "_z")) %>%
  pivot_longer(everything(), names_to = "Trait", values_to = "Score") %>%
  mutate(Trait = sub("_z$", "", Trait)) %>%
  ggplot(aes(x = Trait, y = Score)) +
  geom_boxplot() +
  labs(title = "Standardized Big Five Personality Traits", x = NULL, y = "Z-score") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(fig_dir, "big5_z_boxplot.png"), p_big5_z, width = 7, height = 5, dpi = 300)

cor_mat <- round(cor(selected_score_z, use = "pairwise.complete.obs"), 2)
cor_mat[upper.tri(cor_mat)] <- NA
cor_df <- reshape2::melt(cor_mat, na.rm = TRUE)

p_cor <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), size = 2.5) +
  scale_fill_gradient2(low = "#abd9e9", mid = "#FFFFFF", high = "#FF0000", midpoint = 0, name = "Correlation") +
  labs(title = "Pearson Correlation Matrix", x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(fig_dir, "correlation_heatmap.png"), p_cor, width = 10, height = 8, dpi = 300)

# -----------------------------------------------------------------------------
# Demographic regrouping
# -----------------------------------------------------------------------------
selected_meta_regroup <- selected_meta %>%
  mutate(
    Marital = case_when(Marital %in% c("1", "3", "4", 1, 3, 4) ~ 1, Marital %in% c("2", 2) ~ 2, TRUE ~ NA_real_),
    Parental = case_when(Parental %in% c("1", 1) ~ 1, Parental %in% c("2", "3", "4", 2, 3, 4) ~ 2, TRUE ~ NA_real_),
    Education = case_when(Education %in% c("2", "3", 2, 3) ~ 1, Education %in% c("4", 4) ~ 2, Education %in% c("5", 5) ~ 3, TRUE ~ NA_real_),
    Religion = case_when(Religion %in% c("1", 1) ~ 1, Religion %in% c("2", "3", "4", "5", 2, 3, 4, 5) ~ 2, TRUE ~ NA_real_),
    Occupation = case_when(Occupation %in% c("1", 1) ~ 1, Occupation %in% c("2", "5", 2, 5) ~ 2, Occupation %in% c("3", "4", 3, 4) ~ 3, Occupation %in% c("6", "8", 6, 8) ~ 4, Occupation %in% c("7", 7) ~ 5, TRUE ~ NA_real_),
    Income = case_when(Income %in% c("1", "2", 1, 2) ~ 1, Income %in% c("3", "4", 3, 4) ~ 2, Income %in% c("5", 5) ~ 3, TRUE ~ NA_real_),
    CouplePeriod = case_when(CouplePeriod %in% c("1", "2", "3", 1, 2, 3) ~ 1, CouplePeriod %in% c("4", "5", "6", "7", 4, 5, 6, 7) ~ 2, CouplePeriod %in% c("8", 8) ~ 3, CouplePeriod %in% c("9", "10", 9, 10) ~ 4, TRUE ~ NA_real_),
    NumCouple = case_when(NumCouple %in% c("1", "2", 1, 2) ~ 1, NumCouple %in% c("3", "4", 3, 4) ~ 2, NumCouple %in% c("5", 5) ~ 3, TRUE ~ NA_real_)
  )

# -----------------------------------------------------------------------------
# Latent Profile Analysis
# -----------------------------------------------------------------------------
lpa_input <- selected_score_z %>% select(paste0(big5_vars, "_z"))

lpa_models <- estimate_profiles(lpa_input, n_profiles = 1:9, variances = "varying", covariances = "zero")
lpa_comparison <- compare_solutions(lpa_models)
write.csv(lpa_comparison$fits, file.path(res_dir, "lpa_model_fit_comparison.csv"), row.names = FALSE)

lpa_3 <- estimate_profiles(lpa_input, n_profiles = 3, variances = "varying", covariances = "zero")
lpa_4 <- estimate_profiles(lpa_input, n_profiles = 4, variances = "varying", covariances = "zero")

saveRDS(lpa_3, file.path(res_dir, "lpa_3_profile_solution.rds"))
write.csv(get_estimates(lpa_3), file.path(res_dir, "lpa_3_profile_estimates.csv"), row.names = FALSE)

lpa_group <- get_data(lpa_3)
selected_score_z_lpa <- selected_score_z %>% mutate(Class = factor(lpa_group$Class))
selected_score_lpa <- selected_score %>% mutate(Class = factor(lpa_group$Class))
selected_meta_lpa <- selected_meta_regroup %>% mutate(Class = factor(lpa_group$Class))

p_lpa <- plot_profiles(lpa_3, add_line = TRUE, ci = NULL, sd = FALSE, rawdata = FALSE) +
  theme_classic() +
  scale_y_continuous(limits = c(-1.0, 1.0), breaks = seq(-1, 1, 0.25)) +
  labs(title = "Three-profile LPA Solution")

ggsave(file.path(fig_dir, "lpa_3_profile_plot.png"), p_lpa, width = 7, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# Class-wise descriptive statistics
# -----------------------------------------------------------------------------
desc_by_class <- function(dat) {
  dat %>%
    pivot_longer(cols = -Class, names_to = "Variable", values_to = "Value") %>%
    group_by(Variable, Class) %>%
    summarise(N = sum(!is.na(Value)), Mean = mean(Value, na.rm = TRUE), SD = sd(Value, na.rm = TRUE), Min = min(Value, na.rm = TRUE), Median = median(Value, na.rm = TRUE), Max = max(Value, na.rm = TRUE), .groups = "drop")
}

write.csv(desc_by_class(selected_score_z_lpa), file.path(res_dir, "class_descriptives_z.csv"), row.names = FALSE)
write.csv(desc_by_class(selected_score_lpa), file.path(res_dir, "class_descriptives_raw.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# ANOVA and Scheffe-style post-hoc comparisons
# -----------------------------------------------------------------------------
run_anova_scheffe <- function(dat, vars) {
  anova_results <- data.frame()
  scheffe_results <- data.frame()
  dat$Class <- factor(dat$Class)
  for (var in vars) {
    fml <- as.formula(paste0("`", var, "` ~ Class"))
    aov_model <- aov(fml, data = dat)
    s <- summary(aov_model)[[1]]
    eta2 <- effectsize::eta_squared(aov_model, partial = FALSE)$Eta2[1]
    anova_results <- rbind(anova_results, data.frame(Variable = var, F_value = s$`F value`[1], df1 = s$Df[1], df2 = s$Df[2], p_value = s$`Pr(>F)`[1], eta2 = eta2))
    group_means <- as.numeric(tapply(dat[[var]], dat$Class, mean, na.rm = TRUE))
    n_group <- as.numeric(table(dat$Class))
    k <- length(n_group)
    MSerror <- s$`Mean Sq`[2]
    df_error <- s$Df[2]
    for (i in 1:(k - 1)) {
      for (j in (i + 1):k) {
        mean_diff <- group_means[i] - group_means[j]
        se_diff <- sqrt(MSerror * (1 / n_group[i] + 1 / n_group[j]))
        F_crit <- qf(0.95, df1 = k - 1, df2 = df_error)
        scheffe_crit <- sqrt((k - 1) * F_crit) * se_diff
        p_val <- 1 - pf((mean_diff^2) / (se_diff^2 * (1 / (k - 1))), k - 1, df_error)
        scheffe_results <- rbind(scheffe_results, data.frame(Variable = var, Comparison = paste0("Profile ", i, " - Profile ", j), Difference = mean_diff, Lower_CI = mean_diff - scheffe_crit, Upper_CI = mean_diff + scheffe_crit, p_value = p_val))
      }
    }
  }
  list(anova = anova_results, posthoc = scheffe_results)
}

anova_z <- run_anova_scheffe(selected_score_z_lpa, names(selected_score_z))
anova_raw <- run_anova_scheffe(selected_score_lpa, names(selected_score))
write.csv(anova_z$anova, file.path(res_dir, "anova_by_class_z.csv"), row.names = FALSE)
write.csv(anova_z$posthoc, file.path(res_dir, "scheffe_posthoc_by_class_z.csv"), row.names = FALSE)
write.csv(anova_raw$anova, file.path(res_dir, "anova_by_class_raw.csv"), row.names = FALSE)
write.csv(anova_raw$posthoc, file.path(res_dir, "scheffe_posthoc_by_class_raw.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# Chi-square tests for demographic differences by profile
# -----------------------------------------------------------------------------
chi_vars <- c("Gender", "Age_2", "Marital", "Parental", "Education", "Religion", "Occupation", "Income", "CoupleStatus", "CouplePeriod", "NumCouple")
chi_table <- purrr::map_dfr(chi_vars, function(v) {
  tbl <- table(selected_meta_lpa[[v]], selected_meta_lpa$Class)
  chi <- suppressWarnings(chisq.test(tbl))
  tibble(Variable = v, Chi_Square = unname(chi$statistic), df = unname(chi$parameter), p_value = chi$p.value, Cramers_V = vcd::assocstats(tbl)$cramer)
})
write.csv(chi_table, file.path(res_dir, "chi_square_demographics_by_class.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# BCH-style Wald comparisons using posterior probabilities
# -----------------------------------------------------------------------------
wald_bch_with_mean_sd <- function(posterior_probs, ext, var_list = NULL) {
  if (!is.matrix(posterior_probs)) posterior_probs <- as.matrix(posterior_probs)
  n <- nrow(posterior_probs)
  K <- ncol(posterior_probs)
  if (nrow(ext) != n) stop("The number of rows in ext and posterior_probs must match.")
  if (is.null(var_list)) var_list <- names(ext)
  var_list <- var_list[sapply(ext[var_list], is.numeric)]
  id_long <- rep(seq_len(n), times = K)
  Class_long <- factor(rep(seq_len(K), each = n), levels = seq_len(K))
  w_long <- as.vector(posterior_probs)
  long_df <- ext[id_long, var_list, drop = FALSE]
  long_df$id <- id_long
  long_df$Class <- Class_long
  long_df$w <- w_long
  des <- svydesign(ids = ~id, weights = ~w, data = long_df)
  desc_tbl <- data.frame()
  omnibus_tbl <- data.frame()
  pairwise_tbl <- data.frame()
  for (v in var_list) {
    mean_vec <- sd_vec <- n_eff <- rep(NA_real_, K)
    for (k in 1:K) {
      des_k <- subset(des, Class == k)
      f <- as.formula(paste0("~`", v, "`"))
      m_k <- try(svymean(f, design = des_k, na.rm = TRUE), silent = TRUE)
      v_k <- try(svyvar(f, design = des_k, na.rm = TRUE), silent = TRUE)
      if (!inherits(m_k, "try-error")) mean_vec[k] <- as.numeric(coef(m_k))
      if (!inherits(v_k, "try-error")) sd_vec[k] <- sqrt(max(as.numeric(coef(v_k)), 0))
      n_eff[k] <- sum(long_df$w[long_df$Class == k], na.rm = TRUE)
    }
    desc_tbl <- rbind(desc_tbl, data.frame(Variable = v, Profile = seq_len(K), N_eff = n_eff, Mean = mean_vec, SD = sd_vec))
    fit <- try(svyglm(as.formula(paste0("`", v, "` ~ 0 + Class")), design = des), silent = TRUE)
    if (inherits(fit, "try-error")) next
    b <- coef(fit); V <- vcov(fit); cn <- paste0("Class", seq_len(K))
    if (!all(cn %in% names(b))) next
    b <- b[cn]; V <- V[cn, cn, drop = FALSE]
    L <- cbind(-1, diag(K - 1))
    Lb <- as.matrix(L %*% b); LVLt <- L %*% V %*% t(L)
    W_omni <- as.numeric(t(Lb) %*% qr.solve(LVLt, Lb))
    omnibus_tbl <- rbind(omnibus_tbl, data.frame(Variable = v, Wald_Chi2 = W_omni, df = K - 1, p_value = pchisq(W_omni, df = K - 1, lower.tail = FALSE)))
    for (i in 1:(K - 1)) {
      for (j in (i + 1):K) {
        cvec <- rep(0, K); cvec[i] <- 1; cvec[j] <- -1
        est <- as.numeric(sum(cvec * b)); varc <- as.numeric(t(cvec) %*% V %*% cvec)
        if (is.na(varc) || varc <= 0) next
        W_pair <- (est^2) / varc
        pairwise_tbl <- rbind(pairwise_tbl, data.frame(Variable = v, Contrast = paste0("Profile ", i, " vs ", j), Mean_i = mean_vec[i], SD_i = sd_vec[i], Mean_j = mean_vec[j], SD_j = sd_vec[j], Estimate = est, Wald_Chi2 = W_pair, df = 1, p_value = pchisq(W_pair, df = 1, lower.tail = FALSE)))
      }
    }
  }
  list(desc = desc_tbl, omnibus = omnibus_tbl, pairwise = pairwise_tbl)
}

posterior_probs <- lpa_3$model_2_class_3$model$z
bch_z <- wald_bch_with_mean_sd(posterior_probs, selected_score_z)
bch_raw <- wald_bch_with_mean_sd(posterior_probs, selected_score)
write.csv(bch_z$desc, file.path(res_dir, "bch_descriptives_z.csv"), row.names = FALSE)
write.csv(bch_z$omnibus, file.path(res_dir, "bch_omnibus_z.csv"), row.names = FALSE)
write.csv(bch_z$pairwise, file.path(res_dir, "bch_pairwise_z.csv"), row.names = FALSE)
write.csv(bch_raw$desc, file.path(res_dir, "bch_descriptives_raw.csv"), row.names = FALSE)
write.csv(bch_raw$omnibus, file.path(res_dir, "bch_omnibus_raw.csv"), row.names = FALSE)
write.csv(bch_raw$pairwise, file.path(res_dir, "bch_pairwise_raw.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# Prediction: repeated 5-fold CV, Elastic Net, XGBoost, and MLP
# -----------------------------------------------------------------------------
set.seed(20260224)
pred_data <- selected_score_z_lpa %>% select(paste0(big5_vars, "_z"), Class) %>% filter(!is.na(Class))
ref_levels <- levels(pred_data$Class)

folds <- rsample::vfold_cv(pred_data, v = 5, repeats = 20, strata = Class) %>% mutate(resample_id = paste0("rep", id2, "_fold", id))

rec <- recipes::recipe(Class ~ ., data = pred_data) %>%
  recipes::step_impute_median(recipes::all_numeric_predictors()) %>%
  recipes::step_zv(recipes::all_predictors()) %>%
  recipes::step_normalize(recipes::all_numeric_predictors())

enet_spec <- parsnip::multinom_reg(penalty = 0.01, mixture = 0.50) %>% set_engine("glmnet")
xgb_spec <- parsnip::boost_tree(trees = 800, tree_depth = 6, learn_rate = 0.05, loss_reduction = 0.0, min_n = 10, sample_size = 0.8, mtry = 0.8) %>% set_engine("xgboost", objective = "multi:softprob", num_class = 3, eval_metric = "mlogloss", counts = FALSE) %>% set_mode("classification")
mlp_spec <- parsnip::mlp(hidden_units = 16, penalty = 0.01, epochs = 300) %>% set_engine("nnet", MaxNWts = 20000, trace = FALSE) %>% set_mode("classification")

workflows <- list(
  ElasticNet = workflow() %>% add_recipe(rec) %>% add_model(enet_spec),
  XGBoost = workflow() %>% add_recipe(rec) %>% add_model(xgb_spec),
  MLP = workflow() %>% add_recipe(rec) %>% add_model(mlp_spec)
)

metric_class <- yardstick::metric_set(accuracy, precision, recall, f_meas)
metric_prob <- yardstick::metric_set(mn_log_loss)

calc_metrics <- function(pred_df) {
  prob_cols <- setdiff(grep("^\\.pred_", names(pred_df), value = TRUE), ".pred_class")
  bind_rows(
    metric_class(pred_df, truth = Class, estimate = .pred_class, estimator = "macro"),
    metric_prob(pred_df, truth = Class, !!!rlang::syms(prob_cols))
  )
}

compute_fold_metrics <- function(wf, model_name) {
  map2_dfr(folds$splits, folds$resample_id, function(split, resample_id) {
    train_dat <- rsample::analysis(split) %>% mutate(Class = factor(Class, levels = ref_levels))
    test_dat <- rsample::assessment(split) %>% mutate(Class = factor(Class, levels = ref_levels))
    fit_wf <- fit(wf, data = train_dat)
    train_preds <- bind_cols(train_dat %>% select(Class), predict(fit_wf, train_dat, type = "class"), predict(fit_wf, train_dat, type = "prob"))
    test_preds <- bind_cols(test_dat %>% select(Class), predict(fit_wf, test_dat, type = "class"), predict(fit_wf, test_dat, type = "prob"))
    bind_rows(calc_metrics(train_preds) %>% mutate(set = "train"), calc_metrics(test_preds) %>% mutate(set = "test")) %>%
      mutate(resample = resample_id, model = model_name) %>% select(resample, set, model, .metric, .estimate)
  })
}

fold_metrics <- imap_dfr(workflows, ~ compute_fold_metrics(.x, .y)) %>% mutate(model = factor(model, levels = c("ElasticNet", "XGBoost", "MLP")), set = factor(set, levels = c("train", "test")))
write.csv(fold_metrics, file.path(res_dir, "prediction_fold_metrics_train_test.csv"), row.names = FALSE)

metric_summary <- fold_metrics %>% group_by(set, model, .metric) %>% summarise(mean = mean(.estimate, na.rm = TRUE), sd = sd(.estimate, na.rm = TRUE), .groups = "drop")
write.csv(metric_summary, file.path(res_dir, "prediction_metric_summary_train_test.csv"), row.names = FALSE)

run_anova_posthoc <- function(metric_name, which_set) {
  dat <- fold_metrics %>% filter(set == which_set, .metric == metric_name)
  fit <- lmerTest::lmer(.estimate ~ model + (1 | resample), data = dat, REML = TRUE)
  omnibus <- as.data.frame(stats::anova(fit, ddf = "Satterthwaite"))
  omnibus$metric <- metric_name
  omnibus$set <- which_set
  omnibus$term <- rownames(omnibus)
  emm <- emmeans::emmeans(fit, ~ model, lmer.df = "satterthwaite")
  posthoc <- as.data.frame(confint(emmeans::contrast(emm, method = "pairwise", adjust = "tukey")))
  posthoc$metric <- metric_name
  posthoc$set <- which_set
  list(omnibus = omnibus, posthoc = posthoc)
}

anova_pred <- expand.grid(metric = unique(fold_metrics$.metric), set = c("train", "test"), stringsAsFactors = FALSE) %>%
  mutate(result = map2(metric, set, run_anova_posthoc))
pred_omnibus <- bind_rows(map(anova_pred$result, "omnibus"))
pred_posthoc <- bind_rows(map(anova_pred$result, "posthoc"))
write.csv(pred_omnibus, file.path(res_dir, "prediction_lmer_omnibus.csv"), row.names = FALSE)
write.csv(pred_posthoc, file.path(res_dir, "prediction_tukey_posthoc.csv"), row.names = FALSE)

plot_df <- fold_metrics %>% filter(set == "test") %>% mutate(.metric = as.character(.metric))
for (m in c("accuracy", "precision", "recall", "f_meas", "mn_log_loss")) {
  if (!m %in% unique(plot_df$.metric)) next
  p <- plot_df %>% filter(.metric == m) %>%
    ggplot(aes(x = model, y = .estimate)) +
    geom_boxplot(width = 0.65, outlier.shape = NA) +
    labs(title = paste0("Test-set ", m, " across models"), x = "Model", y = m) +
    theme_classic()
  ggsave(file.path(fig_dir, paste0("prediction_test_", m, "_boxplot.png")), p, width = 6, height = 4.5, dpi = 300)
}

writeLines(capture.output(sessionInfo()), file.path(res_dir, "sessionInfo.txt"))
message("Analysis completed. Results saved to: ", res_dir, " and figures saved to: ", fig_dir)
