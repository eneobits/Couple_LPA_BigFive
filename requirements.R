required_packages <- c(
  "dplyr", "tidyr", "tidyLPA", "ggplot2", "reshape2", "survey",
  "effectsize", "broom", "tidymodels", "glmnet", "xgboost",
  "nnet", "lme4", "lmerTest", "emmeans", "tibble", "purrr", "vcd"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(required_packages, library, character.only = TRUE))
