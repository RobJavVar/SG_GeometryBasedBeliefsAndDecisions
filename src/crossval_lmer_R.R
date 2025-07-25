
library(brms)
library(tidyverse)

run_cv_multimem_lmer <- function(df, outcome_var, random_effects, predictor_vars, k = 10, seed = 123) {
  set.seed(seed)
  
  df <- df %>%
    mutate(fold = sample(rep(1:k, length.out = n())))
  
  rmse_list <- numeric(k)
  formula_str <- paste0(outcome_var, " ~ ", paste(predictor_vars, collapse = " + "),
                        " + (1 | mm(", random_effects[1], ", ", random_effects[2], "))")
  
  cat("Formula:", formula_str, "\n")
  
  for (fold_idx in 1:k) {
    cat("Running fold", fold_idx, "\n")
    
    train_df <- df %>% filter(fold != fold_idx)
    test_df  <- df %>% filter(fold == fold_idx)
    
    model <- brm(
      formula = as.formula(formula_str),
      data = train_df,
      family = gaussian(),
      chains = 2, iter = 1000, cores = 2,
      silent = TRUE, refresh = 0
    )
    
    preds <- posterior_epred(model, newdata = test_df)
    pred_mean <- rowMeans(preds)
    
    rmse <- sqrt(mean((test_df[[outcome_var]] - pred_mean)^2))
    rmse_list[fold_idx] <- rmse
  }
  
  mean_rmse <- mean(rmse_list)
  cat("Average RMSE across folds:", mean_rmse, "\n")
  return(mean_rmse)
}