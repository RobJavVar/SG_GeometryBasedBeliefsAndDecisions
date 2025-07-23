# Load required libraries
library(lme4)
library(dplyr)
library(stringr)
library(purrr)

# Clean column names
clean_column_name <- function(name) {
  name <- gsub("-", "_", name)                 # Replace hyphens with underscores
  name <- gsub("\\s*\\([^)]*\\)", "", name)    # Remove parentheses and *all* contents inside, plus surrounding space
  name <- str_trim(name)                       # Trim whitespace
  return(name)
}

# Bayesian Information Criterion calculator
bic_score <- function(y_true, y_pred, num_params) {
  n <- length(y_true)
  mse <- mean((y_true - y_pred)^2)
  bic <- n * log(mse) + num_params * log(n)
  return(bic)
}

# Cross-validation with multi-membership random effects
cross_val_kfold_lmer <- function(df, formula_str, k = 10, outcome_var = "BelBehAgreement", verbose = TRUE) {
  df <- df %>% rename_with(clean_column_name)
  
  # Extract fixed effect variables from formula
  all_vars <- all.vars(as.formula(formula_str))
  fixed_vars <- setdiff(all_vars, outcome_var)
  
  n <- nrow(df)
  folds <- sample(rep(1:k, length.out = n))
  
  mse_scores <- c()
  bic_scores <- c()
  coefs_list <- list()
  coef_lengths <- c()
  
  for (i in 1:k) {
    if (verbose) cat(sprintf("Fold %d:\n", i))
    
    train_df <- df[folds != i, ]
    test_df <- df[folds == i, ]
    
    # Drop fixed effect columns with no variance in training set
    constant_cols <- fixed_vars[sapply(train_df[fixed_vars], function(col) length(unique(col)) <= 1)]
    if (length(constant_cols) > 0 && verbose) {
      cat(sprintf("  Dropping zero-variance columns: %s\n", paste(constant_cols, collapse = ", ")))
    }
    
    train_df <- train_df %>% select(-any_of(constant_cols))
    test_df <- test_df %>% select(-any_of(constant_cols))
    formula_clean <- formula_str
    for (col in constant_cols) {
      pattern <- paste0("`?", col, "`?")
      formula_clean <- gsub(paste0(pattern, "\\s*\\+\\s*"), "", formula_clean)
      formula_clean <- gsub(paste0("\\+\\s*", pattern), "", formula_clean)
      formula_clean <- gsub(pattern, "", formula_clean)
    }
    
    # Build multi-membership random effects design matrix for train
    Z_A <- model.matrix(~0 + factor(train_df$participant_A))
    Z_B <- model.matrix(~0 + factor(train_df$participant_B))
    Z_mm <- (Z_A + Z_B) / 2
    colnames(Z_mm) <- make.names(colnames(Z_mm))
    train_df_mm <- cbind(train_df, Z_mm)
    
    # Build the same structure for test set, matching columns
    Z_A_test <- model.matrix(~0 + factor(test_df$participant_A))
    Z_B_test <- model.matrix(~0 + factor(test_df$participant_B))
    Z_mm_test <- (Z_A_test + Z_B_test) / 2
    colnames(Z_mm_test) <- make.names(colnames(Z_mm_test))
    
    # Ensure column match
    for (col in setdiff(colnames(Z_mm), colnames(Z_mm_test))) {
      Z_mm_test <- cbind(Z_mm_test, setNames(rep(0, nrow(test_df)), col))
    }
    Z_mm_test <- Z_mm_test[, colnames(Z_mm), drop = FALSE]
    test_df_mm <- cbind(test_df, Z_mm_test)
    
    # Construct full formula
    re_formula <- paste0("(0 + ", paste(colnames(Z_mm), collapse = " + "), ")")
    full_formula <- as.formula(paste(formula_clean, "+", re_formula))
    
    model <- tryCatch({
      lmer(full_formula, data = train_df_mm, REML = FALSE)
    }, error = function(e) {
      if (verbose) cat(sprintf("  Model fitting failed: %s\n", e$message))
      return(NULL)
    })
    
    if (!is.null(model)) {
      preds <- tryCatch({
        predict(model, newdata = test_df_mm, allow.new.levels = TRUE)
      }, error = function(e) {
        if (verbose) cat(sprintf("  Prediction failed: %s\n", e$message))
        return(rep(NA, nrow(test_df_mm)))
      })
      
      y_true <- test_df_mm[[outcome_var]]
      valid_idx <- !is.na(preds) & !is.na(y_true)
      
      mse <- mean((y_true[valid_idx] - preds[valid_idx])^2)
      bic <- bic_score(y_true[valid_idx], preds[valid_idx], length(fixef(model)))
      
      mse_scores <- c(mse_scores, mse)
      bic_scores <- c(bic_scores, bic)
      coefs_list[[length(coefs_list)+1]] <- fixef(model)
      coef_lengths <- c(coef_lengths, length(fixef(model)))
    } else {
      mse_scores <- c(mse_scores, NA)
      bic_scores <- c(bic_scores, NA)
      coefs_list[[length(coefs_list)+1]] <- NA
      coef_lengths <- c(coef_lengths, 0)
    }
    
    if (verbose) cat(sprintf("  %.1f%% complete (%d/%d)\n", i/k*100, i, k))
  }
  
  # Average fixed effects for most common model
  most_common_length <- as.numeric(names(sort(table(coef_lengths), decreasing = TRUE)[1]))
  filtered_coefs <- coefs_list[coef_lengths == most_common_length]
  coefs_df <- do.call(rbind, filtered_coefs)
  avg_coefs <- colMeans(coefs_df, na.rm = TRUE)
  
  return(list(
    mse_scores = mse_scores,
    bic_scores = bic_scores,
    avg_coefs = avg_coefs
  ))
}
