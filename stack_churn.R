# CHURN SCORING APP ----
# BUSINESS OBJECTIVE ----
# - Score likelihood of churn
# - Use stacks ensembling to increase model accuracy

# LIBRARIES ----

# Machine Learning Libraries
library(xgboost)
library(ranger)
library(glmnet)

# Tidymodels
library(tidymodels)
library(stacks)

# Parallel Processing
library(doParallel)

# Core
library(tidyverse)
library(plotly)


# SETUP PARALLEL PROCESSING ----

all_cores <- parallel::detectCores(logical = FALSE)

cl <- makePSOCKcluster(all_cores)

registerDoParallel(cl)

# parallel::stopCluster(cl)


# DATA ----

customer_churn_prep_tbl <- read_csv("data/BankChurners.csv")
# Generate a vector of random unique values within the range of the number of rows
random_values <- sample(1:nrow(customer_churn_prep_tbl))

# Create a new column for the random values:
customer_churn_prep_tbl$customer <- random_values

# Binarize the data frame to a .rds file
saveRDS(customer_churn_prep_tbl, "data/BankChurners.rds")

customer_churn_prep_tbl %>% glimpse()

# PREPROCESSING ----

# Rename "Churn" for logical expression
customer_churn_tbl <- customer_churn_prep_tbl %>%
  mutate(Attrition_Flag = case_when(
    Attrition_Flag == "Existing Customer" ~ "No",
    Attrition_Flag == "Attrited Customer" ~ "Yes",
    TRUE ~ Attrition_Flag 
  )) %>% 
  rename(Churn = Attrition_Flag) %>% 
  select(-Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1, 
         -Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2,
         -CLIENTNUM)

# Normalize numeric data, then dummy categorical
recipe_spec <- recipe(Churn ~ ., data = customer_churn_tbl) %>%
  step_normalize(all_numeric()) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE)

recipe_spec %>% prep() %>% juice() %>% glimpse()

# K-FOLD SPEC ----
set.seed(123)
resamples <- customer_churn_tbl %>% vfold_cv(v = 5, strata = Churn)

resamples

# MODEL RESAMPLING ----

# * Elastic Net ----

wflw_spec_glmnet <- workflow() %>%
  add_model(
    spec = logistic_reg(
      mode  = "classification",
      penalty = tune(),
      mixture = tune()
    ) %>%
      set_engine("glmnet")
  ) %>%
  add_recipe(recipe_spec)



t0 <- Sys.time()
set.seed(123)
tune_results_glmnet <- tune_grid(
  object     = wflw_spec_glmnet, 
  resamples  = resamples, 
  grid       = 5, 
  control    = control_grid(
    verbose       = TRUE, 
    allow_par     = TRUE, 
    save_pred     = TRUE, 
    save_workflow = TRUE
  )
)
tune_results_glmnet
t1 <- Sys.time()
# Time difference in secs
t1 - t0

# Write then read results
tune_results_glmnet %>% write_rds("tune_results/tune_results_glmnet.rds")
tune_results_glmnet <- read_rds("tune_results/tune_results_glmnet.rds")

tune_results_glmnet %>% collect_metrics() %>% filter(.metric == "roc_auc") %>% arrange(-mean)

object.size(tune_results_glmnet)

# * Random Forest ----
wflw_spec_rf <- workflow() %>%
  add_model(
    spec = rand_forest(
      mode  = "classification",
      mtry  = tune(),
      min_n = tune(),
      trees = 500
    ) %>%
      set_engine("ranger")
  ) %>%
  add_recipe(recipe_spec)



t0 <- Sys.time()
set.seed(123)
tune_results_rf <- tune_grid(
  object     = wflw_spec_rf, 
  resamples  = resamples, 
  grid       = 5, 
  control    = control_grid(
    verbose       = TRUE, 
    allow_par     = TRUE, 
    save_pred     = TRUE, 
    save_workflow = TRUE
  )
)
tune_results_rf
t1 <- Sys.time()
# Time difference
t1 - t0

# Write then read results
tune_results_rf %>% write_rds("tune_results/tune_results_rf.rds")
tune_results_rf <- read_rds("tune_results/tune_results_rf.rds")

tune_results_rf %>% collect_metrics() %>% filter(.metric == "roc_auc") %>% arrange(-mean)

object.size(tune_results_rf)

# * XGBoost ----

wflw_spec_xgb <- workflow() %>%
  add_model(
    spec = boost_tree(
      mode       = "classification",
      min_n      = tune(),
      learn_rate = tune(),
      trees      = 500
    ) %>%
      set_engine("xgboost")
  ) %>%
  add_recipe(recipe_spec)

t0 <- Sys.time()
set.seed(123)
tune_results_xgb <- tune_grid(
  object     = wflw_spec_xgb, 
  resamples  = resamples, 
  grid       = 5,
  control    = control_grid(
    verbose       = TRUE, 
    allow_par     = TRUE, 
    save_pred     = TRUE, 
    save_workflow = TRUE
  )
)
tune_results_xgb
t1 <- Sys.time()
# Time difference
t1 - t0


# Write then read results
tune_results_xgb %>% write_rds("tune_results/tune_results_xgb.rds")
tune_results_xgb <- read_rds("tune_results/tune_results_xgb.rds")

tune_results_xgb %>% collect_metrics() %>% filter(.metric == "roc_auc") %>% arrange(-mean)

object.size(tune_results_xgb)

# STACKED ENSEMBLE ----

# * Add Candidates ----
model_stack <- stacks() %>%
  add_candidates(tune_results_glmnet) %>%
  add_candidates(tune_results_rf) %>%
  add_candidates(tune_results_xgb)

# * Blend Predictions ----
model_stack_blend <- model_stack %>% 
  blend_predictions(
    metric = metric_set(roc_auc)
  )

model_stack_blend

object.size(model_stack_blend)


# * Visualize candidate performance ----

model_stack_blend %>% autoplot()

model_stack_blend %>% autoplot(type = "members")

model_stack_blend %>% autoplot(type = "weights")

# * Fit ----

model_stack_fit <- model_stack_blend %>%
  fit_members()

model_stack_fit

object.size(model_stack_fit)
# Model size around 60MB

# * Predict ----

churn_scores_tbl <- model_stack_fit %>% 
  predict(customer_churn_tbl, type = "prob") %>%
  bind_cols(customer_churn_tbl)

churn_scores_tbl 

# Convert 'Churn' to a factor
churn_scores_tbl$Churn <- as.factor(churn_scores_tbl$Churn)

churn_scores_tbl %>% roc_auc(Churn, .pred_Yes)

churn_scores_tbl %>% write_rds("analysis/churn_scores_tbl.rds")


# SHINY APP ----

shiny::runApp(appDir = "churn_app.R")

