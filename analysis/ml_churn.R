# LIBRARIES ----
library(tidyverse)
library(tidyquant)

library(rsample)
library(recipes)
library(h2o)

library(iml)
library(DALEX)

library(correlationfunnel)
library(DataExplorer)

library(tictoc)

customer_churn_raw_tbl <- read_csv("data/BankChurners.csv")

# customer_churn_raw_tbl %>% plot_missing()

ml_churn_tbl <- customer_churn_raw_tbl %>%
  mutate(Attrition_Flag = case_when(
    Attrition_Flag == "Existing Customer" ~ "No",
    Attrition_Flag == "Attrited Customer" ~ "Yes",
    TRUE ~ Attrition_Flag 
  )) %>% 
  rename(Churn = Attrition_Flag) %>% 
  rename(tenure = Months_on_book) %>%
  mutate(avg_trans_per_active_month = Total_Trans_Amt / (Total_Trans_Ct * (tenure - Months_Inactive_12_mon))) %>%
  mutate(customer_interaction_frequency = Contacts_Count_12_mon / tenure) %>%
  select(-Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1, 
         -Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2,
         -CLIENTNUM)

ml_churn_tbl

# 2.0 EDA ----

# UNDERSTAND GLOBAL DATA ----
funnel_churn_ggplot <- ml_churn_tbl %>%
  binarize() %>%
  correlate("Churn__Yes") %>%
  plot_correlation_funnel()

funnel_churn_ggplot

# 3.0 MACHINE LEARNING ----
ml_churn_tbl %>% glimpse()

# 3.1 Preprocessing ----
set.seed(123)
rsample_splits <- initial_split(ml_churn_tbl, prop = 0.8)

rec_obj <- recipe(Churn ~ ., data = training(rsample_splits)) %>%
  step_string2factor(all_nominal()) %>%  # Convert all nominal variables to factors
  step_center(all_numeric(), -all_outcomes()) %>%  # Center numeric variables
  step_scale(all_numeric(), -all_outcomes()) %>%  # Scale numeric variables
  prep()

train_tbl <- bake(rec_obj, training(rsample_splits))
test_tbl  <- bake(rec_obj, testing(rsample_splits))

train_tbl

# 3.2 Random Forest ----
h2o.init()

y <- "Churn"
x <- setdiff(names(train_tbl), y)

h2o_rf <- h2o.randomForest(
  x = x,
  y = y,
  training_frame   = as.h2o(train_tbl),
  validation_frame = as.h2o(test_tbl),
  ntrees = 1000,
  max_depth = 8,
  nfolds = 5,
  stopping_metric = "AUC",
  stopping_rounds = 15,
  stopping_tolerance = 0.005,
  seed = 123
)

h2o_rf

h2o.predict(h2o_rf, newdata = as.h2o(test_tbl)) %>% as_tibble()

h2o.auc(h2o_rf, valid = TRUE)
h2o.auc(h2o_rf, xval = TRUE)

# 4.0 IML ----

# 4.1 IML SETUP ----

# 4.1.1 Model Agnostic Functions

# Setting up with Train data (*0%)
features_tbl <- train_tbl %>% select(-Churn)

response_vec <- train_tbl %>% pull(Churn) %>% as.numeric() - 1

predict_h2o <- function(model, newdata) {
  results_tbl <- h2o.predict(model, newdata = as.h2o(newdata)) %>% as_tibble()
  return(results_tbl)
  #results_tbl %>% pull(Yes)
}

predict_h2o(h2o_rf, newdata = train_tbl)
pred_result <- predict_h2o(h2o_rf, newdata = train_tbl)
print(head(pred_result))


# 4.1.2 IML Predictor Object

# Convert tibble to data.frame for compatibility
features_df <- as.data.frame(features_tbl)

predictor_rf <- Predictor$new(
  model            = h2o_rf,
  data             = features_df,
  y                = response_vec,
  predict.function = predict_h2o,
  class            = "classification",
  type             = "prob"
)

print(h2o.getModel("DRF_model_R_1695155553878_1")@model$features)

# GLOBAL MODEL EXPLAINATION -----

funnel_churn_ggplot

# 5.0 PDP - Partial Dependence Plots ----

# Single Feature - Contract Type 
pdp_con <- FeatureEffect$new(
  predictor = predictor_rf,
  feature   = "Total_Trans_Ct",
  method    = "pdp",
  grid.size = 20
)

pdp_con %>%
  plot() +
  expand_limits(y = 0)

# 2-Way Interactions - Contract Type & Monthly Charges

# NOTE - LONG RUNNING SCRIPT - May take 1 minute or so to run
# Tip - up grid size to get more granularity
tic()
pdp_monthly_charges_by_contract <- FeatureEffect$new(
  predictor = predictor_rf,
  feature   = c("Contract", "MonthlyCharges"),
  method    = "pdp",
  grid.size = 20
)
toc()

pdp_monthly_charges_by_contract %>%
  plot(rug = TRUE) +
  expand_limits(y = 0) +
  theme_tq() +
  scale_color_tq() +
  labs(
    title = "How Random Forest Models Churn",
    subtitle = "2-Way Interaction between Monthly Charges & Contract Type",
    x = "Monthly Charges"
  )



# 6.0 ICE - Individual Conditional Expectation ----

# 6.1 Investigate Contract
ice_con <- FeatureEffect$new(
  predictor = predictor_rf,
  feature   = "Total_Revolving_Bal",
  method    = "ice"
)

ice_contract %>% 
  plot() +
  expand_limits(y = 0) +
  theme_tq() +
  labs(title = "ICE Plot", subtitle = "Contract Type")

# 6.2 Investigate Tenure (without centering)
ice_tenure <- FeatureEffect$new(
  predictor = predictor_rf,
  feature   = "tenure",
  method    = "ice",
  grid.size = 10,
  # No centering
  center.at = NULL
)

ice_tenure %>% 
  plot() +
  geom_smooth(color = palette_light()["green"], size = 2) +
  expand_limits(y = 0) +
  theme_tq() +
  labs(title = "ICE Plot, No Centering")

# 6.3 Investigate Tenure (Centering Churn at Zero)
ice_tenure_centered <- FeatureEffect$new(
  predictor = predictor_rf,
  feature   = "tenure",
  method    = "ice",
  grid.size = 10,
  # No centering
  center.at = 0
)

ice_tenure_centered %>% 
  plot() +
  geom_smooth(color = palette_light()["green"], size = 2) +
  expand_limits(y = 0) +
  theme_tq() +
  labs(title = "ICE Plot, Centering at Zero")



# LOCAL ----

# 7.0 LIME ----
lime_rf <- LocalModel$new(
  predictor  = predictor_rf,
  x.interest = test_tbl %>% slice(2) %>% select(-Churn),
  dist.fun   = "gower",
  k          = 5
)

lime_rf %>% 
  plot() +
  theme_tq() +
  labs(title = "LIME Plot", subtitle = "Customer 2")




# 8.0 SHAP ----
shapley_rf <- Shapley$new(
  predictor  = predictor_rf,
  x.interest = test_tbl %>% slice(2) %>% select(-Churn),
  sample.size = 200
)

shapley_rf %>% 
  plot() +
  theme_tq()


# 9.0 LL PRO BONUS - AUTO ML + DALEX ---- 

# H2O AutoML & DALEX
tic()
source("LL_PRO_BONUS_h2o_automl_dalex.R")
toc()

h2o_automl
h2o_automl@leaderboard
h2o_leader

# 9.1 SHAP ----
# plot(shap_h2o_leader, max_features = 5)

# 9.2 BREAKDOWN ----
breakdown_h2o_leader <- break_down(
  x = explainer_h2o_leader,
  test_tbl %>% slice(2) %>% select(-Churn),
  interactions = FALSE)

plot(breakdown_h2o_leader, max_features = 4)