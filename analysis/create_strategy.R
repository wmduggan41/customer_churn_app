# SUPPORTING FILES FOR CREATING THE SHINY APP ----
# **** ----

# LIBRARIES ----
library(xgboost)
library(ranger)
library(glmnet)

# Tidymodels
library(tidymodels)
library(stacks)

# Parallel Processing
library(doParallel)

# Core
library(plotly)
library(tidyverse)
library(dplyr)
library(stringr)
library(scales)
library(ggplot2)

# Customer Data ----

churn_scores_tbl <- read_rds("analysis/churn_scores_tbl.rds")

# VIP ----

# Read xgboost tune results
tune_results_xgb <- read_rds("tune_results/tune_results_xgb.rds")

# Inspect attributes
attributes(tune_results_xgb)

wflw_spec_xgb <- attr(tune_results_xgb, "workflow")

# Fit an xgboost model with the optimal parameters
wflw_fit_xgb <- tune_results_xgb %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  arrange(-mean) %>%
  dplyr::slice(1) %>%
  finalize_workflow(wflw_spec_xgb, .) %>%
  fit(churn_scores_tbl)

# Get variable importance
vip::vip(wflw_fit_xgb$fit$fit)

# Write data
vip::vi_model(wflw_fit_xgb$fit$fit) %>% write_rds("vip/vip_results.rds")
vip_results_tbl <- read_rds("vip/vip_results.rds")


# Explore & Strategize ----

g <- churn_scores_tbl %>%
  ggplot(aes(Total_Trans_Ct, fill = Churn)) +
  geom_density(alpha = 0.5)

ggplotly(g)

g <- churn_scores_tbl %>%
  ggplot(aes(Total_Trans_Amt, fill = Churn)) +
  geom_density(alpha = 0.5, adjust = 3)

ggplotly(g)

g <- churn_scores_tbl %>%
  ggplot(aes(Total_Revolving_Bal, fill = Churn)) +
  geom_density(alpha = 0.5)

ggplotly(g)

g <- churn_scores_tbl %>%
  ggplot(aes(Total_Relationship_Count, fill = Churn)) +
  geom_density(alpha = 0.5)

ggplotly(g)


# Create recommendations ----

churn_recommendations_tbl <- churn_scores_tbl %>%
  mutate(
    flag_overall_risk = ifelse(.pred_Yes > quantile(.pred_Yes, 0.85), TRUE, FALSE),
    flag_total_trans_ct = ifelse(Total_Trans_Ct < 65, TRUE, FALSE),
    flag_total_trans_amt = ifelse(Total_Trans_Amt > 4404, TRUE, FALSE),
    flag_total_rel_ct = ifelse(Total_Relationship_Count < 4, TRUE, FALSE),
    flag_change = ifelse(Total_Ct_Chng_Q4_Q1 > 0.7122, TRUE, FALSE)
  ) %>%
  mutate(
    strategy_total_transactions = ifelse(flag_total_trans_ct & flag_total_trans_amt, "Customer has low counts of charges, high trans amount. May ask to lower APR", "Maintain and retain."),
    strategy_customer_service = ifelse(flag_total_rel_ct, "Customer has mediocre counts. Reach out and to see if there are any reward based incentives of interest.", "Maintain and retain."),
    strategy_sudden_change = ifelse(flag_change & flag_overall_risk, "Customer has sudden change and is High Risk. Consider offering low APR.", "Maintain and retain.")
  ) %>%
  rowid_to_column(var = "customer_id") %>%
  mutate(customer_label = str_glue("Customer: {customer}, {scales::percent(.pred_Yes)}"))

churn_recommendations_tbl %>% glimpse()

churn_recommendations_tbl %>% write_rds("analysis/churn_recommendations_tbl.rds")           


# Plotly Map ----

df = churn_recommendations_tbl %>% 
  dplyr::slice(1) %>%
  # glimpse() %>%
  mutate(hover_text = str_glue("Customer ID: {customer_id}
                                 Churn Risk: {scales::percent(.pred_Yes)} ({ifelse(flag_overall_risk, 'High', 'Low')})
                                 
                                 ---
                                 Key Attributes
                                 - Total Transactions: {Total_Trans_Ct}
                                 - Total Relationships: {Total_Relationship_Count}
                                 - Changes: {Total_Ct_Chng_Q4_Q1}"))


# Data for Heatmap
heatmap_data <- data.frame(
  .pred_Yes                = runif(20, 0, 1),
  Total_Trans_Ct           = runif(20, 0, 100),
  Total_Relationship_Count = runif(20, 0, 6),
  Total_Ct_Chng_Q4_Q1      = runif(20, 0, 1)
)

# Create the Heatmap
ggplot(heatmap_data, aes(x = Total_Trans_Ct, y = Total_Relationship_Count)) +
  geom_tile(aes(fill = .pred_Yes), color = "white") +
  scale_fill_gradientn(colors = rev(colorRampPalette(c("red", "yellow", "green"))(256))) +
  theme_minimal() +
  labs(title = "Heatmap",
       subtitle = "Churn Risk Based on Transactions and Relationships",
       fill = "Churn Risk") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
