library(caret)
library(tidyverse)
library(plotly)
library(tidyquant)

# 1. Attrition Rate ----
attrition_rate <- customer_churn_tbl %>%
  summarise(attrition_rate = mean(Churn == "Yes") * 100) %>%
  pull(attrition_rate)

# 2. Average Customer Lifetime ----
avg_customer_lifetime <- customer_churn_tbl %>%
  summarise(avg_customer_lifetime = mean(Months_on_book)) %>%
  pull(avg_customer_lifetime)

# 3. Average Credit Utilization ----
avg_credit_utilization <- customer_churn_tbl %>%
  summarise(avg_credit_utilization = mean(Avg_Utilization_Ratio)) %>%
  pull(avg_credit_utilization)

# 4. Average Transaction Amount per Active Month ----
# First create the metric as a new column
customer_churn_tbl <- customer_churn_tbl %>%
  mutate(avg_trans_per_active_month = Total_Trans_Amt / (Total_Trans_Ct * (Months_on_book - Months_Inactive_12_mon)))

# Then calculate the average of this new column
avg_trans_per_active_month <- customer_churn_tbl %>%
  summarise(avg_trans_per_active_month = mean(avg_trans_per_active_month, na.rm = TRUE)) %>%
  pull(avg_trans_per_active_month)

# 5. Customer Interaction Frequency ----
# First create the metric as a new column
customer_churn_tbl <- customer_churn_tbl %>%
  mutate(customer_interaction_frequency = Contacts_Count_12_mon / Months_on_book)

# Then calculate the average of this new column
customer_interaction_frequency <- customer_churn_tbl %>%
  summarise(customer_interaction_frequency = mean(customer_interaction_frequency, na.rm = TRUE)) %>%
  pull(customer_interaction_frequency)