calculate_metrics <- function(data) {
  attrition_rate <- (nrow(data[data$Churn == "Yes",]) / nrow(data)) * 100
  avg_customer_lifetime <- mean(data$tenure) / nrow(data)
  avg_credit_utilization <- sum(data$Avg_Utilization_Ratio) / nrow(data)
  avg_trans_per_active_month <- sum(data$Total_Trans_Amt) / (sum(data$Total_Trans_Ct) * sum(data$Months_Inactive))
  customer_interaction_frequency <- sum(data$Contacts_Count_12_mon) / sum(data$tenure)
  
  metrics <- data.frame(Attrition_Rate = attrition_rate, 
                        Average_Customer_Lifetime = avg_customer_lifetime,
                        Average_Credit_Utilization = avg_credit_utilization,
                        Avg_Trans_Per_Active_Month = avg_trans_per_active_month,
                        Customer_Interaction_Frequency = customer_interaction_frequency)
  
  return(metrics)
}

plot_pie_chart <- function(data, column_name) {
  pie_data <- data %>%
    group_by(!!sym(column_name)) %>%
    summarize(Count = n())
  
  ggplot(pie_data, aes(x = "", y = Count, fill = !!sym(column_name))) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y")
}
