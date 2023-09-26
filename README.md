## Navigating Dashboard

- Main components: Hover, click, or drag over card variables to render different selections. Cards will render different warnings as risk levels increase.
```
-	Select a Customer
-	Filter by Risk
-	Total Transactions (Threshold set to show green, yellow, red pending on numeric value)
-	Customer Service (counts on when customer reached out, and by count of relationships)
-	Sudden Change (value of change from Q1_Q4 for “Yes” Churn customers)
-	Heatmap where “Total_Relationship_Count” over “Total_Trans_Ct” is affected by Churn Risk threshold.
```

- “Churn Scoring Controls” = allows users to select Customer # (reassigned random #s to CLIENTNUM, provides % of risk in same cell). Reacts entire dashboard according to selection.

- “Filter by Risk” = user can drag to render Customer selections that fall within desired risk

## Data Analysis & Feature Engineering
```
- Calculated new metrics, such as the attrition rate, average transaction amount per active month
- Identified correlations between different variables and attrition.
- GLMNet, Random_Forest, and XGBoost techniques conducted for risk analysis and predictions.
- Stacked Ensembles to fine tune and optimize collecting metrics for best performing models.
```
