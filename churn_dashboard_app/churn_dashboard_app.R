library(shiny)
library(shinyWidgets)
library(shinydashboardPlus)
library(bslib)
library(plotly)
library(tidyverse)

color_primary = "#78c2ad"

vip_results_tbl <- read_rds("vip/vip_results.rds")

churn_recommendations_tbl <- read_rds("analysis/churn_recommendations_tbl.rds")


ui <- navbarPage(
  
  title = 'Customer Retention App',
  theme = bs_theme(version = 4, bootswatch = 'lux'),
  inverse = TRUE, 
  
  tabPanel(
    title = "Churn Scoring",
    
    # SIDEBAR ----
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h1("Customer Churn Risk"),
        fluidRow(
          column(width = 2),
          column(width = 10, h3("William Duggan"))
        ),
        br(),
        hr(),
        
        
        h3("Churn Scoring Controls"),
        uiOutput("customer_selector"),
        knobInput(
          inputId = "risk_threshold",
          label = "Filter by Risk",
          value = 0,
          min = 0,
          max = 100,
          displayPrevious = TRUE,
          fgColor = "#428BCA",
          inputColor = "#428BCA"
        )
        
      ),
      
      # MAIN ----
      mainPanel(
        width = 9,
        uiOutput("cards"),
        plotlyOutput("map", height = "500px")
        
      )
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  rv$selected = churn_recommendations_tbl %>% dplyr::slice(1)
  
  observe({
    
    rv$churn_filtered_tbl <- churn_recommendations_tbl %>%
      filter(.pred_Yes > (as.numeric(input$risk_threshold) / 100))
    
    
    
    
  })
  
  observe({
    req(input$customer_id)
    rv$selected <- rv$churn_filtered_tbl %>%
      filter(customer_label == input$customer_id)
  })
  
  output$customer_selector <- renderUI({
    req(rv$churn_filtered_tbl)
    
    shiny::selectizeInput(
      inputId = "customer_id", label = "Select a Customer", choices = rv$churn_filtered_tbl$customer_label
    )
  })
  
  output$cards <- renderUI({
    
    req(rv$selected)
    
    overall_warn_level <- case_when(
      rv$selected$.pred_Yes > quantile(churn_recommendations_tbl$.pred_Yes, 0.85, na.rm = TRUE) ~ 'danger',
      rv$selected$.pred_Yes > quantile(churn_recommendations_tbl$.pred_Yes, 0.70, na.rm = TRUE) ~ 'warning',
      TRUE ~ 'success'
    )
    risk_level_text <- scales::percent(rv$selected$.pred_Yes, 1)
    print(overall_warn_level)
    
    card_1_warn_level <- ifelse(rv$selected$strategy_total_transactions != "Maintain and retain.", "warning", "success")
    card_1_header     <- ifelse(rv$selected$strategy_total_transactions != "Maintain and retain.", "WARNING!", "Doing fine.")
    
    card_2_warn_level <- ifelse(rv$selected$strategy_customer_service != "Maintain and retain.", "warning", "success")
    card_2_header     <- ifelse(rv$selected$strategy_customer_service != "Maintain and retain.", "WARNING!", "Doing fine.")
    
    card_3_warn_level <- ifelse(rv$selected$strategy_sudden_change != "Maintain and retain.", "warning", "success")
    card_3_header     <- ifelse(rv$selected$strategy_sudden_change != "Maintain and retain.", "WARNING!", "Doing fine.")
    
    shiny::tagList(
      shiny::fluidRow(
        column(
          width = 12,
          h1(str_glue("Customer {rv$selected$customer_id}")),
          # br(),
          h4(tags$small(str_glue("Risk Level: {risk_level_text}"))),
          HTML(str_glue(
            '<div class="progress">
                          <div class="progress-bar bg-{overall_warn_level}" role="progressbar" style="width: {risk_level_text};" aria-valuenow="0" aria-valuemin="0" aria-valuemax="100"></div>
                        </div>'
          ))
        )
      ),
      
      shiny::fluidRow(
        
        column(
          width = 4, 
          h1("Total Transactions"),
          p("Charged to customer"),
          HTML(
            str_glue('<div class="alert alert-dismissible alert-{card_1_warn_level}">
                        <button type="button" class="close" data-dismiss="alert">&times;</button>
                        <h4 class="alert-heading">{card_1_header}</h4>
                        <h5>{round(rv$selected$Total_Trans_Ct)}</h5>
                        <p class="mb-0">{rv$selected$strategy_total_transactions}</p>
                    </div>')
          )
          
          # plotlyOutput("interest_rate_plot"),
        ), 
        column(
          width = 4,
          h1("Customer Service"),
          p("Counts"),
          HTML(
            str_glue('<div class="alert alert-dismissible alert-{card_2_warn_level}">
                        <button type="button" class="close" data-dismiss="alert">&times;</button>
                        <h4 class="alert-heading">{card_2_header}</h4>
                        <h5>{round(rv$selected$Total_Relationship_Count)}</h5>
                        <p class="mb-0">{rv$selected$strategy_customer_service}</p>
                    </div>')
          )
        ), 
        column(
          width = 4,
          h1("Sudden Change"),
          p("What Happened?"),
          HTML(
            str_glue('<div class="alert alert-dismissible alert-{card_3_warn_level}">
                        <button type="button" class="close" data-dismiss="alert">&times;</button>
                        <h4 class="alert-heading">{card_3_header}</h4>
                        <h5>{rv$selected$Total_Ct_Chng_Q4_Q1}</h5>
                        <p class="mb-0">{rv$selected$strategy_sudden_change}</p>
                    </div>')
          )
        )
      )
    )
    
    
    
  })
  
  output$map <- renderPlotly({
    
    req(rv$selected)
    
    # Assuming rv$selected holds the user-selected data
    df <- rv$churn_filtered_tbl %>%
      mutate(hover_text = str_glue("Customer ID: {customer_id}
                                 Churn Risk (Churn %): {scales::percent(.pred_Yes)} ({ifelse(flag_overall_risk, 'High', 'Low')})
                                 
                                 ---
                                 Key Attributes
                                 - Total Transactions: {Total_Trans_Ct}
                                 - Total Relationships: {Total_Relationship_Count}
                                 - Churn: {.pred_Yes}"))
    
    # Extract numeric columns
    numeric_data <- df %>% select_if(is.numeric)
    
    # Calculate the correlation matrix
    cor_matrix <- cor(numeric_data, use = "complete.obs")
    
    # Convert the correlation matrix into a long format for ggplot
    heatmap_data_long <- as.data.frame(as.table(cor_matrix))
    
    # Create the Heatmap
    fig <- ggplot(heatmap_data_long, aes(Var1, Var2)) +
      geom_tile(aes(fill = Freq)) +
      scale_fill_gradientn(colors = rev(colorRampPalette(c("red", "yellow", "green"))(256)),
                           limits = c(-1, 1),
                           name = "Correlation") +
      theme_minimal() +
      labs(title = "Correlation Heatmap",
           subtitle = "Relationship between numeric variables colored by correlation value") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
            axis.text.y = element_text(size = 7),
            axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
    # Convert the ggplot object to a plotly object
    plotly_fig <- ggplotly(fig)
    
    return(plotly_fig)
    
    
    
  })
  
  
  
}

shinyApp(ui, server)
