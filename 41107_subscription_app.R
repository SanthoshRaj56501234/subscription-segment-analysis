library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(lubridate)
library(scales)

# Read and prepare data
data_forecast <- read_csv("D2C_data.csv") %>%
  mutate(
    
    order_date = dmy(order_date), 
    week_date = dmy(week_date),
    revenue = quantity_ordered * price * (1 - discount)
  )

# UI 
ui <- page_sidebar(
  title = "Subscription Segment Analysis",
  theme = bs_theme(bootswatch = "darkly"),
  
  sidebar = sidebar(
    width = 250,
    h4("Filters"),
    selectInput("product", "Select Product", 
                choices = unique(data_forecast$product_id),
                selected = unique(data_forecast$product_id)[1]),
    dateInput("start_date", "Start Date", 
              value = min(data_forecast$order_date, na.rm = TRUE)),
    dateInput("end_date", "End Date", 
              value = max(data_forecast$order_date, na.rm = TRUE))
  ),
  
  layout_columns(
    col_widths = c(8, 4),
    card(
      card_header("Segment Revenue Trend"),
      plotlyOutput("revenue_trend", height = "500px")
    ),
    layout_columns(
      col_widths = 12,
      card(
        card_header("Recommendation"),
        uiOutput("recommendation")
      ),
      card(
        card_header("Returning Segment"),
        div(
          style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 15px;",
          div(
            style = "background: #3498db; padding: 20px; border-radius: 8px; text-align: center;",
            div(style = "color: white; font-size: 14px; margin-bottom: 10px;", "No of Customer"),
            div(style = "color: white; font-size: 36px; font-weight: bold;", textOutput("ret_customers"))
          ),
          div(
            style = "background: #3498db; padding: 20px; border-radius: 8px; text-align: center;",
            div(style = "color: white; font-size: 14px; margin-bottom: 10px;", "Avg Orders"),
            div(style = "color: white; font-size: 36px; font-weight: bold;", textOutput("ret_orders"))
          ),
          div(
            style = "background: #3498db; padding: 20px; border-radius: 8px; text-align: center;",
            div(style = "color: white; font-size: 14px; margin-bottom: 10px;", "LTV"),
            div(style = "color: white; font-size: 36px; font-weight: bold;", textOutput("ret_ltv"))
          )
        )
      ),
      card(
        card_header("Subscription Segment"),
        div(
          style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 15px;",
          div(
            style = "background: #27ae60; padding: 20px; border-radius: 8px; text-align: center;",
            div(style = "color: white; font-size: 14px; margin-bottom: 10px;", "No of Customer"),
            div(style = "color: white; font-size: 36px; font-weight: bold;", textOutput("sub_customers"))
          ),
          div(
            style = "background: #27ae60; padding: 20px; border-radius: 8px; text-align: center;",
            div(style = "color: white; font-size: 14px; margin-bottom: 10px;", "Avg Orders"),
            div(style = "color: white; font-size: 36px; font-weight: bold;", textOutput("sub_orders"))
          ),
          div(
            style = "background: #27ae60; padding: 20px; border-radius: 8px; text-align: center;",
            div(style = "color: white; font-size: 14px; margin-bottom: 10px;", "LTV"),
            div(style = "color: white; font-size: 36px; font-weight: bold;", textOutput("sub_ltv"))
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$start_date, input$end_date)
    
    data_forecast %>%
      filter(
        product_id == input$product,
        # --- CHANGE 2: Use order_date for filtering ---
        order_date >= input$start_date,
        order_date <= input$end_date
      ) %>%
      mutate(
        segment_type = case_when(
          segment == "Returning" ~ "Returning",
          segment == "Subscription" ~ "Subscription",
          TRUE ~ "New"
        )
      )
  })
  
  # Calculate segment metrics 
  segment_metrics <- reactive({
    filtered_data() %>%
      group_by(segment_type, customer_id) %>%
      summarise(
        orders = n_distinct(order_id),
        total_revenue = sum(revenue, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      group_by(segment_type) %>%
      summarise(
        n_customers = n_distinct(customer_id),
        avg_orders = round(mean(orders), 0),
        ltv = round(mean(total_revenue), 0),
        .groups = "drop"
      )
  })
  
  # Revenue trend chart 
  output$revenue_trend <- renderPlotly({
    trend_data <- filtered_data() %>%
      mutate(
        month_year = floor_date(week_date, "month"),
        month_label = format(month_year, "%b-%y")
      ) %>%
      group_by(month_label, month_year, segment_type) %>%
      summarise(revenue = sum(revenue, na.rm = TRUE), .groups = "drop") %>%
      arrange(month_year)
    
    if(nrow(trend_data) == 0) return(NULL)
    
    month_levels <- trend_data %>% 
      distinct(month_label, month_year) %>% 
      arrange(month_year) %>% 
      pull(month_label)
    
    trend_data$month_label <- factor(trend_data$month_label, levels = month_levels)
    
    max_rev <- max(trend_data$revenue, na.rm = TRUE)
    y_breaks <- seq(0, ceiling(max_rev/2500)*2500, by = 2500)
    
    plot_ly() %>%
      add_trace(
        data = trend_data %>% filter(segment_type == "Subscription"),
        x = ~month_label, y = ~revenue, name = "Subscription",
        type = "bar", marker = list(color = "#66BB6A"),
        hovertemplate = paste0("Month: %{x}<br>Subscription<br>Revenue: ₹%{y:,.0f}<extra></extra>")
      ) %>%
      add_trace(
        data = trend_data %>% filter(segment_type == "Returning"),
        x = ~month_label, y = ~revenue, name = "Returning",
        type = "bar", marker = list(color = "#42A5F5"),
        hovertemplate = paste0("Month: %{x}<br>Returning<br>Revenue: ₹%{y:,.0f}<extra></extra>")
      ) %>%
      add_trace(
        data = trend_data %>% filter(segment_type == "New"),
        x = ~month_label, y = ~revenue, name = "New",
        type = "bar", marker = list(color = "#FFA726"),
        hovertemplate = paste0("Month: %{x}<br>New<br>Revenue: ₹%{y:,.0f}<extra></extra>")
      ) %>%
      layout(
        barmode = "stack",
        paper_bgcolor = "#2c3e50",
        plot_bgcolor = "#2c3e50",
        font = list(color = "white"),
        xaxis = list(
          title = "Week",
          tickangle = -45,
          tickfont = list(color = "white", size = 9),
          gridcolor = "#34495e"
        ),
        yaxis = list(
          title = "Revenue (₹)",
          tickfont = list(color = "white"),
          gridcolor = "#34495e",
          tickmode = "array",
          tickvals = y_breaks,
          ticktext = as.character(y_breaks)
        ),
        legend = list(
          orientation = "h",
          x = 0.5, xanchor = "center",
          y = -0.2,
          font = list(color = "white"),
          bgcolor = "#2c3e50"
        ),
        margin = list(b = 100)
      ) %>%
      config(displayModeBar = FALSE)
  })
  

  
  # Returning metrics
  output$ret_customers <- renderText({
    metrics <- segment_metrics() %>% filter(segment_type == "Returning")
    if(nrow(metrics) > 0) as.character(metrics$n_customers) else "0"
  })
  
  output$ret_orders <- renderText({
    metrics <- segment_metrics() %>% filter(segment_type == "Returning")
    if(nrow(metrics) > 0) as.character(metrics$avg_orders) else "0"
  })
  
  output$ret_ltv <- renderText({
    metrics <- segment_metrics() %>% filter(segment_type == "Returning")
    if(nrow(metrics) > 0) paste0("₹", format(metrics$ltv, big.mark = ",")) else "₹0"
  })
  
  # Subscription metrics
  output$sub_customers <- renderText({
    metrics <- segment_metrics() %>% filter(segment_type == "Subscription")
    if(nrow(metrics) > 0) as.character(metrics$n_customers) else "0"
  })
  
  output$sub_orders <- renderText({
    metrics <- segment_metrics() %>% filter(segment_type == "Subscription")
    if(nrow(metrics) > 0) as.character(metrics$avg_orders) else "0"
  })
  
  output$sub_ltv <- renderText({
    metrics <- segment_metrics() %>% filter(segment_type == "Subscription")
    if(nrow(metrics) > 0) paste0("₹", format(metrics$ltv, big.mark = ",")) else "₹0"
  })
  
  # Recommendation
  output$recommendation <- renderUI({
    metrics <- segment_metrics()
    sub_ltv <- metrics %>% filter(segment_type == "Subscription") %>% pull(ltv)
    ret_ltv <- metrics %>% filter(segment_type == "Returning") %>% pull(ltv)
    
    if(length(sub_ltv) > 0 && length(ret_ltv) > 0) {
      diff_pct <- abs((sub_ltv - ret_ltv) / ret_ltv * 100)
      
      if(sub_ltv < ret_ltv * 0.915) {
        div(
          style = "color: #e74c3c; padding: 15px;",
          tags$h5(icon("times-circle"), " RECONSIDER SUBSCRIPTION STRATEGY"),
          tags$p(paste0("Subscription LTV (₹", format(sub_ltv, big.mark = ","), ") is ", 
                        round(diff_pct, 1), "% lower than Returning customers. ",
                        "The 5% discount may not be justified."))
        )
      } else {
        div(
          style = "color: #27ae60; padding: 15px;",
          tags$h5(icon("check-circle"), " SUBSCRIPTION PERFORMING WELL"),
          tags$p(paste0("Subscription LTV (₹", format(sub_ltv, big.mark = ","), ") is healthy."))
        )
      }
    } else {
      div(style = "padding: 15px;", "Insufficient data for recommendation")
    }
  })
}

shinyApp(ui, server)