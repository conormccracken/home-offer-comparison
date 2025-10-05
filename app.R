# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(plotly)
library(bslib)

# ============== Define UI for application ==============
ui <- fluidPage(
  
  # Application title
  titlePanel("Costing & Comparing Home Offer Prices"),
  
  # Sidebar with user inputs
  sidebarLayout(
    sidebarPanel(
      tags$p('This tool was initially made to help visualize the implicit costs of bidding on a home. That said, it might also help with understanding the financial aspects of home buying in general!'),
      tags$br(),
      h4("Financial Inputs"),
      
      numericInput("target_monthly_payment",
                   label = "Your Target Monthly Payment ($):",
                   value = 2500,
                   min = 0),
      
      numericInput("mortgage_rate",
                   label = "Mortgage Rate (%):",
                   value = 6.5,
                   min = 0,
                   max = 20,
                   step = 0.1),
      
      numericInput("down_payment_ceiling",
                   label = "Your Down Payment ($):",
                   value = 100000,
                   min = 0),
      
      hr(), 
      
      h4("Offer & Plot Inputs"),
      
      sliderInput("offer_range",
                  label = "Range of Offers to Consider ($):",
                  min = 150000,
                  max = 800000,
                  value = c(400000, 600000),
                  step = 10000,
                  pre = "$",
                  sep = ","),
      
      numericInput("offer_interval",
                   label = "Offer Interval ($):",
                   value = 25000,
                   min = 1000),
      
      numericInput("plot_width",
                   label = "Plot Width (+/- Target Payment $):",
                   value = 750,
                   min = 100),
      
      numericInput("plot_height",
                   label = "Plot Height (+/- Down Payment Ceiling $):",
                   value = 50000,
                   min = 1000)
    ),
    
    # Main panel for displaying the plot(s)
    mainPanel(
      card(
        tags$b('Consider the following...'),
        tags$ul(
          tags$li('"Your Target Monthly Payment" is generally recommended not to exceed 28% of your gross monthly income (e.g., before taxes and other expenses). But what is or is not affordable is different for everyone.'),
          tags$li('"Offer Interval" controls how many reference lines are generated in the "Required Down Payment vs. Monthly Payment" chart; smaller intervals generate more offers than larger ones!'),
          tags$li('"Plot Width" and "Plot Height" control the x- and y-axis, respectively.'),
          tags$li('It is VERY important to keep in mind what this line graph represents: the relationship between amount down and expected monthly mortgage payment at a given offer/sale price. The actual cost of a fully-amoritized 30 year fixed-rate mortgage varies drastically across different points on the same line.'),
        )
      ),
      hr(),
      plotlyOutput("mortgagePlot"),
      hr(), 
      h4("Total Cost of Loan Over 30 Years"),
      plotlyOutput("totalCostPlot")
    )
  )
)

# ============== Define Server logic ==============
server <- function(input, output) {
  
  calculate_payment <- function(principal, rate, periods) {
    payment_formula <- principal * (rate * (1 + rate)^periods) / ((1 + rate)^periods - 1)
    ifelse(principal <= 0, 0, payment_formula)
  }
  
  # Reactive expression for the main plot's data
  generated_data <- reactive({
    
    req(input$offer_interval > 0, input$plot_width > 0, input$plot_height > 0)
    
    r <- (input$mortgage_rate / 100) / 12 
    n <- 30 * 12
    
    if (r == 0) return(NULL)
    
    plot_min_x <- max(0, input$target_monthly_payment - input$plot_width)
    plot_max_x <- input$target_monthly_payment + input$plot_width
    
    plot_min_y <- max(0, input$down_payment_ceiling - input$plot_height)
    plot_max_y <- input$down_payment_ceiling + input$plot_height
    
    monthly_payments <- seq(from = plot_min_x, to = plot_max_x, length.out = 200)
    offers <- seq(from = input$offer_range[1], to = input$offer_range[2], by = input$offer_interval)
    
    data <- expand.grid(
      offer_price = offers,
      monthly_payment = monthly_payments
    )
    
    data <- data %>%
      mutate(
        loan_principal = monthly_payment * (((1 + r)^n - 1) / (r * (1 + r)^n)),
        down_payment = offer_price - loan_principal,
        offer_label = factor(paste0("$", format(offer_price, big.mark = ",")))
      ) %>%
      filter(down_payment >= 0, down_payment <= offer_price)
    
    list(data = data, 
         x_range = c(plot_min_x, plot_max_x), 
         y_range = c(plot_min_y, plot_max_y))
  })
  
  # Render the main interactive plot
  output$mortgagePlot <- renderPlotly({
    
    plot_info <- generated_data()
    
    if (is.null(plot_info) || nrow(plot_info$data) == 0) return(plotly_empty()) 
    
    df <- plot_info$data
    x_range <- plot_info$x_range
    y_range <- plot_info$y_range
    
    p <- ggplot(df, aes(x = monthly_payment, y = down_payment, color = offer_label)) +
      geom_line(linewidth = 1.2) +
      geom_hline(aes(yintercept = input$down_payment_ceiling, linetype = "Down Payment Ceiling"), 
                 color = "black", linewidth = 1) +
      geom_vline(aes(xintercept = input$target_monthly_payment, linetype = "Monthly Payment Wall"), 
                 color = "darkgrey", linewidth = 1) +
      scale_linetype_manual(
        name = "Reference Lines", 
        values = c("Down Payment Ceiling" = "dashed", "Monthly Payment Wall" = "dashed")
      ) +
      scale_y_continuous(labels = label_dollar(), limits = y_range) +
      scale_x_continuous(labels = label_dollar(), limits = x_range) +
      labs(
        title = "Required Down Payment vs. Monthly Payment",
        x = "Monthly Mortgage Payment (Principal & Interest)",
        y = "Required Down Payment",
        color = "Offer Price"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom", legend.box = "vertical")
    
    ggplotly(p)
  })
  
  # Render the total cost bar chart
  output$totalCostPlot <- renderPlotly({
    
    req(input$mortgage_rate, input$down_payment_ceiling, input$offer_range)
    
    r <- (input$mortgage_rate / 100) / 12
    n <- 30 * 12
    if (r == 0) return(plotly_empty())
    
    offers_seq <- seq(from = input$offer_range[1], to = input$offer_range[2], by = input$offer_interval)
    key_offers <- c(min(offers_seq), median(offers_seq), max(offers_seq)) %>% unique()
    
    cost_df <- tibble(offer_price = key_offers) %>%
      mutate(
        down_payment_used = pmin(offer_price, input$down_payment_ceiling),
        loan_principal = offer_price - down_payment_used,
        resulting_monthly_payment = calculate_payment(loan_principal, r, n),
        total_cost = down_payment_used + (resulting_monthly_payment * n),
        Offer_Type = case_when(
          offer_price == min(offer_price) ~ "Lowest Offer",
          offer_price == max(offer_price) ~ "Highest Offer",
          TRUE ~ "Median Offer"
        ) %>% factor(levels = c("Lowest Offer", "Median Offer", "Highest Offer")),
        tooltip_text = paste0(
          "Offer: ", dollar(offer_price),
          "\nTotal Cost: ", dollar(total_cost),
          "\nResulting Monthly Payment: ", dollar(resulting_monthly_payment)
        )
      )
    
    if (nrow(cost_df) == 0) {
      return(plotly_empty() %>% layout(title = "Cannot calculate total cost with current inputs."))
    }
    
    p2 <- ggplot(cost_df, aes(x = Offer_Type, y = total_cost, fill = Offer_Type, 
                              text = tooltip_text)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = dollar(total_cost, accuracy = 1)), vjust = -0.5) +
      scale_y_continuous(labels = label_dollar()) +
      labs(
        subtitle = paste("Assuming a down payment of", dollar(input$down_payment_ceiling)),
        x = NULL,
        y = "Total Cost (Down Payment + All Payments)",
        fill = NULL
      ) +
      theme_minimal(base_size = 14)
    
    ggplotly(p2, tooltip = "text") %>%
      layout(
        yaxis = list(title = "Total Cost Over 30 Years")
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)