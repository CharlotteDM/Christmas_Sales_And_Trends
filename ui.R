library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
print(path)

setwd(path)

#Source of data: https://zoomcharts.com/en/microsoft-power-bi-custom-visuals/challenges/onyx-data-december-2023?utm_source=challenge&utm_medium=onyxdata&utm_campaign=onyxdata_web_december&utm_term=submit&utm_content=registration
christmas_sales <- read.csv("Christmas Sales and Trends.csv", stringsAsFactors = FALSE)
christmas_sales$Hour <- as.numeric(format(strptime(christmas_sales$Time, format = "%H:%M:%S"), "%H"))



# UI
ui <- dashboardPage(
  skin = "red", 
  dashboardHeader(title = "🎄 Christmas Sales Dashboard 🎅"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Hourly Sales", tabName = "hourly_sales", icon = icon("clock")),
      menuItem("Sales by Age and Gender", tabName = "age_gender", icon = icon("users")),
      menuItem("Sales vs Weather", tabName = "weather", icon = icon("cloud")),
      menuItem("Product Category Sales", tabName = "category_sales", icon = icon("tags")),
      menuItem("Customer Satisfaction", tabName = "satisfaction", icon = icon("smile"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
         .content-wrapper { background-color: #f5f5f5; }
        .main-header .logo { background-color: #b71c1c !important; }
        .skin-red .main-header .navbar { background-color: #d32f2f !important; }
        .skin-red .main-header .navbar .sidebar-toggle { color: #fff; }
        .skin-red .main-header .navbar .navbar-brand { color: #fff; }
        .box { border-radius: 15px; }
        
        /* Czerwony pasek dla nagłówka wykresów */
        .box-header {
          background-color: #D32F2F !important;  /* Czerwony pasek */
        }
        
        /* Kolor tytułu na białym tle */
        .box-title {
          color: white;  /* Kolor tekstu */
          font-size: 18px;
          font-weight: bold;
      "))
    ),
    tabItems(
      tabItem(tabName = "hourly_sales",
              fluidRow(
                box(title = "Hourly Sales", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("hourlySalesPlot", height = "600px"))
              )
      ),
      tabItem(tabName = "age_gender",
              fluidRow(
                box(title = "Sales by Age and Gender", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("ageGenderPlot", height = "600px"))
              )
      ),
      tabItem(tabName = "weather",
              fluidRow(
                box(title = "Sales vs Weather", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("weatherPlot", height = "600px"))
              )
      ),
      tabItem(tabName = "category_sales",
              fluidRow(
                box(title = "Filter", width = 4, status = "warning",
                    pickerInput("category", "Select Category:", choices = unique(christmas_sales$Category), options = list(`live-search` = TRUE))
                ),
                box(title = "Product Category Sales", width = 8, status = "primary", solidHeader = TRUE,
                    plotOutput("categorySalesPlot", height = "600px"))
              )
      ),
      tabItem(tabName = "satisfaction",
              fluidRow(
                box(title = "Customer Satisfaction Distribution", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("satisfactionPlot", height = "600px"))
              )
      )
    ),
    tags$footer(
      style = "text-align: center; padding: 10px; background: #b71c1c; color: white;",
      "Source of Data: ",
      tags$a(href = "https://zoomcharts.com/en/microsoft-power-bi-custom-visuals/challenges/onyx-data-december-2023?utm_source=challenge&utm_medium=onyxdata&utm_campaign=onyxdata_web_december&utm_term=submit&utm_content=registration",
             "https://zoomcharts.com", target = "_blank"),
      " | Merry Christmas! 🎄"
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive: preparing data for plots
  hourly_sales_data <- reactive({
    online_sales <- christmas_sales %>%
      filter(OnlineOrderFlag == TRUE) %>%
      group_by(Hour) %>%
      summarise(mean_sales = mean(TotalPrice, na.rm = TRUE))
    
    offline_sales <- christmas_sales %>%
      filter(OnlineOrderFlag == FALSE) %>%
      group_by(Hour) %>%
      summarise(mean_sales = mean(TotalPrice, na.rm = TRUE))
    
    list(online_sales = online_sales, offline_sales = offline_sales)
  })
  
  # Reactive: "Sales by Age and Gender"
  age_gender_data <- reactive({
    christmas_sales %>%
      group_by(Gender, Age) %>%
      summarise(mean_sales = mean(TotalPrice, na.rm = TRUE))
  })
  
  # Reactive: "Sales vs Weather"
  weather_data <- reactive({
    christmas_sales %>%
      group_by(Weather) %>%
      summarise(mean_sales = mean(TotalPrice, na.rm = TRUE))
  })
  
  # Reactive: Product Category Sales"
  category_sales_data <- reactive({
    req(input$category)
    christmas_sales %>%
      filter(Category == input$category)
  })
  
  # Reactive: "Customer Satisfaction"
  satisfaction_data <- reactive({
    christmas_sales
  })
  
  # Chart: "Hourly Sales"
  output$hourlySalesPlot <- renderPlot({
    data <- hourly_sales_data()
    ggplot() +
      geom_line(data = data$online_sales, aes(x = Hour, y = mean_sales, color = "Online"), size = 1.5) +
      geom_line(data = data$offline_sales, aes(x = Hour, y = mean_sales, color = "Offline"), size = 1.5) +
      scale_color_manual(values = c("Online" = "#006400", "Offline" = "#B22222")) +
      labs(title = "Hourly Sales (Online vs Offline)", x = "Hour of Day", y = "Average Sales", color = "Order Type") +
      theme_minimal() +
      theme(plot.title = element_text(color = "darkred", size = 18, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))
  })
  
  # Chart: "Sales by Age and Gender"
  output$ageGenderPlot <- renderPlot({
    data <- age_gender_data()
    ggplot(data, aes(x = Age, y = Gender, fill = mean_sales)) +
      geom_tile() +
      scale_fill_gradient(low = "#F0E68C", high = "#8B0000") +
      labs(title = "Average Sales by Gender and Age", x = "Age", y = "Gender", fill = "Average Sale") +
      theme_minimal() +
      theme(plot.title = element_text(color = "darkgreen", size = 18, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))
  })
  
  # Chart: "Sales vs Weather"
  output$weatherPlot <- renderPlot({
    data <- weather_data()
    ggplot(data, aes(x = Weather, y = mean_sales, fill = Weather)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_text(aes(label = round(mean_sales, 2)), vjust = -0.5, size = 5) +
      scale_fill_manual(values = c("Sunny" = "#FFD700", "Cloudy" = "#ADD8E6", "Rainy" = "#006400")) +
      labs(title = "Average Sale and Weather", x = "Weather", y = "Average Sale", fill = "Weather") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(color = "darkblue", size = 18, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))
  })
  
  # Chart: "Product Category Sales"
  output$categorySalesPlot <- renderPlot({
    data <- category_sales_data()
    ggplot(data, aes(x = ProductName, y = TotalPrice, fill = ProductName)) +
      geom_bar(stat = "summary", fun = "sum", width = 0.7, show.legend = FALSE) +
      geom_text(stat = "summary", fun = "sum", aes(label = round(..y.., 2)), 
                vjust = -0.5, size = 5, color = "black") +
      scale_fill_manual(values = rep("#8B4513", nrow(data))) +
      labs(title = paste("Sales for Category:", input$category), x = "Product", y = "Total Sales") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(color = "goldenrod", size = 18, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))
  })
  
  # Chart: "Customer Satisfaction"
  output$satisfactionPlot <- renderPlot({
    data <- satisfaction_data()
    
    ggplot(data, aes(x = CustomerSatisfaction)) +
      geom_histogram(binwidth = 0.5, fill = "#006400", color = "black", position = "dodge", size = 1) +
      geom_text(stat = "bin", aes(label = ifelse(..count.. > 0, ..count.., "")), 
                vjust = -0.5, size = 5, face = "bold", color = "black") +
      labs(title = "Customer Satisfaction Distribution", x = "Satisfaction Score", y = "Frequency") +
      theme_minimal() +
      theme(plot.title = element_text(color = "goldenrod", size = 18, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))
  })
}

# start app
shinyApp(ui = ui, server = server)
