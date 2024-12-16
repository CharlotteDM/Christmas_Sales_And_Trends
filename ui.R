library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(shinyWidgets)

# load data
christmas_sales <- read.csv("Christmas Sales and Trends.csv", stringsAsFactors = FALSE)
christmas_sales$Hour <- as.numeric(format(strptime(christmas_sales$Time, format = "%H:%M:%S"), "%H"))




# UI
ui <- dashboardPage(
  skin = "red", 
  dashboardHeader(title = "🎄 Christmas Sales Dashboard 🎅"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Hourly Sales", tabName = "hourly_sales", icon = icon("clock")),
      menuItem("Average Total Price by Age and Gender", tabName = "age_gender", icon = icon("users")),
      menuItem("Average Total Price & Weather", tabName = "weather", icon = icon("cloud")),
      menuItem("Product Category Sales", tabName = "category_sales", icon = icon("tags")),
      menuItem("Customer Satisfaction", tabName = "satisfaction", icon = icon("smile")),
      menuItem("Event Sales", tabName = "event_sales", icon = icon("calendar"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f5f5f5; padding: 0; margin: 0; }
        .main-header .logo { background-color: #b71c1c !important; }
        .skin-red .main-header .navbar { background-color: #d32f2f !important; }
        .skin-red .main-header .navbar .sidebar-toggle { color: #fff; }
        .skin-red .main-header .navbar .navbar-brand { color: #fff; }
        .box { border-radius: 15px; margin: 0; padding: 0; }
        .box-header {
          background-color: #D32F2F !important;  
        }
        .box-title {
          color: white;  
          font-size: 18px;
          font-weight: bold;
        }
        .main-sidebar { position: fixed; width: 250px; }
        .main-panel { margin-left: 250px; }  
        .container-fluid { padding-left: 0; padding-right: 0; }
        .content-wrapper { padding-left: 250px; }
        .shiny-output-error { visibility: hidden; }  
      "))
    ),
    tabItems(
      tabItem(tabName = "hourly_sales", class = "tab-pane",
              fluidRow(
                box(title = "Hourly Sales", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("hourlySalesPlot", height = "400px"))
              )
      ),
      tabItem(tabName = "age_gender", class = "tab-pane",
              fluidRow(
                box(title = "Average Total Price by Age and Gender", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("ageGenderPlot", height = "400px"))
              )
      ),
      tabItem(tabName = "weather", class = "tab-pane",
              fluidRow(
                box(title = "Average Total Price & Weather", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("weatherPlot", height = "400px"))
              )
      ),
      tabItem(tabName = "category_sales", class = "tab-pane",
              fluidRow(
                box(title = "Filter", width = 4, status = "warning",
                    pickerInput("category", "Select Category:", choices = unique(christmas_sales$Category), options = list(`live-search` = TRUE))
                ),
                box(title = "Product Category Sales", width = 8, status = "primary", solidHeader = TRUE,
                    plotOutput("categorySalesPlot", height = "400px"))
              )
      ),
      tabItem(tabName = "satisfaction", class = "tab-pane",
              fluidRow(
                box(title = "Customer Satisfaction Distribution", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("satisfactionPlot", height = "400px"))
              )
      ),
      tabItem(tabName = "event_sales", class = "tab-pane",
              fluidRow(
                box(title = "Event Filter", width = 4, status = "warning",
                    pickerInput("event", "Select Event:", choices = unique(christmas_sales$Event), options = list(`live-search` = TRUE))
                ),
                box(title = "Event Sales (Mean Total Price)", width = 8, status = "primary", solidHeader = TRUE,
                    plotOutput("eventSalesPlot", height = "400px"))
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
  
  # Reactive: przygotowanie danych do wykresów
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
  
  # Reactive: Product Category Sales
  category_sales_data <- reactive({
    req(input$category)
    christmas_sales %>%
      filter(Category == input$category)
  })
  
  # Reactive: "Customer Satisfaction"
  satisfaction_data <- reactive({
    christmas_sales
  })
  
  # Reactive: "Event"
  event_sales_data <- reactive({
    req(input$event)
    christmas_sales %>%
      filter(Event == input$event) %>%
      group_by(Event) %>%
      summarise(mean_total_price = mean(TotalPrice, na.rm = TRUE))
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
            axis.text = element_text(size = 12),
            plot.margin = unit(c(5, 5, 5, 5), "mm"))  
  })
  
  # Chart: "Average Total Price by Age and Gender"
  output$ageGenderPlot <- renderPlot({
    data <- age_gender_data()
    ggplot(data, aes(x = Age, y = Gender, fill = mean_sales)) +
      geom_tile() +
      scale_fill_gradient(low = "#F0E68C", high = "#8B0000") +
      labs(title = "Average Total Price by Gender and Age", x = "Age", y = "Gender", fill = "Average Total Price") +
      theme_minimal() +
      theme(plot.title = element_text(color = "darkgreen", size = 18, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            plot.margin = unit(c(5, 5, 5, 5), "mm"))  
  })
  
  # Chart: "Average Total Price & Weather"
  output$weatherPlot <- renderPlot({
    data <- weather_data()
    ggplot(data, aes(x = Weather, y = mean_sales, fill = Weather)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_text(aes(label = round(mean_sales, 2)), vjust = -0.5, size = 5, color = "black") +
      labs(title = "Average Sales by Weather Condition", x = "Weather Condition", y = "Average Sales") +
      theme_minimal() +
      theme(plot.title = element_text(color = "darkblue", size = 18, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))
  })
  
  # Chart: "Product Category Sales"
  output$categorySalesPlot <- renderPlot({
    data <- category_sales_data()
    ggplot(data, aes(x = Category, y = TotalPrice, fill = Category)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      labs(title = "Sales by Product Category", x = "Category", y = "Total Sales") +
      theme_minimal() +
      theme(plot.title = element_text(color = "darkorange", size = 18, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))
  })
  
  # Chart: "Customer Satisfaction"
  output$satisfactionPlot <- renderPlot({
    data <- satisfaction_data()
    ggplot(data, aes(x = Satisfaction, y = ..density..)) +
      geom_histogram(fill = "#D32F2F", binwidth = 1) +
      labs(title = "Customer Satisfaction Distribution", x = "Satisfaction Level", y = "Density") +
      theme_minimal() +
      theme(plot.title = element_text(color = "goldenrod", size = 18, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))
  })
  
  # Chart: "Event Sales"
  output$eventSalesPlot <- renderPlot({
    data <- event_sales_data()
    ggplot(data, aes(x = Event, y = mean_total_price)) +
      geom_bar(stat = "identity", fill = "darkgreen", show.legend = FALSE) +
      geom_text(aes(label = round(mean_total_price, 2)), vjust = -0.5, size = 5, color = "black") +
      labs(title = paste("Average Total Price for Event:", input$event), x = "Event", y = "Mean Total Price") +
      theme_minimal() +
      theme(plot.title = element_text(color = "goldenrod", size = 18, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))
  })
}

# Uruchomienie aplikacji
shinyApp(ui, server)
