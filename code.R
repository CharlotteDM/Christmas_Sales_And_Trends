library(ggplot2)
library(dplyr)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
print(path)

setwd(path)

#Source of data: https://zoomcharts.com/en/microsoft-power-bi-custom-visuals/challenges/onyx-data-december-2023?utm_source=challenge&utm_medium=onyxdata&utm_campaign=onyxdata_web_december&utm_term=submit&utm_content=registration
christmas_sales <- read.csv("Christmas Sales and Trends.csv", stringsAsFactors = FALSE)

colnames(christmas_sales)

#average sale for gender and age
heatmap_data <- christmas_sales %>%
  group_by(Gender, Age) %>%
  summarise(mean_sales = mean(TotalPrice, na.rm = TRUE))

ggplot(heatmap_data, aes(x = Age, y = Gender, fill = mean_sales)) +
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Average Sales by Gender and Age", x = "Age", y = "Gender", fill = "Average Sale") +
  theme_minimal()


#average sale and weather

sales_by_weather <- christmas_sales %>%
  group_by(Weather) %>%
  summarise(mean_sales = mean(TotalPrice, na.rm = TRUE))

ggplot(sales_by_weather, aes(x = Weather, y = mean_sales, fill = Weather)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = round(mean_sales, 2)), vjust = -0.5) +
  labs(title = "Average Sale and Weather", 
       x = "Weather", 
       y = "Average Sale", 
       fill = "Weather") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

shapiro_test <- christmas_sales %>%
  group_by(Weather) %>%
  summarise(shapiro_p_value = shapiro.test(TotalPrice)$p.value)

print(shapiro_test)

# Test ANOVA
anova_result <- aov(TotalPrice ~ Weather, data = christmas_sales)
summary(anova_result) #no statistically significant differences



#time and sales
#creating new column
christmas_sales$Hour <- as.numeric(format(strptime(christmas_sales$Time, format = "%H:%M:%S"), "%H"))

#average sales by hour
sales_by_hour <- christmas_sales %>%
  group_by(Hour) %>%
  summarise(mean_sales = round(mean(TotalPrice, na.rm = TRUE), 2))


ggplot(sales_by_hour, aes(x = Hour, y = mean_sales)) +
  geom_line(color = "darkgreen", size = 1) + 
  geom_point(color = "darkred", size = 2) +
  geom_label(aes(label = round(mean_sales, 2)), fill = "white", color = "darkred", size = 3, label.padding = unit(0.15, "lines")) +  
  labs(
    title = "Average Sales by Hour of the Day", 
    x = "Hour of the Day", 
    y = "Average Sales Value",
    caption = "Source: Onyx Data Challenge &\nhttps://www.kaggle.com/datasets/ibikunlegabriel/christmas-sales-and-trends/data"
  ) +
  scale_x_continuous(breaks = sales_by_hour$Hour) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "darkred", size = 14, face = "bold", hjust = 0.5), 
    axis.title.x = element_text(color = "darkred", size = 12), 
    axis.title.y = element_text(color = "darkred", size = 12), 
    plot.caption = element_text(hjust = 0.5, size = 10) 
  )

unique(christmas_sales$Category)
unique((christmas_sales$Location))

#online and offline by hour
online_sales_by_hour <- christmas_sales %>%
  filter(OnlineOrderFlag == TRUE) %>%
  group_by(Hour) %>%
  summarise(mean_sales = round(mean(TotalPrice, na.rm = TRUE), 2))

offline_sales_by_hour <- christmas_sales %>%
  filter(OnlineOrderFlag == FALSE) %>%
  group_by(Hour) %>%
  summarise(mean_sales = round(mean(TotalPrice, na.rm = TRUE), 2))

# Plot for Online Sales
ggplot(online_sales_by_hour, aes(x = Hour, y = mean_sales)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(color = "darkblue", size = 2) +
  geom_label(aes(label = round(mean_sales, 2)), fill = "white", color = "darkblue", size = 3, label.padding = unit(0.15, "lines")) +  
  labs(
    title = "Average Online Sales by Hour of the Day", 
    x = "Hour of the Day", 
    y = "Average Sales Value",
    caption = "Source: Onyx Data Challenge &\nhttps://www.kaggle.com/datasets/ibikunlegabriel/christmas-sales-and-trends/data") +
  scale_x_continuous(breaks = online_sales_by_hour$Hour) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "darkblue", size = 14, face = "bold", hjust = 0.5), 
    axis.title.x = element_text(color = "darkblue", size = 12), 
    axis.title.y = element_text(color = "darkblue", size = 12), 
    plot.caption = element_text(hjust = 0.5, size = 10) 
  )

# Plot for Offline Sales
ggplot(offline_sales_by_hour, aes(x = Hour, y = mean_sales)) +
  geom_line(color = "green", size = 1) + 
  geom_point(color = "darkgreen", size = 2) +
  geom_label(aes(label = round(mean_sales, 2)), fill = "white", color = "darkgreen", size = 3, label.padding = unit(0.15, "lines")) +  
  labs(
    title = "Average Offline Sales by Hour of the Day", 
    x = "Hour of the Day", 
    y = "Average Sales Value",
    caption = "Source: Onyx Data Challenge &\nhttps://www.kaggle.com/datasets/ibikunlegabriel/christmas-sales-and-trends/data"
  ) +
  scale_x_continuous(breaks = offline_sales_by_hour$Hour) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "darkgreen", size = 14, face = "bold", hjust = 0.5), 
    axis.title.x = element_text(color = "darkgreen", size = 12), 
    axis.title.y = element_text(color = "darkgreen", size = 12), 
    plot.caption = element_text(hjust = 0.5, size = 10) 
  )

