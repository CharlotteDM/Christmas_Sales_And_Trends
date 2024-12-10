library(ggplot2)
library(dplyr)

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
print(path)

setwd(path)

#Source of data: https://www.kaggle.com/datasets/ibikunlegabriel/christmas-sales-and-trends
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

# Podsumowanie wyników testu ANOVA
summary(anova_result)
