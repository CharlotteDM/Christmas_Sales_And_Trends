path <- dirname(rstudioapi::getActiveDocumentContext()$path)
print(path)

setwd(path)

#Source of data: https://www.kaggle.com/datasets/ibikunlegabriel/christmas-sales-and-trends
christmas_sales <- read.csv("Christmas Sales and Trends.csv", stringsAsFactors = FALSE)
