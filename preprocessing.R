library(tidyverse)
library(readxl)
library(tidymodels)
library(readxl)
library(caTools)
library(xlsx)

# Get DataFrame
df <- read_excel("./energy_data.xlsx")
View(df)

df <- df$`11:00`
View(df)
str(df)
plot(df)

# Partial Autocorelation plot
pacf(x = df, plot = TRUE)

# Create Lags for Time Series
lag_1 = lag(df,1)
lag_2 = lag(df,2)
lag_3 = lag(df,3)
lag_4 = lag(df,4)
lag_5 = lag(df,5)
df <- cbind(df,lag_1,lag_2,lag_3,lag_4,lag_5)

# Formatting df
df <- na.omit(df)
sum(is.na(df))

colnames(df) <- c("original","v2","v3","v4","v5","v6")

# Scale Data
df_scaled <- scale(df)
str(df_scaled)

# Save processed data to excel file
write.xlsx(df_scaled,
           file = "./powerUsage-scaled.xlsx",
           col.names = TRUE, append = TRUE, row.names = FALSE)
