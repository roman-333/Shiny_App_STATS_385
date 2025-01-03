library(tidyverse)
library(zoo)

getwd()
setwd("C:/Users/Roman/OneDrive/Desktop/R_Repo/385_Project(s)/SA385/Data")
#"Default_Data" to "Data_00-24"
#_______________________________________________________________________________
# Brief description: 
# There was quarterly and monthly data for the variables
# The quarterly data had two different data intervals(end of month or beginning of month)
# 1)I split the data the quarterly variables into 3 separate variables, with their
    # respective dates(time intervals)
# I used a spline interpolation conversion to change to quarterly into monthly
# Then combined the new monthly conversion into "converted" variable
    # followed by erasing the old data and repplacing it with "converted"

data = read_csv("Default_Data.csv")
# 1)
# Create 3 separate quarterly variables 
GoldPrice_and_AccountBalance = data %>%  
  select("Gold price (USD)":"Curent Account Balance")
GoldPrice_and_AccountBalance = GoldPrice_and_AccountBalance[,-1]

Debt_To_GDP = data %>% 
  select("Curent Account Balance":"Debt To GDP Ratio")

Account_Balance = GoldPrice_and_AccountBalance[,-2]
Debt_To_GDP = Debt_To_GDP[,-1]
Growth_Rate = GoldPrice_and_AccountBalance[,-3]

# Set all column names the same("Data")
Account_Balance = Account_Balance %>% rename(
   Date = "Date...23"
)
Debt_To_GDP = Debt_To_GDP %>% rename(
  Date = "Date...26"
)
Growth_Rate = Growth_Rate %>% rename(
  Date = "Date...23"
)

Account_Balance$Date = as.Date(Account_Balance$Date, format = "%m/%d/%Y")
Debt_To_GDP$Date = as.Date(Debt_To_GDP$Date, format = "%m/%d/%Y")
Growth_Rate$Date = as.Date(Growth_Rate$Date, format = "%m/%d/%Y")

spline_interpolation = function(data, start_date = "1995-01-01", end_date = "2024-01-01") {
  data = data %>% arrange(Date)
  
  monthly_dates = seq.Date(
    from = as.Date(start_date),
    to = as.Date(end_date),
    by = "month"
  )
  
  x = as.numeric(data$Date)
  y = data[[2]]  # Get the second column values
  
  spline_fit = spline(x, y, n = length(monthly_dates))
  
  result = data.frame(
    Date = monthly_dates,
    Value = spline_fit$y
  )
  
  return(result)
}
# Rename column variables
monthly_gdp = spline_interpolation(Growth_Rate)
names(monthly_gdp)[2] = "GDP Growth Rate"


monthly_balance = spline_interpolation(Account_Balance)
names(monthly_balance)[2] = "Current Account Balance"

monthly_ratio = spline_interpolation(Debt_To_GDP)
names(monthly_ratio)[2] = "Debt To GDP Ratio"

head(monthly_gdp)
head(monthly_balance)
head(monthly_ratio)

converted = cbind(monthly_gdp,
                          monthly_balance[-1],
                          monthly_ratio[-1]
)
# Remove exegesis information 
converted = converted[-1:-62,-1]
head(converted)

#"Data_00-24" extract names
#________________________________________________________________________

# Remove old data and add new data
data = data[,-23:-27]
Data_00_24 = cbind(data, converted)
names(Data_00_24)[5] = "EPS Growth"
names(Data_00_24)[1] = "Date"

# Commented out because further edits are required, CTL+SHIFT+S will cause issues
# Might use the following...
write.csv(data, "Data_00-24.csv", row.names = FALSE)

names = as_tibble(names(Data_00_24))
names = names %>% arrange(names)

#write.csv(data, "names.csv", row.names = FALSE)

