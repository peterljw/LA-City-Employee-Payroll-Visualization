library(readr)
library(tidyverse)
library(reshape2)

df <- read_csv("/home/m280data/la_payroll/City_Employee_Payroll.csv")


# Wide data of all types of payments for every year
years <- unique(df$Year)
payments <- list()
for(year in years) {
  payments[[toString(year)]] <- df %>%
    filter(Year == year) %>%
    select(`Total Payments`, `Base Pay`, `Overtime Pay`, `Permanent Bonus Pay`, `Longevity Bonus Pay`, 
           `Temporary Bonus Pay`, `Lump Sum Pay`, `Other Pay & Adjustments`) %>%
    sapply(function(x) sum(x, na.rm = T))
}
df.year <- data.frame(matrix(unlist(payments), nrow=length(years), byrow=T),stringsAsFactors=FALSE)
colnames(df.year) <- c("Total","Base Pay", "Overtime Pay", "Permanent Bonus Pay", "Longevity Bonus Pay", 
                       "Temporary Bonus Pay", "Lump Sum Pay", "Other Pay & Adjustments")
df.year <- cbind("Year" = years, df.year)
# saveRDS(df.year, "/home/peterljw/biostat-m280-2019-winter/hw3/LA-City-Employee-Payroll/PaymentsByYear.RDS")

# Long data of basic, overtime, and other for every year
long.df <- melt(df.year, id.vars='Year')
colnames(long.df) <- c("Year", "Payment", "Amount")
# saveRDS(long.df, "/home/peterljw/biostat-m280-2019-winter/hw3/LA-City-Employee-Payroll/PaymentsByYear-Long.RDS")

 
# Wide data of basic, overtime, and other for every employee 
# (combined title and department into ID; sorted in descending order) 
df <- df[order(-df$`Total Payments`),]
df.individual <- within(df,  ID <- paste(`Job Class Title`, `Department Title`, sep="-"))
df.individual <- df.individual %>%
  select(`Year`, `Total Payments`, `Base Pay`, `Overtime Pay`, `Other Pay (Payroll Explorer)`, `ID`)
colnames(df.individual) <- c("Year", "Total", "Base Pay", "Overtime Pay", "Other Pay", "ID")
# saveRDS(df.individual, "/home/peterljw/biostat-m280-2019-winter/hw3/LA-City-Employee-Payroll/PaymentsByIndividual.RDS")


# Wide data of basic, overtime, and other for every department (sorted in descending order)
depts <- unique(df$`Department Title`)
years <- unique(df$Year)
df.dept <- c()
for(dept in depts) {
  for(year in years) {
    sums <- df %>%
      filter(`Department Title` == dept & Year == year) %>%
      select(`Total Payments`, `Base Pay`, `Overtime Pay`, `Other Pay (Payroll Explorer)`) %>%
      sapply(function(x) sum(x, na.rm = T))
    r <- c(dept, year, unlist(sums))
    df.dept <- rbind(df.dept, r)
  }
}
df.dept <- data.frame(df.dept, stringsAsFactors=FALSE)
df.dept[,3:6] <- sapply(df.dept[,3:6], function(x) as.numeric(as.character(unlist(x))))
colnames(df.dept) <- c("Department", "Year", "Total", "Base Pay", "Overtime Pay", "Other Pay")
df.dept <- df.dept[order(-df.dept$Total),]

# saveRDS(df.dept, "/home/peterljw/biostat-m280-2019-winter/hw3/LA-City-Employee-Payroll/PaymentsByDept.RDS")


# Wide data of health, dental, and basic life cost for every department (sorted in descending order)
depts <- unique(df$`Department Title`)
years <- unique(df$Year)
df.benefit <- c()
for(dept in depts) {
  for(year in years) {
    sums <- df %>%
      filter(`Department Title` == dept & Year == year) %>%
      select(`Average Benefit Cost`, `Average Health Cost`, `Average Dental Cost`, `Average Basic Life`) %>%
      sapply(function(x) sum(x, na.rm = T))
    r <- c(dept, year, unlist(sums))
    df.benefit <- rbind(df.benefit, r)
  }
}
df.benefit <- data.frame(df.benefit, stringsAsFactors=FALSE)
df.benefit[,3:6] <- sapply(df.benefit[,3:6], function(x) as.numeric(as.character(unlist(x))))
colnames(df.benefit) <- c("Department", "Year", "Total", "Health Cost", "Dental Cost", "Basic Life")
df.benefit <- df.benefit[order(-df.benefit$Total),]

# saveRDS(df.benefit, "/home/peterljw/biostat-m280-2019-winter/hw3/LA-City-Employee-Payroll/BenefitsByDept.RDS")
