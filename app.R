library(shiny)
library(shinydashboard)
library(shinythemes)
library(readr)
library(plotly)
library(tidyverse)
library(reshape2)
library(ggpubr)

# ------------------ Load Data Sets ------------------

Year <- 2013:2018
df.year <- readRDS("PaymentsByYear.RDS")
df.long <- readRDS("PaymentsByYear-Long.RDS")
df.individual <- readRDS("PaymentsByIndividual.RDS")
df.dept <- readRDS("PaymentsByDept.RDS")
df.benefit <- readRDS("BenefitsByDept.RDS")

# ------------------ Functions ------------------

# function to plot a bar for a specific year
plot.year <- function(df.year, year){
  df.year.long <- df.year %>%
    filter(Year == year) %>%
    melt(id.vars = "Year")
  colnames(df.year.long) <- c("Year", "Payment", "Amount")
  ggplotly(ggplot(data = df.year.long, aes(x=Year, y=Amount, fill=Payment)) +
             geom_bar(stat="identity") + coord_flip() +
             scale_x_discrete(limits = rev(levels(df.year.long$Year))) + xlab(NULL))
}

# function to plot bar plots for individuals
plot.individual <- function(df.individual, year, n) {
  df.individual.long <- df.individual %>%
    filter(Year == year) %>% # reaplce year
    select(`ID`, `Base Pay`, `Overtime Pay`, `Other Pay`) %>%
    slice(1:n) %>% # replace n
    cbind("Index"=1:n) %>% #replace n
    within(ID <- paste(`Index`, `ID`, sep="-")) %>%
    select(ID, `Base Pay`, `Overtime Pay`, `Other Pay`)
  df.individual.long$ID <- factor(df.individual.long$ID, as.character(df.individual.long$ID))
  df.individual.long <- melt(df.individual.long, id.vars="ID")
  colnames(df.individual.long) <- c("ID", "Payment", "Amount")
  ggplotly(ggplot(data = df.individual.long, aes(x=ID, y=Amount, fill=Payment)) + 
             geom_bar(stat="identity") + coord_flip() + 
             scale_x_discrete(limits = rev(levels(df.individual.long$ID))) + xlab(NULL))
}

# function to plot bar plots for departments
plot.dept <- function(df.dept, year, n) {
  df.dept.long <- df.dept %>%
    filter(Year == year) %>% 
    select(`Department`, `Base Pay`, `Overtime Pay`, `Other Pay`) %>%
    slice(1:n) %>% 
    cbind("Index"=1:n) %>% 
    within(ID <- paste(`Index`, `Department`, sep="-")) %>%
    select(ID, `Base Pay`, `Overtime Pay`, `Other Pay`)
  df.dept.long$ID <- factor(df.dept.long$ID, as.character(df.dept.long$ID))
  df.dept.long <- melt(df.dept.long, id.vars="ID")
  colnames(df.dept.long) <- c("ID", "Payment", "Amount")
  ggplotly(ggplot(data = df.dept.long, aes(x=ID, y=Amount, fill=Payment)) +
             geom_bar(stat="identity") + coord_flip() +
             scale_x_discrete(limits = rev(levels(df.dept.long$ID))) + xlab(NULL))
}

# function to plot median box plots for departments
plot.median <- function(df.dept, year, n){
  df.dept.long <- df.dept %>%
    filter(Year == year) %>%
    select(Year, Total, `Base Pay`, `Overtime Pay`, `Other Pay`) %>%
    slice(1:n) %>%
    melt(id.vars="Year")
  colnames(df.dept.long) <- c("Year", "Payment", "Amount")
  ggplotly(ggplot(df.dept.long, aes(x=Payment, y=Amount, fill=Payment)) + geom_boxplot())
}

# function to plot mean's 95 CI for departments
plot.mean <- function(df.dept, year, n){
  df.dept.long <- df.dept %>%
    filter(Year == year) %>%
    select(Year, Total, `Base Pay`, `Overtime Pay`, `Other Pay`) %>%
    slice(1:n) %>%
    melt(id.vars="Year")
  colnames(df.dept.long) <- c("Year", "Payment", "Mean")
  ggplotly(ggerrorplot(df.dept.long, x = "Payment", y = "Mean",
                       desc_stat = "mean_ci", color = "Payment") + theme_minimal())
}

# function to plot benefits cost
plot.benefit <- function(df.benefit, year, n) {
  df.benefit.long <- df.benefit %>%
    filter(Year == year) %>%
    select(`Department`, `Health Cost`, `Dental Cost`, `Basic Life`) %>%
    slice(1:n) %>%
    cbind("Index"=1:n) %>%
    within(ID <- paste(`Index`, `Department`, sep="-")) %>%
    select(ID, `Health Cost`, `Dental Cost`, `Basic Life`)
  df.benefit.long$ID <- factor(df.benefit.long$ID, as.character(df.benefit.long$ID))
  df.benefit.long <- melt(df.benefit.long, id.vars="ID")
  colnames(df.benefit.long) <- c("ID", "Cost", "Amount")
  ggplotly(ggplot(data = df.benefit.long, aes(x=ID, y=Amount, fill=Cost)) +
             geom_bar(stat="identity") + coord_flip() +
             scale_x_discrete(limits = rev(levels(df.benefit.long$ID))) + xlab(NULL))
}


# ------------------ UI ------------------

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "LA City Payroll"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Total Payroll", tabName = "total", icon = icon("users")),
      menuItem("Individual Earnings", tabName = "individual", icon = icon("user")),
      menuItem("Departmental Earnings", tabName = "dept", icon = icon("building")),
      menuItem("Departmental Benefit Costs", tabName = "benefit", icon = icon("briefcase-medical"))
    )
  ),
  dashboardBody(
    tabItems(
      
      # first tab: total payroll
      tabItem(tabName = "total",
              tabsetPanel(
                tabPanel("Overview",
                         plotlyOutput("plotOverview")),
                tabPanel("2013",
                         plotlyOutput("plot2013")),
                tabPanel("2014",
                         plotlyOutput("plot2014")),
                tabPanel("2015",
                         plotlyOutput("plot2015")),
                tabPanel("2016",
                         plotlyOutput("plot2016")),
                tabPanel("2017",
                         plotlyOutput("plot2017")),
                tabPanel("2018",
                         plotlyOutput("plot2018"))
              )  
      ),
      
      # second tab: individual employee
      tabItem(tabName = "individual",
              fluidRow(
                column(3,
                       box(width = NULL,
                           selectInput("individualYear", h3("Year Selection"), 
                                       choices = c("2013" = 2013,
                                                   "2014" = 2014,
                                                   "2015" = 2015,
                                                   "2016" = 2016,
                                                   "2017" = 2017,
                                                   "2018" = 2018),
                                       selected = "2017"),
                           numericInput("individualNum", h3("Number of Top Earning Employees"), 
                                        value = 10))
                ),
                column(9,
                       box(width = NULL, solidHeader = TRUE,
                           plotlyOutput("plotIndividual")
                       )
                )
              )
      ),
      
      # third tab: departmental payroll
      tabItem(tabName = "dept",
              fluidRow(
                column(3,
                       box(width = NULL,
                           selectInput("deptYear", h3("Year Selection"), 
                                       choices = c("2013" = 2013,
                                                   "2014" = 2014,
                                                   "2015" = 2015,
                                                   "2016" = 2016,
                                                   "2017" = 2017,
                                                   "2018" = 2018),
                                       selected = "2017"),
                           selectInput("method", h3("Method Selection"), 
                                       choices = c("Mean",
                                                   "Median"),
                                       selected = "Median"),
                           numericInput("deptNum", h3("Number of Top Earning Departments"), 
                                        value = 5))
                ),
                column(9,
                       box(width = NULL, solidHeader = TRUE,
                           tabsetPanel(
                             tabPanel("Mean/Median Box Plot",
                                      plotlyOutput("plotDeptBox")),
                             tabPanel("Aggregate Bar Charts",
                                      plotlyOutput("plotDept"))
                           )
                       )
                )
              )
      ),
      # fourth tab: departmental benefit
      tabItem(tabName = "benefit",
              fluidRow(
                column(3,
                       box(width = NULL,
                           selectInput("deptBenefitYear", h3("Year Selection"), 
                                       choices = c("2013" = 2013,
                                                   "2014" = 2014,
                                                   "2015" = 2015,
                                                   "2016" = 2016,
                                                   "2017" = 2017,
                                                   "2018" = 2018),
                                       selected = "2017"),
                           numericInput("deptBenefitNum", h3("Number of Top Costing Departments"), 
                                        value = 5))
                ),
                column(9,
                       box(width = NULL, solidHeader = TRUE,
                           plotlyOutput("plotBenefit")
                       )
                )
              )
      )
    )
  )
)


# ------------------ Server ------------------

server <- function(input, output) {
  # tab 1
  output$plotOverview <- renderPlotly({
    ggplotly(ggplot(data = df.long, aes(x=Year, y=Amount, fill=Payment)) +
               geom_bar(stat="identity") +
               scale_x_continuous(breaks = Year))
  })
  output$plot2013 <- renderPlotly({
    plot.year(df.year, 2013)
  })
  output$plot2014 <- renderPlotly({
    plot.year(df.year, 2014)
  })
  output$plot2015 <- renderPlotly({
    plot.year(df.year, 2015)
  })
  output$plot2016 <- renderPlotly({
    plot.year(df.year, 2016)
  })
  output$plot2017 <- renderPlotly({
    plot.year(df.year, 2017)
  })
  output$plot2018 <- renderPlotly({
    plot.year(df.year, 2018)
  })
  
  # tab 2
  output$plotIndividual <- renderPlotly({
    plot.individual(df.individual, input$individualYear, input$individualNum)
  })
  
  # tab 3
  output$plotDeptBox <- renderPlotly({
    if(input$method == "Median"){
      plot.median(df.dept, input$deptYear, input$deptNum)
    }else{
      plot.mean(df.dept, input$deptYear, input$deptNum)
    }
  })
  output$plotDept <- renderPlotly({
    plot.dept(df.dept, input$deptYear, input$deptNum)
  })
  
  # tab 4
  output$plotBenefit <- renderPlotly({
    plot.benefit(df.benefit, input$deptBenefitYear, input$deptBenefitNum)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
