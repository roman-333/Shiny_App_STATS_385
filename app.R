library(shiny)
library(tidyverse)
library(bslib)
library(DT)

setwd("C:/Users/Roman/OneDrive/Desktop/R_Repo/385_Project(s)/SA385")
getwd()

df = read.csv("Data/Data_00-24.csv")

ui = navbarPage(
  " Stock Performance Dashboard",
  
  tabPanel("Dashboard",
           sidebarLayout(
             sidebarPanel(
               selectInput("plot_type", "Select Plot Type:",
                           choices = c("Line Plot", "Bar Plot", "Box Plot")),
               selectizeInput("variable", "Enter Stock:",
                            choices = NULL,  
                            selected = "PMI.Index",
                            options = list(
                              create = FALSE,  
                              maxOptions = 100,
                              searchField = c("value", "label"),
                              searchConjunction = "or"
                            )
               ),
               selectInput("color_var", "Color Type:",
                           choices = c("None" = "darkslategray4", 
                                       "Blue" = "steelblue",
                                       "Red" = "firebrick",
                                       "Green"= "forestgreen")),
               
               radioButtons("graph_type", "Select Graph Type:",
                            choices = c("ggplot", "base R")),
               
               downloadButton("download_plot", "Download Plot"),
               dataTableOutput("dataFrame")
             ),
             
             mainPanel(
               plotOutput("main_plot", height = "420px")
             )
           ),
           fluidRow(
             column(12,
                    h3("Data Table"),
                    dataTableOutput("dataFrame")
             )
           )
  ),
  
  tabPanel("About",
           fluidRow(
             column(12,
                    h2("About This Dashboard"),
                    p("This dashboard visualizes economic indicators and individual stocks from 2000-2024,
                      allowing users to explore trends and relationships in key economic metrics."),
                    p("The dataset was created using Bloomberg with the initial purpose of serving as the
                      foundation for a stock portfolio."),
                    h3("Dataset Description"),
                    p("The dataset contains 26 stocks/economic indicators plus the corresponding dates and 287 entries."),
                    tags$ul(
                      tags$li("Consumer Confidence Index"),
                      tags$li("CPI (Consumer Price Index)"),
                      tags$li("Unemployment Rate"),
                      tags$li("GDP Growth Rate"),
                      tags$li("CPI Index"),
                      tags$li("USD/EUR")
                    ),
                    h3("Developer Information"),
                    p("Created by: Bryan Roman"),
                    p("Contact information: broman8@illinois.edu"),
                    p("Version: 1.0"),
                    p("Last Updated: 12/15/2024")
             )
           )
  ))


server = function(input, output, session) {
    observe({
      choices = setNames(
        names(df),  
        gsub("\\.", " ", names(df)) 
      )
    
    choices = choices[!names(choices) %in% c("Date", "Year")]
    
    updateSelectizeInput(
        inputId = "variable",
        session = session,
        choices = choices,
        selected = "PMI.Index",
        server = TRUE,
        options = list(
          placeholder = "e.g SP500 ETF",
          onDropdownOpen = I("function($dropdown) 
                             {if (!this.lastQuery.length) {this.close(); 
                             this.settings.openOnFocus = false;}}"),
          onType = I("function (str) {if (str === \"\") {this.close();}}")
        )
     )
  })
  
  data = reactive({
      df$Date = as.Date(paste0(df$Date, "-01"), format = "%m/%d/%Y")
      df$Year = year(df$Date)
      df
  })
  
  output$main_plot = renderPlot({
      df = data()
  
# Special case for EPS Growth, percentage -----------------------------------
  if(input$variable == "EPS.Growth") {
    df$EPS.Growth = as.numeric(gsub("%", "", df$EPS.Growth))
    
    if(input$graph_type == "ggplot") {
      p = ggplot(df, aes(x = Year, y = EPS.Growth)) +
        theme_minimal() +
        labs(title = "Annual EPS Growth",
             x = "Year",
             y = "EPS Growth (%)") +
        scale_y_continuous(labels = scales::label_percent(scale = 1))
      
      if(input$plot_type == "Line Plot") {
        p = p + geom_line(color = input$color_var)
      } else if(input$plot_type == "Bar Plot") {
        p = p + geom_col(fill = input$color_var)
      } else {
        p = p + geom_boxplot(aes(group = Year), fill = input$color_var)
      }
      print(p)
      
    } else { # Base R
      if(input$plot_type == "Line Plot") {
        plot(df$Year, df$EPS.Growth,
             type = "l",
             col = input$color_var,
             main = "Annual EPS Growth",
             xlab = "Year",
             ylab = "EPS Growth (%)")
        axis(2, at = axTicks(2), labels = paste0(axTicks(2), "%"))
        
      } else if(input$plot_type == "Bar Plot") {
        barplot(tapply(df$EPS.Growth, df$Year, mean),
                col = input$color_var,
                main = "Annual EPS Growth",
                xlab = "Year",
                ylab = "EPS Growth (%)")
        axis(2, at = axTicks(2), labels = paste0(axTicks(2), "%"))
        
      } else {
        boxplot(EPS.Growth ~ Year,
                data = df,
                col = input$color_var,
                main = "Annual EPS Growth",
                xlab = "Year",
                ylab = "EPS Growth (%)")
        axis(2, at = axTicks(2), labels = paste0(axTicks(2), "%"))
      }
    }
  }
# End of special case
#---------------------------------------------------------------------------      
      
    # ggplot
    if(input$graph_type == "ggplot") {
      p = ggplot(df, aes_string(x = "Year", y = input$variable)) +
        theme_minimal() +
        labs(title = paste("Annual", gsub("\\.", " ", input$variable)),
             x = "Year",
             y = gsub("\\.", " ", input$variable))
      
      if(input$plot_type == "Line Plot") {
          p = p + geom_line(color = input$color_var)
      } else if(input$plot_type == "Bar Plot") {
          p = p + geom_col(fill = input$color_var)
      } else {
          p = p + geom_boxplot(aes(group = Year), fill = input$color_var)
      }
      print(p)  
      
    } else {
      # Base R 
      if(input$plot_type == "Line Plot") {
        plot(df$Year, df[[input$variable]], type = "l", 
             col = input$color_var,
             main = paste("Annual", gsub("\\.", " ", input$variable)),
             xlab = "Year", ylab = gsub("\\.", " ", input$variable))
      } else if(input$plot_type == "Bar Plot") {
        barplot(tapply(df[[input$variable]], df$Year, mean),
             col = input$color_var,
             main = paste("Annual", gsub("\\.", " ", input$variable)),
             xlab = "Year", ylab = gsub("\\.", " ", input$variable))
      } else {
        boxplot(df[[input$variable]] ~ df$Year,
             col = input$color_var,
             main = paste("Annual", gsub("\\.", " ", input$variable)),
             xlab = "Year", ylab = gsub("\\.", " ", input$variable))
             }
      }
    }
  )
  output$dataFrame = DT:: renderDataTable(df)
  
  output$download_plot = downloadHandler(
    filename = function() {
      paste0(gsub("\\.", "_", input$variable), "_", 
             gsub(" ", "_", input$plot_type), "_", 
             format(Sys.Date(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      if(input$graph_type == "ggplot") {
        # For ggplot, use ggsave
        ggsave(file, width = 10, height = 7, units = "in", dpi = 300, device = "png", {
          df = data()
          
          # Special case for EPS Growth
          if(input$variable == "EPS.Growth") {
            df$EPS.Growth = as.numeric(gsub("%", "", df$EPS.Growth))
            p = ggplot(df, aes(x = Year, y = EPS.Growth)) +
              theme_minimal() +
              labs(title = "Annual EPS Growth",
                   x = "Year",
                   y = "EPS Growth (%)") +
              scale_y_continuous(labels = scales::label_percent(scale = 1))
            
            if(input$plot_type == "Line Plot") {
              p = p + geom_line(color = input$color_var)
            } else if(input$plot_type == "Bar Plot") {
              p = p + geom_col(fill = input$color_var)
            } else {
              p = p + geom_boxplot(aes(group = Year), fill = input$color_var)
            }
            p
          } else {
            p = ggplot(df, aes_string(x = "Year", y = input$variable)) +
              theme_minimal() +
              labs(title = paste("Annual", gsub("\\.", " ", input$variable)),
                   x = "Year",
                   y = gsub("\\.", " ", input$variable))
            
            if(input$plot_type == "Line Plot") {
              p = p + geom_line(color = input$color_var)
            } else if(input$plot_type == "Bar Plot") {
              p = p + geom_col(fill = input$color_var)
            } else {
              p = p + geom_boxplot(aes(group = Year), fill = input$color_var)
            }
            p
          }
        })
      } else {
        # For base R, use png device
        png(filename = file, width = 1000, height = 700)
        
        df = data()
        if(input$variable == "EPS.Growth") {
          df$EPS.Growth = as.numeric(gsub("%", "", df$EPS.Growth))
          if(input$plot_type == "Line Plot") {
            plot(df$Year, df$EPS.Growth,
                 type = "l",
                 col = input$color_var,
                 main = "Annual EPS Growth",
                 xlab = "Year",
                 ylab = "EPS Growth (%)")
            axis(2, at = axTicks(2), labels = paste0(axTicks(2), "%"))
          } else if(input$plot_type == "Bar Plot") {
            barplot(tapply(df$EPS.Growth, df$Year, mean),
                    col = input$color_var,
                    main = "Annual EPS Growth",
                    xlab = "Year",
                    ylab = "EPS Growth (%)")
            axis(2, at = axTicks(2), labels = paste0(axTicks(2), "%"))
          } else {
            boxplot(EPS.Growth ~ Year,
                    data = df,
                    col = input$color_var,
                    main = "Annual EPS Growth",
                    xlab = "Year",
                    ylab = "EPS Growth (%)")
            axis(2, at = axTicks(2), labels = paste0(axTicks(2), "%"))
          }
        } else {
          if(input$plot_type == "Line Plot") {
            plot(df$Year, df[[input$variable]], type = "l",
                 col = input$color_var,
                 main = paste("Annual", gsub("\\.", " ", input$variable)),
                 xlab = "Year", ylab = gsub("\\.", " ", input$variable))
          } else if(input$plot_type == "Bar Plot") {
            barplot(tapply(df[[input$variable]], df$Year, mean),
                    col = input$color_var,
                    main = paste("Annual", gsub("\\.", " ", input$variable)),
                    xlab = "Year", ylab = gsub("\\.", " ", input$variable))
          } else {
            boxplot(df[[input$variable]] ~ df$Year,
                    col = input$color_var,
                    main = paste("Annual", gsub("\\.", " ", input$variable)),
                    xlab = "Year", ylab = gsub("\\.", " ", input$variable))
          }
        }
        dev.off()
      }
    }
  )
}
  

# Run the app
shinyApp(ui = ui, server = server)