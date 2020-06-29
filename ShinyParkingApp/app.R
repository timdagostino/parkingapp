
library(shiny)
library(janitor)
library(tidyverse)
library(stringr)
library(dplyr)
library(lubridate)
library(DT)
library(scales)
#library (ggthemr)
library (knitr)
library(ggthemes)
library(Cairo)

#Load in Parking Functions
source("parking_rate_functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Revenue Analysis Tool"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            #Upload File
            fileInput("file1", "Step 1: Upload the Completed Transaction template [Found Below] (.csv only)",
                      accept = ".csv"),
            
            #Template
            downloadButton("template1", label = "Download Transaction Template"),
            helpText(h4("Instructions:")),
            helpText(h5("To fill out the template, copy and paste the data necessary as indicated by the column headers. Do NOT delete the column headers.")),
            helpText(h5("Important: When filling in the .csv template with data...")),
            helpText(h5("1. Make sure the Revenue column is in \"Number\" format without any decimals.")),
            helpText(h5("2. Make sure the Exit and Entry columns are in the following data/time format - %m/%d/%Y %H:%M")),
            
            br(),
            
            #Inputs
            numericInput("gp_time", "Length of Grace Period (In Minutes)", value = 10),
            numericInput("fh_rate", "First Hour Rate (If Applicable)", value = 0),
            helpText(h5("Note: If not applicable, please leave value at 0.")),
            numericInput("inc", "Charge Increment (In Minutes)", value = 60),
            helpText(h5("For Example: If customers are charged hourly, the value should be set to 60.")),
            numericInput("inc_rate", "Incremental Rate ($)", value = 0),
            numericInput("dmax", "Daily Max ($)", value = 25),
            numericInput("weekly_rate", "Weekly Rate ($) (If Applicable)", value = 0),
            helpText(h5("Note: If not applicable, please leave value at 0.")),
            numericInput("num_days", "Weekly Rate Applied After ___ Days (If Applicable)", value = 0),
            helpText(h5("Note: If not applicable, please leave value at 0.")),
            selectInput("change", "Rate Change?", choices = c("Positive", "Negative", "No Change")),
            helpText(h5("For Example: If you are testing an increase in rates, choose positive.")),
            #selectInput("rate_type", "Charge Type", choices = c("Rollover", "Hard")),
            #helpText(h5("Rollover: After 24 Hours, the incremental rate structure starts over.")),
            #helpText(h5("Hard: After 24 Hours, the customer is charged the daily max for each additional day.")),
            
            #Action Button to Run Analysis
            actionButton("run",label = "Run")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Summary",
                                 
                                 #Company Logo
                                 column(
                                     width = 12,
                                     align = "center",
                                     img(src = "spplus.jpg", height = 140)),
                                 
                                 fluidRow(
                                     #Revenue Plot
                                     column(6, plotOutput("plot.rev")),
                                     #Sensitivty Table Output
                                     column(6, DTOutput("sensitivity"))
                                     
                                 ),

                            
                                 #Data Output
                                 tags$strong(textOutput("trans_count")),
                                 br(),
                                 tags$strong(textOutput("stay"))
                        ),
                        
                        tabPanel("Data",
                                 
                                 #All Data Output
                                 DTOutput("final")
                                 
                        )
                        
            )
        )
    )
)


server <- function(input, output) {
    
    options(shiny.usecairo=T)
    
    #Template
    output$template1 <- downloadHandler(
        filename <- function() {
            paste("analysis_template", ".csv")
        },
        
        content <- function(file) {
            file.copy("analysis_template.csv", file)
        },
        contentType = "text/csv"
    )
    
    button_push <- observeEvent(input$run, {
        
        #File Inputs Assigned to Objects
        inFile1 <- input$file1
        
        #Cleaning the Data from the Input File
        prelim.data <- read.csv(inFile1$datapath)
        
        #Preliminary Data Cleaning
        lot.data <- prelim.data %>%
            mutate(Ticket.Number = as.character(Ticket.Number)) %>%
            remove_empty(which = c("rows", "cols"))
        
        #Change Entry and Exit Column Formats
        lot.data$Entry <- as.POSIXct(lot.data$Entry, format = "%m/%d/%Y %H:%M")
        lot.data$Exit <- as.POSIXct(lot.data$Exit, format = "%m/%d/%Y %H:%M")
        
        #Calculate total time (minutes)
        lot.data$minutes <- as.numeric(difftime(lot.data$Exit, lot.data$Entry, units = "mins"))
        
        ## convert na values to 0
        lot.data[is.na(lot.data$minutes),"minutes"]<-0
        
        #Calculate Hours
        lot.data$hours <- ceiling(lot.data$minutes/60)
        
        #Calculate Days
        lot.data$days <- ceiling(lot.data$hours/24)
        
        #Calculate Potential Revenue
        lot.data$Potential.Revenue <- mapply(pot_revenue_calc, 
                                             mins = lot.data$minutes, 
                                             gp = input$gp_time, 
                                             fh_rate = input$fh_rate, 
                                             inc = input$inc,
                                             inc_rate = input$inc_rate,
                                             weekly_rate = input$weekly_rate,
                                             num_days = input$num_days,
                                             rollover = TRUE,
                                             dmax = input$dmax)
        
        #create Final Data Table
        final.data <- lot.data %>%
            select(c(Ticket.Number, Entry, Exit, minutes, hours, Revenue, Potential.Revenue))
        
        #Calculate Number of Transactions
        tcount <- nrow(final.data)
        
        output$trans_count <- renderText({
            paste0("Number of Transactions: ", tcount)
        })
        
        output$stay <- renderText ({
            paste0("Average Length of Stay: ", round(mean(final.data$minutes)), " Minutes", " (~", round(mean(final.data$minutes)/60), " Hours)")
        })
        
        #Render the Data Table 
        output$final <- DT::renderDT({datatable(final.data,
                                               extensions = 'Buttons',
                                               options = list(
                                                   columnDefs = list(list(className = 'dt-center')),
                                                   searching = TRUE,
                                                   fixedColumns = TRUE,
                                                   autoWidth = TRUE,
                                                   ordering = TRUE,
                                                   buttons = c('copy', 'csv', 'excel')),
                                               class = "display") %>%
                formatCurrency(c("Revenue", "Potential.Revenue")) })
        
        #Establish Revenue Decrease Multiplier
        percent <- c(seq(.0,.5,.05))
        p <- as.data.frame(percent)
        
        #Convert to Numeric
        final.data$Revenue<-as.numeric(final.data$Revenue)
        
        #Calculate Total Revenues from TRUE revenues and Potential Revenues based on Pricing Inputs
        totalrev <- c(sum(final.data$Revenue,na.rm = TRUE), sum(final.data$Potential.Revenue))
        
        
        #Calculate Total Revenues from TRUE revenues and Potential Revenues based on Pricing Inputs
        totalrev <- c(sum(final.data$Revenue), sum(final.data$Potential.Revenue))
        
        #Calculate Sensitivity
        sens.analysis.a <- as.data.frame(floor(totalrev[1]))
        
        if (input$change == "Positive") {
            sens.analysis.b <- as.data.frame(floor(totalrev[2] - (totalrev[2]*percent)))
        }
        
        if (input$change == "Negative" || input$change == "No Change") {
            sens.analysis.b <- as.data.frame(floor(totalrev[2] + (totalrev[2]*percent)))
        }
        
        sensitivity <- cbind(p, sens.analysis.a, sens.analysis.b)
        
        #Rename Sensitivity Data Table Columns 
        sensitivity <- sensitivity %>%
            rename(Customer.Change = 1, Current = 2, Proposed = 3)
        
        #Generate Table with Sensitivity Calculations
        output$sensitivity <- DT::renderDT({datatable(sensitivity,
                                                      options = list(
                                                          columns.className = list(list(className = 'dt-center')),
                                                          paging = FALSE,
                                                          searching = FALSE,
                                                          fixedColumns = TRUE,
                                                          autoWidth = TRUE,
                                                          ordering = FALSE),
                                                      class = "display") %>%
                formatCurrency(c("Current", "Proposed")) %>%
                formatPercentage(c("Customer.Change")) })
        
        #Generate the Plot
        output$plot.rev <- renderPlot(res = 120,{
            
            ggplot(sensitivity, aes(x = Customer.Change)) +
                geom_line(aes(y=Current, color = "#808080")) +
                geom_line(aes(y=Proposed, color = "#D23139")) +
                geom_text(aes(y = sensitivity[1,2], x = .5, label="Current Revenue", color = "#808080"), vjust = -1, hjust = -.3) +
                geom_text(aes(y = sensitivity[1,3], x = .5, label="Proposed Revenue", color = "#D23139"), vjust = 1, hjust = -.3) +
                geom_point(aes(y=Current, color = "#808080")) +
                geom_point(aes(y=Proposed, color = "#D23139")) +
                labs(subtitle="Understanding Revenue based on Lost Customers", 
                     y="Revenue ($)", 
                     x="Customers Lost/Gained (%)", 
                     title="Sensitivity Analysis") +
                scale_y_continuous(labels=dollar_format(prefix="$")) +
                scale_x_continuous(labels=percent_format()) +
                theme_hc() + 
                theme(plot.margin = unit(c(1,3,1,1), "lines"), legend.position = "none")
            
        })
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
