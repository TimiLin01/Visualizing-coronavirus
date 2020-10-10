library(shiny)
library(readxl)
library(ggplot2)
library(scales)
library(shinythemes)
library(processx)

cases = read.csv("cases201010.csv")
testing = read.csv("testing201009.csv")
deaths = read.csv("deaths201008.csv")
adhospital = read.csv("adhospital201007.csv")
cases$date <- as.Date(cases$date)
testing$date <- as.Date(testing$date)
deaths$date <- as.Date(deaths$date)
adhospital$date <- as.Date(adhospital$date)

ui <- fluidPage(theme = shinytheme("united"),
 fluidRow(
    column(width = 3, wellPanel(
      selectInput("class", "Coronavirus in the UK",
                   c("Cases", "Testing","Patients admitted to hospital", "Deaths")
      ),
      conditionalPanel("input.class === 'Cases'",
                    checkboxInput("moving", "7-day Moving Average", FALSE
                     )
      ),
      conditionalPanel("input.class === 'Testing'",
                     checkboxInput("move", "7-day Moving Average", FALSE
                     )
    ))),
    
    column(width = 7,
           plotOutput("plot1", height = 400,
                      click = "plot_click",
                      dblclick = dblclickOpts(
                        id = "plot_dblclick"
                      )
           )
    )
  )
)


server <- function(input, output) {
  output$plot1 <- renderPlot({
    if (input$class == "Cases") {
      if(input$moving){
        ggplot(data = cases, aes(x = date, group = 1)) + 
        geom_bar(aes(y = newCasesByPublishDate, fill = "Number of cases"),stat = "identity") +
        xlab("Date") + ylab("Daily Cases") +
        ggtitle("Cases in UK (2020)") +
        theme(plot.title = element_text(lineheight=.8, face="bold"))  +
        geom_line(aes(y=movav, colour = "Cases (7-day average)"), size = 1.2, alpha = 0.85) +
        scale_fill_manual(name = "", values = c("Number of cases" = "orange2")) +
        scale_colour_manual(name = "", values = c("Cases (7-day average)" = "sienna"))+
        scale_x_date(date_breaks = "2 month",
                     date_minor_breaks = "1 month",
                     date_labels = "%b")
      }else{
      ggplot(data = cases, aes(x = date, group = 1)) + 
        geom_bar(aes(y = newCasesByPublishDate, fill = "Number of cases"),stat = "identity") +
        xlab("Date") + ylab("Daily Cases") +
        ggtitle("Cases in UK (2020)") +
        theme(plot.title = element_text(lineheight=.8, face="bold"))  +
        scale_fill_manual(name = "", values = c("Number of cases" = "orange2")) +
        scale_x_date(date_breaks = "2 month",
                     date_minor_breaks = "1 month",
                     date_labels = "%b")
    }
    }else if (input$class == "Testing") {
      if(input$move){
        ggplot(data = testing, aes(x = date, group = 1)) + 
        geom_bar(aes(y = newTestsByPublishDate, fill = "Tests processed"), stat = "identity" ) +
        xlab("Date") + ylab("Daily Testing") +
        ggtitle("Testing in UK (2020)") +
        geom_line(aes(y=movav, colour = "Tests processed (7-day average)"), size = 1.2, alpha = 0.7) +
        theme(plot.title = element_text(lineheight=.8, face="bold")) +
        theme(axis.text.y = element_text(size=10)) +
        scale_fill_manual(name = "", values = c("Tests processed" = "steelblue3"))+
        scale_colour_manual(name = "", values = c("Tests processed (7-day average)" = "navy")) +
        scale_x_date(date_breaks = "2 month",
                     date_minor_breaks = "1 month",
                     date_labels = "%b")  +
        scale_y_continuous(labels = comma_format(big.mark = "." , 
                                                 decimal.mark = ","))
      }else{
      ggplot(data = testing, aes(x = date, group = 1)) + 
        geom_bar(aes(y = newTestsByPublishDate, fill = "Tests processed"), stat = "identity" ) +
        xlab("Date") + ylab("Daily Testing") +
        ggtitle("Testing in UK (2020)") +
        theme(plot.title = element_text(lineheight=.8, face="bold")) +
        theme(axis.text.y = element_text(size=10)) +
        scale_fill_manual(name = "", values = c("Tests processed" = "steelblue3"))+
        scale_x_date(date_breaks = "2 month",
                     date_minor_breaks = "1 month",
                     date_labels = "%b")  +
        scale_y_continuous(labels = comma_format(big.mark = "." , 
                                                 decimal.mark = ","))
    }}else if (input$class == "Deaths"){
      ggplot(data = deaths, aes(x = date))+
        geom_bar(aes(y = newDeaths28DaysByDeathDate, fill = factor(areaName, levels=c("Wales", "Scotland", "Northern Ireland","England"))),stat = "identity", position = "stack") +
        scale_x_date(date_breaks = "2 month",
                     date_minor_breaks = "1 month",
                     date_labels = "%b")  +
        xlab("Date")+ylab("Daily Deaths") +
        ggtitle("Deaths within 28 days of positive test by date of death (UK,2020)") +
        theme(plot.title = element_text(lineheight=.8, face="bold"))  +
        labs(fill = "Nation") +
        scale_fill_manual(values=c("midnightblue","darkred","goldenrod1","steelblue3"))
    } else if (input$class == "Patients admitted to hospital"){
      ggplot(data = adhospital, aes(x = date))+
        geom_bar(aes(y = newAdmissions, fill = factor(areaName, levels=c("Wales", "Scotland", "Northern Ireland","England"))),stat = "identity", position = "stack") +
        scale_x_date(date_breaks = "2 month",
                     date_minor_breaks = "1 month",
                     date_labels = "%b")  +
        xlab("Date")+ylab("Daily numbers of patients") +
        ggtitle("COVID-19 Patients admitted to hospital (UK,2020)") +
        theme(plot.title = element_text(lineheight=.8, face="bold"))  +
        labs(fill = "Nation") +
        scale_fill_manual(values=c("midnightblue","darkred","goldenrod1","steelblue3"))
    }
  })

}


shinyApp(ui, server)
