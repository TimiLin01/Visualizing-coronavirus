library(shiny)
library(readxl)
library(ggplot2)
library(scales)
library(shinythemes)
library(processx)

Sys.setlocale("LC_ALL", "English")
cases = read.csv("cases201010.csv")
testing = read.csv("testing201009.csv")
deaths = read.csv("deaths201014.csv")
adhospital = read.csv("adhospital201014.csv")
cases$date <- as.Date(cases$date)
testing$date <- as.Date(testing$date)
deaths$date <- as.Date(deaths$date)
adhospital$date <- as.Date(adhospital$date)

ui <- fluidPage(theme = shinytheme("united"),
 titlePanel("Coronavirus in the UK"),
 p("The coronavirus is still spreading around the world today, which affects more than 200 countries.
   This report aims to examine the COVID-19 in the UK, hence by collecting data from the official governmental coronavirus website of the UK(",
   a("https://coronavirus.data.gov.uk/",
     href = "https://coronavirus.data.gov.uk/"),
   ") I made the following graphs in R using ggplot."),
 sidebarLayout(
   sidebarPanel(position = "left",
      selectInput("class", "Plot:",
                   c("Cases", "Testing","Patients admitted to hospital", "Deaths")
      ),
      conditionalPanel("input.class === 'Cases'",
                    checkboxInput("move1", "7-day Moving Average", FALSE),
                    h4("Daily and Cumulative Cases in the UK"),
                    p("This plot shows the exact number of daily and cumulative cases. By ticking the checkbox, we can see the 7-day moving average line, which shows the trending." ),
                    p("Daily cases already reached its peak once in May, but recently it goes upward again so that we may assume that in the next few months the United Kingdom will probably have a second peak.")
      ),
      conditionalPanel("input.class === 'Testing'",
                     checkboxInput("move2", "7-day Moving Average", FALSE),
                     h4("Daily and Cumulative Testing in the UK"),
                     p("This plot shows the exact number of daily and cumulative testing. By ticking the checkbox, we can see the 7-day moving average line, which shows the trending."),
                     p("Unlike daily cases, this daily bar chart overall shows an increasing trend. Since we assumed that UK will have its second peak than we can expect that the testing will remain increasing.")
      ),
      conditionalPanel("input.class === 'Patients admitted to hospital'",
                     checkboxInput("move3", "7-day Moving Average", FALSE),
                     h4("Daily and Cumulative number of patients"),
                     p("This bar chart gives an overall idea of the daily and cumulative number of patients admitted to hospital.")
      ),
      conditionalPanel("input.class === 'Deaths'",
                     checkboxInput("move4", "7-day Moving Average", FALSE),
                     h4("Daily and Cumulative deaths"),
                     p("This bar chart gives an overall idea of the daily and cumulative number of deaths.")
      )
   ),
   mainPanel(width = 8,
      tabsetPanel(
        tabPanel("Daily", plotOutput("plot1", height = 500)), 
        tabPanel("Cumulative", plotOutput("plot2", height = 500))
     )
 )
)
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    if (input$class == "Cases") {
      if(input$move1){
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
      if(input$move2){
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
      if(input$move4){
      ggplot(data = deaths, aes(x = date))+
        geom_bar(aes(y = newDeaths28DaysByDeathDate,fill = "Deaths"),stat = "identity", position = "stack") +
        geom_line(aes(y=movav, colour = "Deaths (7-day average)"), size = 1.2, alpha = 0.9) +
        scale_x_date(date_breaks = "2 month",
                     date_minor_breaks = "1 month",
                     date_labels = "%b")  +
        xlab("Date")+ylab("Daily Deaths") +
        ggtitle("Deaths within 28 days of positive test by date of death (UK,2020)") +
        theme(plot.title = element_text(lineheight=.8, face="bold"))+
        scale_fill_manual(name = "", values = c("Deaths" = "firebrick3"))+
        scale_colour_manual(name = "", values = c("Deaths (7-day average)" = "coral4"))
      }else{
      ggplot(data = deaths, aes(x = date))+
        geom_bar(aes(y = newDeaths28DaysByDeathDate,fill = "Deaths"),stat = "identity", position = "stack") +
        scale_x_date(date_breaks = "2 month",
                     date_minor_breaks = "1 month",
                     date_labels = "%b")  +
        xlab("Date")+ylab("Daily Deaths") +
        ggtitle("Deaths within 28 days of positive test by date of death (UK,2020)") +
        theme(plot.title = element_text(lineheight=.8, face="bold"))+
        scale_fill_manual(name = "", values = c("Deaths" = "firebrick3"))
        }
    } else if (input$class == "Patients admitted to hospital"){
      if(input$move3){
        ggplot(data = adhospital, aes(x = date))+
        geom_bar(aes(y = newAdmissions,fill = "Patients"), stat = "identity", position = "stack") +
        geom_line(aes(y=movav, colour = "Patients (7-day average)"), size = 1.2, alpha = 0.7) +
        scale_x_date(date_breaks = "2 month",
                     date_minor_breaks = "1 month",
                     date_labels = "%b")  +
        xlab("Date")+ylab("Daily numbers of patients") +
        scale_fill_manual(name = "", values = c("Patients" = "steelblue"))+
        scale_colour_manual(name = "", values = c("Patients (7-day average)" = "navy"))+
        ggtitle("COVID-19 Patients admitted to hospital (UK,2020)") +
        theme(plot.title = element_text(lineheight=.8, face="bold"))
        }else{
      ggplot(data = adhospital, aes(x = date))+
        geom_bar(aes(y = newAdmissions,fill = "Patients"), stat = "identity", position = "stack") +
        scale_x_date(date_breaks = "2 month",
                     date_minor_breaks = "1 month",
                     date_labels = "%b")  +
        xlab("Date")+ylab("Daily numbers of patients") +
        ggtitle("COVID-19 Patients admitted to hospital (UK,2020)") +
        scale_fill_manual(name = "", values = c("Patients" = "steelblue"))+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
      }
    }
  })
  output$plot2 <- renderPlot({
    if (input$class == "Cases") {
      if(input$move1){
        ggplot(data = cases, aes(x = date, group = 1)) + 
          geom_bar(aes(y = cumCasesByPublishDate, fill = "Number of cases"),stat = "identity") +
          xlab("Date") + ylab("Cumulative Cases") +
          ggtitle("Cases in UK (2020)") +
          theme(plot.title = element_text(lineheight=.8, face="bold"))  +
          geom_line(aes(y=movAv, colour = "Cases (7-day average)"), size = 1.2, alpha = 0.85) +
          scale_fill_manual(name = "", values = c("Number of cases" = "orange2")) +
          scale_colour_manual(name = "", values = c("Cases (7-day average)" = "sienna"))+
          scale_x_date(date_breaks = "2 month",
                       date_minor_breaks = "1 month",
                       date_labels = "%b")+
          scale_y_continuous(labels = comma_format(big.mark = "." , 
                                                   decimal.mark = ","))
      }else{
        ggplot(data = cases, aes(x = date, group = 1)) + 
          geom_bar(aes(y = cumCasesByPublishDate, fill = "Number of cases"),stat = "identity") +
          xlab("Date") + ylab("Cumulative Cases") +
          ggtitle("Cases in UK (2020)") +
          theme(plot.title = element_text(lineheight=.8, face="bold"))  +
          scale_fill_manual(name = "", values = c("Number of cases" = "orange2")) +
          scale_x_date(date_breaks = "2 month",
                       date_minor_breaks = "1 month",
                       date_labels = "%b")+
          scale_y_continuous(labels = comma_format(big.mark = "." , 
                                                   decimal.mark = ","))
      }
    }else if (input$class == "Testing") {
      if(input$move2){
        ggplot(data = testing, aes(x = date, group = 1)) + 
          geom_bar(aes(y = cumTestsByPublishDate, fill = "Tests processed"), stat = "identity" ) +
          xlab("Date") + ylab("Cumulative Testing") +
          ggtitle("Testing in UK (2020)") +
          geom_line(aes(y=movAv, colour = "Tests processed (7-day average)"), size = 1.2, alpha = 0.7) +
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
          geom_bar(aes(y = cumTestsByPublishDate, fill = "Tests processed"), stat = "identity" ) +
          xlab("Date") + ylab("Cumulative Testing") +
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
        if(input$move4){
          ggplot(data = deaths, aes(x = date))+
            geom_bar(aes(y = cumDeaths28DaysByDeathDate,fill = "Deaths"),stat = "identity", position = "stack") +
            geom_line(aes(y=movAv, colour = "Deaths (7-day average)"), size = 1.2, alpha = 0.9) +
            scale_x_date(date_breaks = "2 month",
                         date_minor_breaks = "1 month",
                         date_labels = "%b")  +
            xlab("Date")+ylab("Cumulative Deaths") +
            ggtitle("Deaths within 28 days of positive test by date of death (UK,2020)") +
            theme(plot.title = element_text(lineheight=.8, face="bold"))+
            scale_fill_manual(name = "", values = c("Deaths" = "firebrick3"))+
            scale_colour_manual(name = "", values = c("Deaths (7-day average)" = "darkred"))+
            scale_y_continuous(labels = comma_format(big.mark = "." , 
                                                     decimal.mark = ","))
        }else{
          ggplot(data = deaths, aes(x = date))+
            geom_bar(aes(y = cumDeaths28DaysByDeathDate,fill = "Deaths"),stat = "identity", position = "stack") +
            scale_x_date(date_breaks = "2 month",
                         date_minor_breaks = "1 month",
                         date_labels = "%b")  +
            xlab("Date")+ylab("Cumulative Deaths") +
            ggtitle("Deaths within 28 days of positive test by date of death (UK,2020)") +
            theme(plot.title = element_text(lineheight=.8, face="bold"))+
            scale_fill_manual(name = "", values = c("Deaths" = "firebrick3"))+
            scale_y_continuous(labels = comma_format(big.mark = "." , 
                                                     decimal.mark = ","))
        }
      } else if (input$class == "Patients admitted to hospital"){
        if(input$move3){
          ggplot(data = adhospital, aes(x = date))+
            geom_bar(aes(y = cumAdmissions,fill = "Patients"), stat = "identity", position = "stack") +
            geom_line(aes(y=movAv, colour = "Patients (7-day average)"), size = 1.2, alpha = 0.7) +
            scale_x_date(date_breaks = "2 month",
                         date_minor_breaks = "1 month",
                         date_labels = "%b")  +
            xlab("Date")+ylab("Cumulative numbers of patients") +
            scale_fill_manual(name = "", values = c("Patients" = "steelblue"))+
            scale_colour_manual(name = "", values = c("Patients (7-day average)" = "navy"))+
            ggtitle("COVID-19 Patients admitted to hospital (UK,2020)") +
            theme(plot.title = element_text(lineheight=.8, face="bold"))+
            scale_y_continuous(labels = comma_format(big.mark = "." , 
                                                     decimal.mark = ","))
        }else{
          ggplot(data = adhospital, aes(x = date))+
            geom_bar(aes(y = cumAdmissions,fill = "Patients"), stat = "identity", position = "stack") +
            scale_x_date(date_breaks = "2 month",
                         date_minor_breaks = "1 month",
                         date_labels = "%b")  +
            xlab("Date")+ylab("Cumulative numbers of patients") +
            ggtitle("COVID-19 Patients admitted to hospital (UK,2020)") +
            scale_fill_manual(name = "", values = c("Patients" = "steelblue"))+
            theme(plot.title = element_text(lineheight=.8, face="bold"))+
            scale_y_continuous(labels = comma_format(big.mark = "." , 
                                                     decimal.mark = ","))
        }
      }
  })
}


shinyApp(ui, server)
