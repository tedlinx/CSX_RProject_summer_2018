#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(DT)
library(shiny)
library(ggplot2)

dta <- read.csv('UCI_Credit_Card.csv')
dta1 <- read.csv('1.csv')
dta2 <- read.csv('2.csv')
dta3 <- read.csv('BANK_NTP_LG_GD .CSV',sep='\t')

dta$SEX<- as.factor(dta$SEX)
levels(dta$SEX)<-c("Male", "Female")

dta$EDUCATION<-as.factor(dta$EDUCATION)
levels(dta$EDUCATION)<-c("Unknown", "Graduate School", "University", "High school", "Others", "Unknown", "Unknown")

dta$MARRIAGE<- as.factor(dta$MARRIAGE)
levels(dta$MARRIAGE)<-c("unknown",'married', "single", "others")

dta$default.payment.next.month<- as.factor(dta$default.payment.next.month)
levels(dta$default.payment.next.month)<-c('no','yes')




choice.type1 <-
  c('SEX','EDUCATION','MARRIAGE','AGE','default.payment.next.month')
choice.type2 <-
  c('SEX','EDUCATION','MARRIAGE','default.payment.next.month')
choice.type3 <-c('SEX')#############
choice.value <-
  c("LIMIT_BAL")


  

# UI
ui <- navbarPage(
  "Shiny Example",
  tabPanel(
    "Introduction",
    tags$h1("This is an example for making a shiny app."),
    tags$p("explore the clients' data of the credit card")
    #HTML("<img height=600 src=\"https://78.media.tumblr.com/aa70ed84a7cd2e7b83c36118dfb2c0e5/tumblr_p8xzq1U8ik1qhy6c9o1_500.gif\"/>")
  ),
  
  tabPanel(
    "Raw Data",
    tags$h1("Let's take a look at the dataset."),
    br(),
    fluidRow(column(
      8,
      tabPanel("Table",
               DT::dataTableOutput("data.raw"))
    ))
  ),
  
  
 
  tabPanel(
    "Single Variable",
    tags$h1("Summrizing time!"),
    br(),
    sidebarLayout(
      sidebarPanel(
        selectInput('SV.input', 'type', c(choice.type1, choice.value), selectize = TRUE)
      ),
      mainPanel(plotOutput("SV.plot"))
    ),

    tags$h1("Summary"),
    verbatimTextOutput("summary")

  ),

  
  
  ####
  # tabPanel(
  #   'hello',tags$h1('as'),
  #   sidebarLayout(
  #    sidebarPanel(
  #      sliderInput("obs", 
  #                  "Number of observations:", 
  #                  min = 1,
  #                  max = 1000, 
  #                  value = 500)
  #    ),mainPanel(
  #      plotOutput('distPlot')
  #    )
  #     
  #   )
  # ),
  ####
  
  #####worldphone
  # tabPanel(
  #  # fluidPage(
  #   'WorldPhones',
  #   titlePanel("Telephones by region"),
  #   sidebarLayout(
  #     sidebarPanel(
  #       selectInput('region','Region:',
  #                   choices = colnames(WorldPhones)),
  #       hr(),
  #       helpText("Data from AT&T (1961) The World's Telephones.")
  #     ),
  #     mainPanel(plotOutput("phonePlot"))
  #   #)
  # )),
  #####
  
  #PAY
  # tabPanel(
  #   # fluidPage(
  #   'PAY',
  #   titlePanel("PAY"),
  #   sidebarLayout(
  #     sidebarPanel(
  #       selectInput('PAY','PAY:',
  #                   choices = colnames(dta1)[2:8]),
  #       hr(),
  #       helpText("Data from AT&T (1961) The World's Telephones.")
  #     ),
  #     mainPanel(plotOutput("phonePlot"))
  #     #)
  #   )),
  #
  
  
  #iris
  # tabPanel(
  # #pageWithSidebar(
  #   #headerPanel('Iris k-means clustering'),
  #   'Iris k-means clustering',
  #   sidebarPanel(
  #     selectInput('xcol', 'X Variable', names(iris)),
  #     selectInput('ycol', 'Y Variable', names(iris),
  #                 selected=names(iris)[[2]]),
  #     numericInput('clusters', 'Cluster count', 3,
  #                  min = 1, max = 9)
  #   ),
  #   mainPanel(
  #     plotOutput('plot1')
  #   #)
  # )),
  #
  
  
  #iris test
  tabPanel(
   #pageWithSidebar(
     #headerPanel('credit card k-means clustering'),
     'point',
     sidebarPanel(
       selectInput('xcol', 'X Variable', names(dta)[c(-1,-3,-4,-5,-6,-25,c(-7:-12))]),
       selectInput('ycol', 'Y Variable', names(dta)[c(-1,-3,-4,-5,-6,-25,c(-7:-12))],
                   selected=names(dta)[[2]]),
       numericInput('clusters', 'Cluster count', 3,
                     min = 1, max = 9)
    ),
    mainPanel(
      plotOutput('plot1')
     #)
   )),

 
#hist
 tabPanel('Age vs Limit_Bal',
     sidebarLayout(
       plotOutput("plot"),
       sliderInput("n", "Age", 25, dta2$AGE,15)
     )
     
 ),

tabPanel(
  "Box Plot",
  tags$h1("Box Plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput('PA.plot', 'type', choice.type2, selectize = TRUE),
      selectInput('PA.value', 'Value', choice.value, selectize =
                    TRUE)
    ),
    mainPanel(plotOutput("PA.plot"))
  ),
  h1("T Test / ANOVA"),
  verbatimTextOutput("t.test.anova")
))


#


###
  # navbarMenu('default',
  #         tabPanel(title = 'boxplot',plotOutput('unif'),
  #                  actionButton('reunif','Resample')),
  #         tabPanel(title = 'histograme',plotOutput('unif'),
  #                  actionButton('reunif','Resample'))))
  # 



###
# navbarMenu('default',
#         tabPanel(title = 'boxplot',plotOutput('unif'),
#                  actionButton('reunif','Resample'))
#           )

###


############################################################

# server
server <- function(input, output, session) {
  output$SV.plot <- renderPlot({
    if( is.element(input$SV.input, choice.type1) ){
      ggplot(data = dta, aes_string(x = input$SV.input)) +
        geom_bar() +
        labs(y = "count", x = input$SV.input)
    }
    else{
      ggplot(data = dta, aes_string(x = input$SV.input)) +
        geom_histogram() +
        labs(y = "count", x = input$SV.input)
    }
  })

  ####
  # output$distPlot <- renderPlot({
  #   dist <- rnorm(input$obs)
  #   hist(dist)
  # })
  ####
  
  
  output$PA.plot <- renderPlot({
    ggplot(data = dta, aes_string(x = input$PA.plot, y = input$PA.value)) +
      geom_boxplot() + coord_flip() +
      labs(y = input$PA.value, x = input$PA.plot)
    
  })
 
  
  #hist
  output$plot <- renderPlot({
    hist(dta2$LIMIT_BAL[seq_len(input$n)], breaks = 30)
  })
  #
  
  
  
  output$summary <- renderPrint({
    summary(dta)
  })
  
  
  
  
  
  ###
  output$PA.plot1 <- renderPlot({
    ggplot(data = dta, aes_string(x = input$PA.plot1, y = input$PA.value1)) +
      geom_boxplot() + coord_flip() +
      labs(y = input$PA.value, x = input$PA.plot1)
    
  })
  ###
  
  
  #####worldphone
  # output$phonePlot <- renderPlot({
  #   barplot(WorldPhones[,input$region],
  #           main = input$region,
  #           ylab = 'Number of Telephones',
  #           xlab = 'Year')
  # })
  ####
  
  
  #PAY
  # output$phonePlot <- renderPlot({
  #   barplot(dta1[,input$PAY],
  #           main = input$PAY,
  #           ylab = 'PAY Amount',
  #           xlab = 'Education')
  # })

  
  #iris
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    dta[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  #
  
 
  
  output$summary <- renderPrint({
    summary(dta)
  })
  
  
  
  ###
  output$summary1 <- renderPrint({
    summary(dta)
  })
  ###
  
  
  output$t.test.anova <- renderPrint({
    type = unlist(dta[input$PA.plot])
    value = unlist(dta[input$PA.value])
    if (length(levels(type)) == 2){
      t.test( value ~ type )
    } else {
      anova( lm(value ~ type) )
    }
  })
  

  
  
  output$data.raw <- DT::renderDataTable({
    DT::datatable(dta)
  })
  
 
  
  output$data.summary <- DT::renderDataTable({
    DT::datatable(summary(dta))
  })
}



# Run the application 
shinyApp(ui = ui, server = server)

