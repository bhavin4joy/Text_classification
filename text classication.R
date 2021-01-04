library(shiny)
library(shinythemes)
library(shinydashboard)
ui <-dashboardPage(
 dashboardHeader(title = "TEXT CLASSIFIER"),
 dashboardSidebar(
  sidebarMenu(
   menuItem("Upload File", tabName = "Up_fl", icon = icon("arrow-circle-up")),
   menuItem("Classification", tabName = "class", icon = icon("cog"),badgeLabel = "new", badgeColor = "green")
  )
 ),
 dashboardBody(
  tabItems(
   tabItem(tabName = "Up_fl",
           h2("Upload File"),
           fluidPage(
            mainPanel(
             fileInput("file1", "Choose CSV File",
                       multiple = FALSE,
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")),
             tags$hr(),
             checkboxInput("header", "Header", TRUE),
             radioButtons("sep", "Separator",
                          choices = c(Comma = ",",Semicolon = ";",Tab = "\t"),selected = ","),
             #radioButtons("quote", "Quote",choices = c(None = "","Double Quote" = '"',"Single Quote" = "'"),selected = '"'),
             tags$hr(),
             radioButtons("disp", "Display",choices = c(Head = "head",All = "all"),selected = "head"),
             tableOutput("contents")
            )
           )
   ),
   tabItem(tabName = "class",
           h2("The category is:"),
           h1(NB_pred1))
           
     #   tags$hr(),
      #     fluidPage(
       #     fluidRow(
        #     tabBox(
         #     height = 10,
          #    width = 10,
           #   id="Classification",
            #    tabPanel("class",tags$hr(),
             #   fluidRow(valueBoxOutput("NB_pred1"))
             )
 )
 )


server <- function(input, output) {
 
 output$contents <- renderTable({
  req(input$file1)
  tryCatch(
   {
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
   },
   error = function(e) {
    stop(safeError(e))
   }
  )
  if(input$disp == "head") {
   return(head(df))
  }
  else {
   return(df)
  }
 })
 cor<-reactive({preprocess(df)})
 dtm1 <-reactive({ DocumentTermMatrix(cor)}) 
 dtm1 <-reactive( {t(weightTfIdf(t(dtm1)))})
 #remove sparcity
 dtm1 <- reactive({removeSparseTerms(dtm1, 0.8)})
 # Plot term_count
 freq1 <- reactive({colSums(as.matrix(dtm1))})
 wf1 <-reactive( {data.frame(word = names(freq1), freq = freq1)})
 
 newsparse1 <-reactive( {as.data.frame(as.matrix(dtm1))})
 dim(newsparse1)
 #colnames(newsparse1) <-reactive({ make.names(colnames(newsparse1))})
 #newsparse1$category <-reactive( {as.factor(df$category)})
 output$NB_pred1<-renderInfoBox({
  valueBox(tags$p(predict(NB_model,newsparse1), style = "font-size: 75%;"),"Dickey-Fuller",color = "blue")
 })
}
shinyApp(ui, server)
