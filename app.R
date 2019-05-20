#https://github.com/datastorm-open/visNetwork/issues/168

library(shiny)
#library(tidyverse)
library(tm)
library(networkD3)
library(visNetwork)
library(data.tree)
#library(tm)
library("SnowballC")  
library("wordcloud")
library("RColorBrewer")
#library(plotly)
library(dplyr)
#library(udpipe)
#library(tm)
library(igraph)
#library(ggraph)
#library(ggplot2)
#library(shinythemes)

#setwd('D:/Downloads_D/comments2')
#setwd('C:/Users/choong.koon.wai.joe/Desktop/load')

dflike <- read.csv('wl500.csv',header=T,stringsAsFactors = F)
dflike <- dflike[dflike$a != 'wish',]
dfwish <- read.csv('ww500.csv',header=T,stringsAsFactors = F)
# 
word_freq_like <- sort(table(dflike$a),decreasing=T)
wordlike <- names(word_freq_like)
# 
word_freq_wish <- sort(table(dfwish$a),decreasing=T)
wordwish <- names(word_freq_wish)
# 
wordss <- wordlike

dfSearch <- read.csv('ILIW.csv',header=T,stringsAsFactors = F)

col <- c("Date", "Email.Address","User.ID","Completion")
#dfSearch$Date <- anydate(c("01 Jan 2000", "01/01/2000", "2015/10/10"))
dfSearch$Date <- as.Date(dfSearch$Date)
#dfSearch$Date <- as.Date(dfSearch$Date, format="%d/%m/%Y")
#### Server ####
server <- function(input, output,session) {
  
  #df <- read.csv('w1500.csv',header=T,stringsAsFactors = F)
  #wordss <- df %>% select(a) %>% unique()
  word = reactive({
    dflike <- read.csv('wl500.csv',header=T,stringsAsFactors = F)
    dflike <- dflike[dflike$a != 'wish',]
    #wordcloud
    docs <- VCorpus(VectorSource(dflike))
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v = sort(rowSums(m),decreasing = TRUE)
    data.frame(word=names(v),freq=v)$word
  })
  
  freq = reactive({
    dflike <- read.csv('wl500.csv',header=T,stringsAsFactors = F)
    dflike <- dflike[dflike$a != 'wish',]
    #wordcloud
    docs <- VCorpus(VectorSource(dflike))
    dtm <- TermDocumentMatrix(docs)
    m = as.matrix(dtm)
    v = sort(rowSums(m),decreasing = TRUE)
    data.frame(word=names(v),freq=v)$freq
  })
  
 net1 = reactive({
   dfnet <- read.csv('ng.csv',header=T,stringsAsFactors = F)
   n <- nrow(dfnet)
   level1 <- as.integer(0.3*n)
   level2 <- as.integer(0.6*n)
   level3 <- n
   updateSelectInput(session, "levels",
                     
                     choices = c("Level 1","Level 2", "Level 3"),
                     #selected=input$search)
                     selected=input$levels)
   if(input$levels=="Level 1"){dfnet <- dfnet[1:level1,]}
   else if(input$levels=="Level 2"){dfnet <- dfnet[1:level2,]}
   else if(input$levels=="Level 3"){dfnet <- dfnet[1:n,]}
 })
  
  searchfunc <- reactive({
    dfSearch <- read.csv('ILIW.csv',header=T,stringsAsFactors = F)
    
    col <- c("Date", "Email.Address","User.ID","Completion")
    dfSearch$Date <- as.Date(dfSearch$Date)
    dfSearch <- dfSearch[,col]
    startdate <- input$daterange[1]
    enddate <- input$daterange[2]
    #with(dfSearch, dfSearch[Date >= startdate & Date <= enddate, ])
    dfSearch <- dfSearch[dfSearch$Date >= as.character(startdate) & dfSearch$Date <= as.character(enddate),]
    dfSearch <- dfSearch[,col]
    dfSearch$Date <- as.character(dfSearch$Date)
    searchwords <- c(input$words,input$search) # the search term
    
    search1 <- input$words
    search2 <- input$search
    #search3 <- paste("^",search2,"$")
    
    result1 <- dfSearch[ grep(search1, dfSearch$Completion) , ]
    result <- result1[grep(search2, result1$Completion), ]
    #result <- result1[grep("^",search2,"$",result1$Completion), ]
  
    #result <- dfSearch[ grep(searchword, dfSearch$Completion) , ] 
    #result <- unique(grep(paste(searchwords,collapse="&"),dfSearch$Completion,value=TRUE))
    #result <- grepl(paste0("^", searchwords, "$", collapse = "|"),dfSearch$Completion,value=TRUE)
    #result <- filter(dfSearch,grepl(paste(searchwords,collapse="|"),Completion))
    #result$Date <- as.character(result$Date)
    
  })
  
  #####################################################
  diag1 <- reactive({
    
    
    if (input$wishlike == "Like"){
      df <- read.csv('wl500.csv',header=T,stringsAsFactors = F)
      df <- df[df$a != 'wish',]
      word_freq_like <- sort(table(df$a),decreasing=T)
      wordlike <- names(word_freq_like)
      wordss <- wordlike
      if (input$words %in% wordss) { #,word2 <- input$words,word2 <- wordss[2])
        updateSelectInput(session, "words",
                          
                          choices = wordss,
                          selected=input$words)
        word2 <- input$words                 #choices = wordss)
      } 
      else {
        updateSelectInput(session, "words",
                          
                          choices = wordss)
        #selected=input$words)
        word2 <- wordss[2]   }
      
      df1 <- df[df$a==word2,]
      
      
      #df1$pathString <- paste("LIKE",df1$a,df1$b,df1$c,df1$d,df1$e,df1$f, sep= "|")
      df1$pathString <- paste("LIKE",df1$a,df1$b,df1$c,df1$d,df1$e,df1$f, sep= "|")
      
    }
    else if (input$wishlike == "Wish") {
      df <- read.csv('ww500.csv',header=T,stringsAsFactors = F)
      word_freq_wish <- sort(table(df$a),decreasing=T)
      wordwish <- names(word_freq_wish)
      wordss <- wordwish
      if (input$words %in% wordss) { 
      updateSelectInput(session, "words",
                        
                        choices = wordss,
                        selected=input$words)
       word2 <- input$words                
      } 
      else {
        updateSelectInput(session, "words",
                          
                          choices = wordss)
                          #selected=input$words)
      word2 <- wordss[2]     } #
      
      df1 <- df[df$a==word2,]
      
      
      
      df1$pathString <- paste("WISH",df1$a,df1$b,df1$c,df1$d,df1$e,df1$f, sep= "|")
    }
    
    #searchword1 <- unique(df1[c("b", "c")])
    updateSelectInput(session, "search",
                      
                      choices = unique(df1$b),
                      #selected=input$search)
                      selected=unique(df1$b)[5])
    #df1$pathString <- paste("LIKE",df1$X2,df1$X3,df1$X4,df1$X5, sep= "|")
    #head(df1)
    
    wordTree <- as.Node(df1, pathDelimiter = "|")
    
    wordTreeList <- ToListExplicit(wordTree, unname =TRUE)
    
    
    
  })
  
## START OUTPUT #######################################################################
########################################################################################
  
  
  output$diag <- renderDiagonalNetwork({diagonalNetwork(List=diag1(), 
                                                        fontSize = 9, 
                                                        fontFamily = "OpenSans-Light", 
                                                        nodeStroke = "orange",
                                                        linkColour = "#AAA",
                                                        opacity = 0.9)#input$opaci)
  })
  
  output$wordcloud <- renderPlot({
    wordcloud(words = word(), 
              freq = freq(), 
              min.freq = 1, scale = c(8,0.8),
              max.words=input$max, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  })
  
  output$wordplot <- renderPlot({ 
     numwords = as.integer(input$max/2)
     barplot(freq()[1:numwords], las = 2, names.arg = word()[1:numwords],
             col ="gold", main ="Most frequent words",
             ylab = "Word frequencies")
     })
  
 
  
  output$search <- renderTable(searchfunc())
  
  output$network <- renderVisNetwork({
    # minimal example
    
    dfnet <- net1()
    weight <- dfnet$cooc
    
    nodes <- data.frame(id =unique(dfnet$term1),label = unique(dfnet$term1),
                        value=length(unique(dfnet$term1)):1,group = unique(dfnet$term1),#shape="circle",
                        #title = paste0("<p>", unique(dfnet$term1),"<br> !</p>"), stringsAsFactors = FALSE)
                        title = paste0("<p>", unique(dfnet$term1)), stringsAsFactors = FALSE)
    
    #edges <- data.frame(from = dfnet$term1, to = dfnet$term2)
    edges <- data.frame(from = dfnet$term1, to = dfnet$term2)
    edges <- mutate(edges, width = weight/5 + 1)
    
    visNetwork(nodes, edges, height = "750px", width = "100%") %>% 
      visNodes(scaling = list(label = list(enabled = T))) %>%
      visGroups(groupname = "like", color = "blue", shape = "triangle", size=45) %>%
                #shadow = list(enabled = TRUE)) %>% 
      #visGroups(groupname = "like", shape = "icon", icon = list(code = "f164",color="yellow",size=90)) %>% 
                                                                
      #visGroups(groupname = "like", shape = "icon", icon = icon("thumbs-up"),size=30) %>% 
                                                                
      addFontAwesome() %>%
     
      visOptions(highlightNearest = TRUE, selectedBy = "group") %>%
      visEdges(arrows = "middle")
  })
  
}

#### UI ###############################################################################
#####################################################################################
# w <- "240px"
# h <- "240px"
ui <- shinyUI(fluidPage(
  div(style="height:2000px;",
  titlePanel("Comments Intelligence "),
  
      sidebarLayout(
        sidebarPanel(width = 2,
          
          radioButtons("wishlike", "Select Wish Or Like:",
                       choices = c("Like","Wish")),
          
          selectInput("words", 
                      label = "Diagonal Network: Select A Word ",
                      choices = dput(wordss)), #dput(wordss),
          
          dateRangeInput("daterange", "Date range: (yyyy-mm-dd)",
                         start  = min(dfSearch$Date),
                         end    = max(dfSearch$Date),
                         min    = min(dfSearch$Date),
                         max    = max(dfSearch$Date),
                         format = "yyyy/mm/dd",
                         separator = " - "),
          
          br(),
          
          selectInput("search", 
                      label = "Search Details: Select A Word",
                      choices = NULL), #dput(wordss$a),
                      #selected = wordss[1]),
          br(),
          
          sliderInput("max", "WordCloud: Select number of words", 50, min = 30,
                      max = 100, step = 10),
          
          br(),
          selectInput("levels", 
                      label = "Network Graph: Select A Depth Level",
                      choices = c("Level 1","Level 2", "Level 3"),
                      selected= "Level 1"),
          
         
          br()
          
        ),
       
################################################################################################
        mainPanel(
          tabsetPanel(
            tabPanel("Diagonal Network", diagonalNetworkOutput("diag")),
            tabPanel("Search Details",tableOutput("search")),
            tabPanel("Wordcloud",plotOutput("wordcloud", height = "400px"),plotOutput("wordplot", height = "400px",
                                                                                      width="100%")),
            tabPanel("Network Graph",visNetworkOutput("network",height = "750px"))
            
          )
        )
      ))
))

#### Run ####
shinyApp(ui = ui, server = server)