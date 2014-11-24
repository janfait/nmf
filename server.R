######################################################################################################
# SETUP

#globalni promenne = knihovny + promenne co se nemeni v prubehu uzivani
######################################################################################################

#authentication
bez_autorizace<-TRUE
# TRUE === no need to login
# FALSE -- login required
Logged = bez_autorizace;
PASSWORD <- read.table("passtest.txt",header = TRUE)

#load libraries
library(shiny)
library(psych)
library(plyr)
library(foreign)
library(stringr)
library(NMF)

#enables the use of layouts such as navbarPage,fluidPage, sidebarLayout, ...
try(source("bootstrap.R"),silent=T)
#read in helper functionss
try(source("functions.R"),silent=T)

######################################################################################################
# GLOBAL VARIABLES

#promenne ktere jsou definovane jeste predtim, nez zacne shiny brat jakekoli reaktivni inputy
######################################################################################################

#define tabnames
navbarnames <- c("NMF","Data","Variables","About")

#error message
error_messages <- c("something went wrong while reading datafile","your FROM and TO variables are not present","you have not specified the FROM and TO variables")

hint <- HTML('<p> 
             <br><b>What is this?</b><br>      
             NMF is non-negative matrix factorization which is used to cluster both variables and cases of data and extracts latent components.
             <br><br>
             <b>Data input options</b>
             <ul>
             <li>Select a range of variables"</li>
             <li>Recode them as needed </li>
             <li>Make sure there are no missings and negative values</li>
             </ul>
             <br>
             <b>NMF Settings</b><br>
             <ul>
             <li>Pick how many latent components to look for</li>
             <li>Select a number of runs to revalidate the result</li>
             <li>Click Run NMF</li>
             <li>Wait (a while)</li>
             </ul>
             The result is a coefficient map which shows the loadings of your variables to components.
             </p>')


######################################################################################################
# SERVER 

#server kod
######################################################################################################
shinyServer(function(input, output, session) {
  
  ####################################### INPUTS FROM WIDGETS ###################################
  
  input_datafile <- reactive(input$data)
  input_load <- reactive(input$load)
  
  input_binarize <- reactive(input$binarize)
  input_binarize_t <- reactive(input$binarize_t)
  
  input_custom <- reactive(input$custom)
  input_custom_t <- reactive(input$custom_t)
  
  input_sample <- reactive(input$sample)
  input_runs <- reactive(input$runs)
  input_comp <- reactive(input$comp)
  
  input_vars_from <- reactive(input$vars_from)
  input_vars_to <- reactive(input$vars_to)
  
  input_go <- reactive(input$go)
  
  ####################################### MAIN DATA FUNCTION ###################################
  
  #reactive function that Select data  
  Rdata <- reactive({
    
    load <- input_load()
    file <- input_datafile()
    
    if(is.null(file)){
      
      return(NULL)
      
    }else{
      
      data_filename <- file$datapath
      data_spss <-try(read.spss(data_filename,use.value.labels=F),silent=T) 
      data <- as.data.frame(data_spss,stringsAsFactors=F)
      
      #get varlabel object from spss data
      varlabels <- attr(data_spss,"variable.labels")
      varlabels <-t(as.data.frame(varlabels))
      datalist <- list(data=data,varlabels=varlabels)
    }
    
    return(datalist)
    
  })
  
  ####################################################################################################
  # LOGIN 
  ####################################################################################################
  
  #reactive login status
  USER <- reactiveValues(Logged = bez_autorizace)
  
  #uvodni okno k loginu
  output$uiLogin <- renderUI({
    if (USER$Logged == FALSE) {
      wellPanel( 
        textInput("Username", "JmĂ©no:"),
        textInput("Password", "Heslo:"),
        br(),
        actionButton("Login", "PĹ™ihlaĹˇte se:"),
        br(),
        br(),
        br(),
        list("TechnickĂˇ podpora: inovace@stemmark.cz\n Aplikace je ve vĂ˝voji")
      )
    }
  }) 
  
  output$pass <- renderText({
    if (USER$Logged == FALSE) {
      if (input$Login > 0) {
        Username <- isolate(input$Username)
        Password <- isolate(input$Password)
        Id.username <- which(PASSWORD$juzr == Username)
        Id.password <- which(PASSWORD$veslo  == Password)
        if (length(Id.username) > 0 & length(Id.password) > 0) {
          if (Id.username == Id.password){
            USER$Logged <- TRUE
            pom <- TRUE
          }
        } else  {
          list("PĹ™ihlĂˇĹˇenĂ� se nezdaĹ™ilo");
        }
      }
    }
  })
  
  ##################################################################################################
  # OUTPUTY
  #reaktivni outputy ktere se prepocitaji pri jakekoli zmene vstupu
  ##################################################################################################          
  
  output$full_data <- renderDataTable({
    
    #pass the result of RData() function whenever it changes 
    datalist <- Rdata()
    d<-head(datalist$data,100)
    return(d)
    
  })
  
  
  output$vars <- renderTable({
    
    #pass the result of RData() function whenever it changes 
    datalist <- Rdata()
    v<-t(datalist$varlabels)
    d<-describe(datalist$data)[2:5]
    d<-cbind(v,d)
    colnames(d)<-c("Label","N","Mean","SD","Median")
    return(d)
    
  })
  
  output$hint <- renderUI({
    go <- input_go()
    if(length(go)==0){
      return(NULL)
    }else{ 
      hint 
    }
  })
  
  output$plot <- renderPlot({
    
    #run the analysis
    go <- input_go()
    
    if(go==0){
      
      return(NULL)
      
    }
    
    #pass the settings only when the button is clicked
    isolate({
      
      #data function
      datalist <- Rdata()
      
      if(is.null(datalist)){
        
        stop(error_messages[1])
        
      }
      
      d<-datalist$data
      v<-datalist$varlabels
      
      #nmf settings
      runs <- input_runs()
      comp <- input_comp()
      
      #data settings
      sample <- input_sample()
      
      vars_from <- input_vars_from()
      vars_to<- input_vars_to()
      
      binarize <- input_binarize()
      binarize_t <- input_binarize_t()
      
      custom <- input_custom()
      custom_t <- input_custom_t()
      
      if(vars_from=="" || vars_to==""){
        
        stop(error_messages[3])
        
      } else {
        
        #sample rows
        s<- sample(nrow(d),round(nrow(d)*(sample/100),0),replace=T)
        d<-d[s,]
        
        #subset columns
        index_from <- which(colnames(d)==vars_from)
        index_to <- which(colnames(d)==vars_to)
        
        if(length(index_from)==0 || length(index_to)==0){
          
          stop(error_messages[2])
          
        }else{
          
          d<-d[,c(index_from:index_to)]
          v<-v[c(index_from:index_to)]
          v<-strtrim(v,35)
          colnames(d)<-v
          
          #binarize and custom recode
          if(binarize){
            d[d<=binarize_t]<-0
            d[d>binarize_t]<-1
          }
          if(custom){
            d[d %in% custom_t]<-0
          }else{
            d[d < 0]<-0
          }
          
          #drop zero rows and NAs
          rsums<-rowSums(d)
          d<-d[which(rsums>0),]
          d[is.na(d)]<-0
          
          
          #fit the NMF model based on rank estimation
          fit<-nmf(d,comp, method="lee", nrun=runs)
          c<-coefmap(fit)
          c
          
        }
        
      }
      
    })
    
  }, height = 800)
  
  
  ####################################################################################################
  # RENDER UI
  ####################################################################################################
  
  #switch to the third panel whenever we upload data
  observe({
    if (length(input$load)==0){}
    else if (input$load>0) { updateTabsetPanel(session, "navbar", selected = "3") }    
  })
  
  #tabsets
  output$content <- renderUI({
    
    if (USER$Logged == TRUE) {
      
      #define navbar a basic layout
      navbarPage(
        id = "navbar",
        title = HTML('<div id="logo"><img src="stemmark_300.png" width="200"></div>'),
        header = HTML('<script src="custom.js"></script>'),
        footer = HTML('<hr><div class="footer">Aplikace je ve vyvoji. Pro technickou podporu nas kontaktujte na <a href="mailto:inovace@stemmark.cz">inovace@stemmark.cz</a></div>'),
        
        #start navbar tabpanel
        tabPanel(navbarnames[1],  
                 sidebarLayout(
                   
                   sidebarPanel(
                     
                     wellPanel(
                       h4("Data Input Options"),
                       fileInput("data","Choose a datafile", accept=c("sav",".sav",".csv",".txt")),
                       actionButton("load","Show variables"),
                       h5("Select variables"),
                       textInput("vars_from","from variable", value =""),
                       textInput("vars_to","to variable", value =""),
                       h5("Sample cases (%)"),
                       sliderInput("sample", "", value = 100 , min = 10, max = 100, step = 10 ),
                       h5("Recode to 0|1"),
                       checkboxInput("binarize",""),
                       sliderInput("binarize_t", "", value = 100 , min = 0, max = 100, step = 1 ),
                       h5("Custom recode to 0"),
                       checkboxInput("custom",""),
                       sliderInput("custom_t", "", value = c(-99,-1) , min = -100, max = 100, step = 1 )
                       
                     ),
                     wellPanel(
                       h4("NMF Options"),
                       sliderInput("comp", "How many components to find?", value = 3 , min = 2, max = 10, step = 1 ),
                       sliderInput("runs", "Bootstrap Runs", value = 5 , min = 1, max = 10, step = 1 ),
                       actionButton("go","Run NMF")
                     )
                     
                   ),
                   
                   #outputs
                   mainPanel(
                     htmlOutput("hint"),
                     plotOutput("plot")
                   ),
                   
                 ),value=1),#end navbar tabpanel
        
        #start navbar tabpanel
        tabPanel(navbarnames[2],
                 mainPanel(
                   dataTableOutput("full_data")
                 ),    
                 value = 2),#end navbar tabpanel
        
        #start navbar tabpanel
        tabPanel(navbarnames[3],
                 mainPanel(
                   tableOutput("vars")
                 ),    
                 value = 3)#end navbar tabpanel
        
      )#end navbarPage
    }#end if
    
  })#end renderUI
  
  #checkboxes & drop-downs
  output$vyber <- renderUI( {
    if (USER$Logged == TRUE) {
      
    }
  }) 
  
  
})     

