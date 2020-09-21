#crop year and barley variety with new sheet

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(tidyverse)
library(magrittr)
library(plotly)
library(corrplot)
library(caret)
library(missMDA)
library(lares)
library(leaflet)
library(leafpop)
library(ggpmisc)
#Correlelogram plot
#https://www.statsandr.com/blog/correlogram-in-r-how-to-highlight-the-most-correlated-variables-in-a-dataset/
#https://datascienceplus.com/find-insights-with-ranked-cross-correlations/
ui <- navbarPage(title=div(img(src="Admiral Logo.png",
                               style="margin-top: -14px; padding-right:10px;padding-bottom:10px",
                               height = 60)),
                 theme = shinytheme("yeti"),
                 tabPanel("Welcome",
                          h6("Hello"),
                          tags$iframe(src="SOP.pdf",height=500,width=850)),
                 tabPanel("Upload",
                          fileInput("file", "Upload the latest .csv from Airtable",
                                    accept = c('.csv')
                          ),
                          actionButton("load", "Hit this button to make it work")),
                 tabPanel("BatchVis",
                          textInput("Batch","Batch #"),
                          actionButton("batchvisgo","Submit"),
                          p(strong("Style:")),
                          textOutput("Style"),
                          plotlyOutput("batchvis"),
                          dataTableOutput("BatchTable")),
                 tabPanel("QC Plot",
                          selectInput("variable","Variable",choices = NULL),
                          textInput("userupperlimit","Upper Limit"),
                          textInput("userlowerlimit", "Lower Limit"),
                          textInput("graphbatches","# of last batches",value = NULL),
                          pickerInput("style2","Style",choices = NULL,
                                      options = list(`actions-box` = TRUE),
                                      multiple = T),
                          actionButton("submitqc","Submit"),
                          plotlyOutput("qcplot")),
                 tabPanel("Parameter Correlations",
                          textInput("Corparam", "Parameter"),
                          pickerInput("corstyle","Style",choices=NULL,
                                      options = list(`actions-box` = TRUE),
                                      multiple = T),
                          actionButton("CorrGo","Submit"),
                          plotOutput("crosscor")),
                 tabPanel("Visualize",
                          selectInput("columns1", "Select X Columns", choices = NULL),
                          selectInput("columns2", "Select Y Columns", choices = NULL),
                          pickerInput("style","Style",choices = NULL,
                                      options = list(`actions-box` = TRUE),
                                      multiple = T),
                          p(strong("R-squared")),
                          textOutput("correlation"),
                          plotlyOutput("visplot")
                 ),
                 tabPanel("Look Up Data",
                          textInput("batch", "Batch #"),
                          pickerInput("style3","Style",choices = NULL,
                                      options = list(`actions-box` = TRUE),
                                      multiple = T),
                          pickerInput("param","Parameter",choices = NULL,
                                      options = list(`actions-box` = TRUE),
                                      multiple = T),
                          dataTableOutput("lookupdata")),
                 tabPanel("Beta Table",
                          dataTableOutput("betatable"),
                 ),
                 tabPanel("Version History",
                          p(strong(h2("Updates"))),
                          p(strong(h5("v1.0.0: Takes data from googlesheets")),
                            h6("Google API was not friendly with the Shiny interface.", 
                               br("Googlesheets was difficult to use in practice for data entry."))),
                          p(strong(h5("v.1.1.0: Uploads data from Airtable instead of googledrive"))),
                          p(strong(h5("v1.2.0: QC chart and searchable database table added")),
                            h6("Data was inadvertantly ommited when it was pulled off of Airtable")),
                          p(strong(h5("v2.0.0: Allows the user to upload data as a csv file")),
                            h6("Added the Visualize, Beta Table, and Version History tab",
                            br("All of the code was rewritten from v1.2.0 to accomadate the switch to .csv files"))),
                          p(strong(h5("v2.1.0:Added in PCA and Correlation Matrix Tabs along with Bug Fixes")),
                            h6("PCA and Correlation Matrix Tab are static and do not take inputs")),
                          p(strong(h5("v2.2.1:Added in the BatchVis tab and updated the QC Chart Tab")),
                            h6("QC Tab now updates the mean and SD limits when changing the selected data",
                            br("QC Tab now allows for user input for manual limits"),
                            br("BatchVis tab added to see how a specific batch aligns with other batches of the same variety"))),
                          p(strong(h5("v2.2.2: Batch# displayed on the Visualize tab and added table to the BatchVis tab"))),
                          p(strong(h5("v2.3.0: PCA/CorrMatrix can be looked at for batches within a single style or between styles"))),
                          p(strong(h5("v2.5.0: PCA and Correlation Tabs have been replaced by Parameter Correlations Tab")))),
                 tabPanel("Map",
                          p("In case you forgot where you are"),
                          leafletOutput("map"))
)

server <- function(input, output, session) {
  
##Main Data 
  ###Visualize, Lookupdata, Parameter Correlations, and Batchvis use this
  Data<-eventReactive(input$load, {
    inFile <- input$file
    req(inFile)
    D <- read.csv(inFile$datapath, header = TRUE, sep = ",", quote = "'")
  })

##Updating Picker/Select Inputs  
  C<-observeEvent(input$load,{
    
    #Visualize
    updateSelectInput(session, "columns1","X variable",
                      choices = colnames(Data()),)
    updateSelectInput(session, "columns2","Y variable",
                      choices = colnames(Data()))
    updatePickerInput(session,inputId = "style","Style",
                      choices =levels(as.factor(Data()$Style)))
    
    #Correlation Parameters
    updatePickerInput(session,"corstyle","Style",
                      choices=c(levels(as.factor(Data()$Style))))
  
    #Look Up Data
    updatePickerInput(session,"style3","Style",choices = levels(as.factor(Data()$Style)))
    updatePickerInput(session,"param","Parameter",choices = colnames(Data()[3:ncol(Data())]))
  })

##QCplot
  Data2 <- eventReactive(input$load, {
    D3<-Data()%>%select_if(.,is.numeric)%>%cbind(Data()[1:2],.)
    D4<-D3%>%pivot_longer(.,colnames(.[3:ncol(.)]),
                          names_to="Parameter",
                          values_to="Value")
    batch<-levels(as.factor(D4$`Batch..`))%>%length()
    pars<-levels(as.factor(D4$Parameter))
    sty<-levels(as.factor(D4$Style))
    updateSelectInput(session,"variable","Variable",choices = pars)
    updatePickerInput(session,"style2","Style",choices = sty)
    updateTextInput(session,"graphbatches","# of Batches to Graph",value= batch)
    D4
  })
    
  Upperlimit<-eventReactive(input$submitqc, {
    as.integer(input$userupperlimit)
  })
  Lowerlimit<-eventReactive(input$submitqc, {
    as.integer(input$userlowerlimit)
  })
  
##BatchVis
  
  #variables to be plotted on BtachVis
  AllParameters<-eventReactive(input$load,{
    c("Batch..","Style","Total.Protein","Soluble.Protein",
      "S.T","pH","Hartwick.Friability","FAN","Extract",
      "DP","Color","B.Glucan","AA")
    })
  
  #normalize data to the mean
  Data4<-eventReactive(input$batchvisgo, {
    Sty<-Data()%>%filter(`Batch..`==input$Batch)%>%pull("Style")
    D15<-Data()%>%
      filter(.,`Style`==Sty)%>%
      select(any_of(AllParameters()))%>%
      mutate_at(vars(colnames(.[-c(1:2)])),funs(./mean(.,na.rm = T)))
    D15
  })
  
  #pivot the mean data for ggplot
  Data5<-eventReactive(input$batchvisgo, {
    Data4()%>%pivot_longer(.,colnames(.[3:ncol(.)]),
                         names_to="Parameter",
                         values_to="Values")
  })
  
  #Plot batch as blue dot
  Data6<-eventReactive(input$batchvisgo, {
      Data4()%>%filter(`Batch..`==input$Batch)%>%
        pivot_longer(.,colnames(.[3:ncol(.)]),
                     names_to="Parameter",
                     values_to="Values")
    })
  
  #Plot SD's as orange and red dots
  Data7<-eventReactive(input$batchvisgo, {
      Data4()[-c(1:2)]%>%summarise_all(.,sd,na.rm=T)%>%
        pivot_longer(.,colnames(.),
                     names_to="Parameter",
                     values_to="Values")%>%
        mutate(.,USD=.$Values+1,LSD=1-.$Values,UUSD=2*.$Values+1,LLSD=1-2*.$Values)
    })
  #Table of actual mean and batch values
  Data8<-eventReactive(input$batchvisgo, {
      
      Sty<-Data()%>%filter(`Batch..`==input$Batch)%>%pull("Style")
      
      D13<-Data()%>%
        filter(.,`Style`==Sty)%>%
        select(any_of(AllParameters()[-c(1,2)]))%>%
        summarise_all(.,mean,na.rm=T)%>%
        pivot_longer(.,colnames(.[1:ncol(.)]),
                       names_to="Parameter",
                       values_to="Mean")
      D14<-Data()%>%
        filter(.,`Style`==Sty,`Batch..`==input$Batch)%>%
        select(any_of(AllParameters()[-c(1,2)]))%>%
        pivot_longer(.,colnames(.[1:ncol(.)]),
                     names_to="Parameter",
                     values_to="Batch Value")
      D15<-cbind(D13,D14[2])
  })
  #Tells you what style the batch was
  BatchVisStyle<-eventReactive(input$batchvisgo, {
      Sty<-Data()%>%filter(`Batch..`==input$Batch)%>%pull("Style")
    })

##Parameter Correlations

Correlations<-eventReactive(input$CorrGo,{
  
  P<-Data()%>%filter(Style%in%input$corstyle)%>%
    keep(is.numeric)%>%
    .[-c(nearZeroVar(.))]
  
  S<-Data()%>%
    filter(Style%in%input$corstyle)%>%
    .[1]
  
  C<-cbind(S,P)%>%
    remove_rownames()%>%
    column_to_rownames("Batch..")%>%
    MIPCA(.,ncp=2,nboot = 1)
  
  R<-lapply(C[["res.MI"]],`[`,c(1:ncol(C[["res.MI"]][[1]])))%>%
    map_df(rownames_to_column,"Batch..")%>%
    group_by(`Batch..`)%>%
    summarise_at(.,vars(colnames(.[2:ncol(.)])),funs(mean(.)))%>%
    remove_rownames()%>%
    column_to_rownames("Batch..")
  
  R
  })

Parameter<-eventReactive(input$CorrGo,{
  as.character(input$Corparam)
})



##############Outputs
  
##map
  output$map<-renderLeaflet({
    AdmiralLogo<-"https://admiralmaltings.com/wp-content/uploads/2018/05/AdmiralMaltings_Branding_Final-01-1.png"
    content<-paste(sep = "<br/>",
                   "<b><a href='http://www.admiralmaltings.com'>Admiral Maltings</a></b>",
                   "<b><a href='https://www.google.com/maps/dir//Admiral+Maltings,+651+W+Tower+Ave,+Alameda,+CA+94501/@37.7832,-122.298837,15z/data=!4m9!4m8!1m0!1m5!1m1!1s0x808f80ee2e784a93:0xd9beaf2b49b6bf6!2m2!1d-122.2996428!2d37.7832067!3e0'>Directions</a></b>")
    leaflet()%>%
      addTiles()%>%
      setView(lng = -122.299396,lat = 37.783181,zoom = 10)%>%
      addPopups(lng=-122.299396, lat = 37.783181,
                popup =popupImage(AdmiralLogo,height=75,width=75,src = "remote"),
                options = popupOptions(closeButton = F))
  })
  
##Visualize
  output$visplot<-renderPlotly({
    Data()%>%
      filter(`Style`%in%input$style)%>%
      ggplot(aes(color=`Style`,group=1, label=`Batch..`))+
      geom_point(aes_string(x=input$columns1, y=input$columns2))+
      geom_smooth(aes_string(x=input$columns1, y=input$columns2),se=F,
                  method="lm", 
                  formula = y ~ poly(x, 1))+
      theme(axis.text.x = element_text(angle = 90, size = 6))
  })
  
  output$correlation<-renderText({
    Data()%>%
      filter(`Style`%in%input$style)%>%
    remove_missing()%$%
      cor(x=as.numeric(.[[input$columns1]]),
          y=as.numeric(.[[input$columns2]]))
  })

##QC PLOT
    output$qcplot<-renderPlotly({
    
    Mean<-Data2()%>%
      filter(`Parameter`==input$variable,`Style`%in%input$style2)%>%
      slice_tail(.,n=as.integer(input$graphbatches))%>%
      summarise(mean=mean(`Value`,na.rm = T))%>%
      as.integer()
    SD<-Data2()%>%
      filter(`Parameter`==input$variable,`Style`%in%input$style2)%>%
      slice_tail(.,n=as.integer(input$graphbatches))%>%
      summarise(sd=sd(`Value`,na.rm = T))%>%
      as.integer()
    uppersd2<-Mean+2*SD
    uppersd1<-Mean+SD
    lowersd2<-Mean-2*SD
    lowersd1<-Mean-SD
    
    Data2()%>%
      filter(`Parameter`==input$variable,`Style`%in%input$style2)%>%
      slice_tail(.,n=as.integer(input$graphbatches))%>%
      ggplot(aes(x=`Batch..`,y=`Value`,group=1,color=Style))+
      geom_point()+
      geom_smooth(se=T,
                  color="purple",
                  size=0.5)+
      geom_hline(yintercept=Mean,linetype="dashed")+
      geom_hline(yintercept = uppersd2,color="red",size=0.25)+
      geom_hline(yintercept = lowersd2,color="red",size=0.25)+
      geom_hline(yintercept = uppersd1,color="orange",size=0.25)+
      geom_hline(yintercept = lowersd1,color="orange",size=0.25)+
      geom_hline(yintercept = Upperlimit(),color="blue",size=0.25)+
      geom_hline(yintercept = Lowerlimit(),color="blue",size=0.25)+
      theme(axis.text.x = element_text(angle = 90, size = 6))
  })
    
##BatchVis
    output$batchvis<-renderPlotly({
     Data5()%>%
        ggplot(aes(x=`Parameter`,y=`Values`))+
        geom_point(size=0.1)+
        geom_point(data = Data6(),aes(x=`Parameter`,y=`Values`),color="blue",shape="|",size=1)+
        geom_point(data=Data7(),aes(x=`Parameter`,y=`USD`),color="orange",shape="|",size=1)+
        geom_point(data=Data7(),aes(x=`Parameter`,y=`LSD`),color="orange",shape="|",size=1)+
        geom_point(data=Data7(),aes(x=`Parameter`,y=`LLSD`),color="red",shape="|",size=1)+
        geom_point(data=Data7(),aes(x=`Parameter`,y=`UUSD`),color="red",shape="|",size=1)+
        ylim(min=0,max=2)+
        coord_flip()
    })
    
    output$BatchTable<-renderDataTable({
      Data8()%>%
        mutate_if(.,is.numeric,round,digits = 2)}
    )
    
    output$Style<-renderText({
      BatchVisStyle()
    })
##Lookupata
  output$lookupdata<-renderDataTable({
    Data()%>%
      filter(str_detect(`Batch..`,input$batch))%>%
      filter(`Style`%in%input$style3)%>%
      select(`Batch..`|`Style`|input$param)%>%
      print()
  })

##Parameter Correlations
  output$crosscor<-renderPlot({
    corr_cross(Correlations(),type = 1,
               max_pvalue = 0.05,
               top=50,
               contains =Parameter())
  },height = 1000)
  
##BetaTable
  output$betatable<-renderDataTable({
    BatchCorr()%>%print()
  })
}

shinyApp(ui, server)


