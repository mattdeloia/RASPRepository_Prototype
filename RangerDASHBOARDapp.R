#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#load packages
library(tidyverse)
library(foreign)
library (memisc)
library(corrplot)
library(kableExtra)
library(reshape2)
library(readxl)
library(cluster)
library(factoextra)
library(stats)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(Rmisc)
library(cluster)
library(DT)

# load and tidy data
df <- read_xlsx("RASPDataRepository.xlsx")
df<- dplyr::rename(df, ID = SSN, Complexity = S1, Breadth = S2, Innovation = S3, Tolerance = S4, Empathy = S5, Anxiety = S6, Cooperativeness = S7, Sociability = S8, Confidence = S9, Energy = S10, Astuteness = S11, Risk = S12, Organization = S13, Traditional = S14, Responsibility = S15) 
df$ID <- as.character(df$ID)
df$Rank <-as.factor(df$Rank)
df$Age <-as.integer(df$Age)
physicaldimensions<-c("Pushup", "Situp", "Run", "Chinup")
personalitydimensions<-c("Complexity", "Breadth", "Innovation", "Tolerance", "Empathy", "Anxiety", "Cooperativeness", "Sociability", "Confidence", "Energy", "Astuteness", "Risk", "Organization", "Traditional", "Responsibility")
MABII<-c("Information", "Comprehension", "Arithmetic", "Similarities", "Vocabulary", "DS", "PC", "Spatial", "PA", "OA")
clusters <-c ("Cluster 1", "Cluster 2","Cluster 3","Cluster 4","Cluster 5","Cluster 6","Cluster 7","Cluster 8")

#gather data
df1<- df %>% select (ID:SEX, `GT Score`:Responsibility) %>% 
  gather(`GT Score`:Responsibility, key=Measure, value=Score) 

#scale scores for Physical, JPI-R and MAB-II; provide summary Verbal and Performance Score from MABII
dfa<- cbind(df[,-c(11:15,18:27,84:98)], round(100*pnorm(scale(df[,c(11:15,18:27,84:98)])),1)) %>% 
     mutate(Run=(100-Run), Verbal=((Information+Comprehension+Arithmetic+Similarities+Vocabulary)/5), Performance=((DS+PC+Spatial+PA+OA)/5)) 

#gather data and create Categories (Physical, Cognitive, Personality) and create Groups for Above, Below and Normal
dfa2 <- dfa %>% select(ID:SEX, `GT Score`:Responsibility) %>% gather(`GT Score`:Responsibility, key=Measure, value=Score) %>% 
  mutate(Category=if_else(Measure %in% physicaldimensions, "Physical", if_else(Measure %in% c("GT Score", MABII), "Cognitive", if_else(Measure %in% personalitydimensions, "Personality", "Other"))), Group = if_else(Score> 84, "Above", if_else(Score>16, "Normal", "Below")))

dfa2$Group <- as.factor (dfa2$Group)
dfa2$Category<-factor(dfa2$Category, levels = c("Physical", "Cognitive", "Personality"))

#dataframe for personality summary
dfPersonality <- df %>% select(ID, MTS1:MTS15)
dfPersonality<- dplyr::rename(dfPersonality, Complexity = MTS1, Breadth = MTS2, Innovation = MTS3, Tolerance = MTS4, Empathy = MTS5, Anxiety = MTS6, Cooperativeness = MTS7, Sociability = MTS8, Confidence = MTS9, Energy = MTS10, Astuteness = MTS11, Risk = MTS12, Organization = MTS13, Traditional = MTS14, Responsibility = MTS15) %>% select(ID, Complexity:Responsibility) %>% gather(Complexity:Responsibility, key=Category, value=Score, na.rm = TRUE) 

###K Means Clustering
set.seed(98)
dfcluster <- df %>% dplyr::select(ID , Complexity:Responsibility) 
dfcluster <- cbind(dfcluster[,-c(2:16)], (scale(dfcluster[,c(2:16)])))
myclusters <- kmeans(dfcluster, centers = 8, nstart = 25)
dfcluster2 <- dfcluster %>% cbind(myclusters$cluster)
dfcluster2 <- dplyr::rename(dfcluster2, cluster = `myclusters$cluster`) 
#myclusters$centers
#myclusters$size
#fviz_cluster(myclusters, data = dfcluster)

###Cluster profiles
dfcluster2 <- dfcluster2 %>% gather(Complexity:Responsibility, key=Category, value=Score)
dfcluster2$cluster<-as.factor(dfcluster2$cluster)
dfcluster2$Category <-factor(dfcluster2$Category, levels=c(personalitydimensions))
dfcluster3<-dfcluster2 %>% group_by(Category, cluster) %>% summarise(Score=mean(Score))
dfcluster3$cluster<- paste("Cluster", dfcluster3$cluster)

dfb <- dfa %>% cbind(myclusters$cluster)
dfb <- dplyr::rename(dfb, cluster = `myclusters$cluster`) 
dfb$cluster <- paste("Cluster", dfb$cluster) 
dfb$cluster <- as.factor(dfb$cluster)
dfb$cluster <- factor(dfb$cluster, levels=c("Cluster 1", "Cluster 2","Cluster 3","Cluster 4","Cluster 5","Cluster 6","Cluster 7","Cluster 8","Cluster 9","Cluster 10"))


#Standard Plot function for use with GT and MABII summaries
standardplot <- function (data, measure, Score) {
  data %>% filter(Measure == measure) %>% ggplot() + 
  geom_density(aes(x=Score), fill="lightgreen") + 
  geom_vline(aes(xintercept=(mean(Score)-2*(sd(Score)))), linetype="solid", size =.5, color="red") +
  geom_vline(aes(xintercept=(mean(Score)-1*(sd(Score)))), linetype="dashed", size =.5, color="red") +
  geom_vline(aes(xintercept=(mean(Score))), linetype="solid", size =.5, color="black") +
  geom_vline(aes(xintercept=(mean(Score)+2*(sd(Score)))), linetype="dashed", size =.5, color="darkgreen") +
  geom_vline(aes(xintercept=(mean(Score)+1*(sd(Score)))), linetype="dashed", size =.5, color="darkgreen") +
  geom_text (aes (y=0, x=(mean(Score)-2*(sd(Score))), label=(round((mean(Score)-2*(sd(Score))), 1)), hjust=-.1, vjust=-.0), color="red", size = 5, angle=0) +
  geom_text (aes (y=0, x=(mean(Score)-1*(sd(Score))), label=(round((mean(Score)-1*(sd(Score))), 1)), hjust=-.1, vjust=-.0), color="red", size = 5, angle=0) +
  geom_text (aes (y=0, x=(mean(Score)+2*(sd(Score))), label=(round((mean(Score)+2*(sd(Score))), 1)), hjust=-.1, vjust=-.0), color="darkgreen", size = 5, angle=0) +
  geom_text (aes (y=0, x=(mean(Score)+1*(sd(Score))), label=(round((mean(Score)+1*(sd(Score))), 1)), hjust=-.1, vjust=-.0), color="darkgreen", size = 5, angle=0) +
  geom_text (aes (y=0, x=(mean(Score)), label=(round((mean(Score)), 1)), hjust=-.1, vjust=-.0), color="black", size = 5, angle=0) +
  geom_text (aes (y=0, x=(mean(Score)-2*(sd(Score))), label=("-2sd"), hjust=-.1, vjust=-2), color="red", size = 5, angle=0) +
  geom_text (aes (y=0, x=(mean(Score)-1*(sd(Score))), label=("-1sd"), hjust=-.1, vjust=-2), color="red", size = 5, angle=0) +
  geom_text (aes (y=0, x=(mean(Score)), label=("mean"), hjust=-.1, vjust=-2), color="black", size = 5, angle=0) +
  geom_text (aes (y=0, x=(mean(Score)+2*(sd(Score))), label=("+2sd"), hjust=-.1, vjust=-2), color="darkgreen", size = 5, angle=0) +
  geom_text (aes (y=0, x=(mean(Score)+1*(sd(Score))), label=("+1sd"), hjust=-.1, vjust=-2), color="darkgreen", size = 5, angle=0) +
  xlab("Raw Scores") }

###Define UI for application
ui <- dashboardPage (
        dashboardHeader(title="Ranger Dashboard"),
        
        dashboardSidebar(
            sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Data Table", tabName="datatable", icon = icon("table")),
            menuItem("Individual Report", tabName="report", icon = icon("bar-chart-o")),
            menuItem("Physical Summary", tabName = "physical", icon = icon("check")),
            menuItem("Cognitive Summary", tabName="cognitive", icon = icon("check")),
            menuItem("Personality Summary", tabName="personality", icon = icon("check"))
            ),
            sliderInput("cognitive", "Cognitive Importance",min = 0, max = 1, value = .5, step = .1),
            uiOutput("physical_slider"),
            checkboxGroupInput(inputId = "clusters", label =  "Personality Profiles/Clusters:", choices = clusters, selected = clusters )
            ),
        
        dashboardBody(
            
            tabItems(
               tabItem(tabName="dashboard",

                       fluidRow(
                          wellPanel (plotlyOutput("plot"))
                       ),
                    
                       fluidRow(
      
                          box(title="Physical Component Tuning", status="primary", solidHeader = TRUE,
                              sliderInput("pushup", "Pushup Weighting", min = 0, max = 1, value = .25, step = .01),
                              uiOutput("situp_slider"),
                              uiOutput("run_slider"),
                              uiOutput("chinup_slider")),
                          
                          box(title="Cognitive Component Tuning", status="primary", solidHeader = TRUE, 
                              sliderInput("GT", "GT Score Weighting",min = 0, max = 1, value = .33, step = .01),
                              uiOutput("verbal_slider"),
                              uiOutput("performance_slider")) 
                         ) 
               ),
                       
               
               tabItem(tabName = "datatable",
                  downloadButton("downloadData", "Download Data"),
                  textInput("ID", label = ("ID input for Individual Report"), value = "Enter ID..."),
                  dataTableOutput("table")
               ),
                
                tabItem(tabName="report",
                        downloadButton("downloadPlot", "Download Report"),
                        
                        fluidRow(
                          box(title="Physical Report", plotOutput("report1")),
                          box(title="Cogntive Report", plotOutput("report2"))
                          ),
                        
                        fluidRow(
                          wellPanel(plotOutput("report3")) 
                          )
               ),
                
               tabItem(tabName = "personality",
                        
                       fluidRow(
                             wellPanel(title="10 Most Common Personality Profiles", plotOutput("plot5"))
                         ),
                      
                       fluidRow(
                             wellPanel(title="JPI-R Dimension Results", plotOutput("plot6"))
                         )
               ),
               
               tabItem(tabName = "cognitive",
                        
                        fluidRow(
                         tabsetPanel(
                          tabPanel (title = "GT Summary", status="primary", solidHeader = TRUE, plotOutput("plot4a")),
                          tabPanel(title = "Information", status="warning", solidHeader = TRUE, plotOutput("plot4b")),
                          tabPanel(title = "Comprehension", status="warning", solidHeader = TRUE, plotOutput("plot4c")),
                          tabPanel(title = "Arithmetic", status="warning", solidHeader = TRUE, plotOutput("plot4d")),
                          tabPanel(title = "Similarities", status="warning", solidHeader = TRUE, plotOutput("plot4e")),
                          tabPanel(title = "Vocabulary", status="warning", solidHeader = TRUE, plotOutput("plot4f")),
                          tabPanel(title = "Digit Symbol", status="success", solidHeader = TRUE, plotOutput("plot4g")),
                          tabPanel(title = "Picture Completion", status="success", solidHeader = TRUE,plotOutput("plot4h")),
                          tabPanel(title = "Spatial", status="success", solidHeader = TRUE,plotOutput("plot4i")),
                          tabPanel(title = "Picture Arrangemment", status="success", solidHeader = TRUE,plotOutput("plot4j")),
                          tabPanel(title = "Object Assembly", status="success", solidHeader = TRUE,plotOutput("plot4k"))
                          )
                         ),
                        
                        fluidRow(
                          tabPanel (title= "MAB II Summary", plotOutput("plot4"))
                        )
               ),
                
                tabItem(tabName = "physical",
                     
                        fluidRow(
                          box(title = "Pushup Summary", status="primary", solidHeader = TRUE,plotOutput("plot3a")),
                          box(title = "Situp Summary", status="primary", solidHeader = TRUE,plotOutput("plot3b")),
                          box(title = "Run Summary", status="primary", solidHeader = TRUE,plotOutput("plot3c")),
                          box(title = "Chinup Summary", status="primary", solidHeader = TRUE,plotOutput("plot3d"))
                          ) 
               )
                ) ) )

####################### Define server logic for slider examples ----
server <- function(input, output) {

output$samplesize <- renderText({ df %>% nrow() })
output$GTAverage <- renderText ({round(mean(df$`GT Score`, na.rm = TRUE),0)})
output$AgeAverage <- renderText ({round(mean(df$`Age`, na.rm = TRUE),0)})
output$PushupAverage <- renderText ({round(mean(df$Pushup, na.rm=TRUE), 0)})
output$SitupAverage <- renderText ({round(mean(df$Situp, na.rm=TRUE), 0)})
output$RunAverage <- renderText ({round(mean(df$Run, na.rm=TRUE), 0)})
output$ChinupAverage <- renderText ({round(mean(df$Chinup, na.rm=TRUE), 0)})
  
#reactive dataframe
df2 <- reactive ({ dfb %>% 
    mutate(Cognitive=round(((input$GT*(`GT Score`) + input$verbal*(Verbal) + input$performance*(Performance))/(input$GT+input$verbal+input$performance) ),1),
      Physical = round(((input$pushup*(Pushup)+input$situp*(Situp)+input$run*(Run)+input$chinup*(Chinup))/(input$pushup+input$situp+input$run+input$chinup)),1),
      TotalScore = round(((input$physical*Physical + input$cognitive*Cognitive)/(input$physical+input$cognitive)),1), 
      Group = if_else(TotalScore>= 85, "Above", if_else(TotalScore>=16, "Normal", "Below"))) %>% filter(cluster %in% input$clusters)})

#sliders
output$physical_slider <- renderUI({
    if(is.null(input$cognitive)) {
        return()
    }
   sliderInput("physical", "Physical Importance",
                min = 0, max = 1, value = (1-input$cognitive), step = .01)
    })

output$verbal_slider <- renderUI({
    if(is.null(input$cognitive)) {
        return()
    }
    sliderInput("verbal", "MABII: Verbal Weighting",
                min = 0, max = 1,value = ((1-(input$GT))/2), step = .01)    
})

output$performance_slider <- renderUI({
    if(is.null(input$cognitive)) {
        return()
    }
    sliderInput("performance", "MABII: Performance Weighting",
                min = 0, max = 1, value = (1-(input$GT +input$verbal)), step = .01)
    })
 
output$situp_slider <- renderUI({
    if(is.null(input$cognitive)) {
        return()
    }

    sliderInput("situp", "Situp Weighting",
                min = 0, max = 1, value = ((1-input$pushup)/3), step = .01)
})

output$run_slider <- renderUI({
    if(is.null(input$cognitive)) {
        return()
    }
    sliderInput("run", "Run Weighting",
                min = 0, max = 1, value = (((1-(input$pushup+input$situp))/2)), step = .01)
})

output$chinup_slider <- renderUI({
    if(is.null(input$cognitive)) {
        return()
    }
    sliderInput("chinup", "Chinup Weighting",
                min = 0, max = 1, value = ((1-(input$pushup+input$situp+input$run))), step = .01)
})

###Ranger Dashboard Plot
output$plot <- renderPlotly({
    
    df2 () %>% ggplot() + 
     geom_point(aes(group=ID,x=Cognitive, y=Physical, color=Group)) + 
     ylim(0,100) + xlim(0,100) +
    theme(text = element_text(size = 10))  +
    scale_color_manual(name="", values=c("Above"="lightgreen", "Normal"="gray", "Below" = "red")) +
    theme(legend.position="bottom")+
    geom_hline(aes(yintercept=16), linetype="dashed", size =.2, color="red") +
    geom_vline(aes(xintercept=16), linetype="dashed", size =.2, color="red") +
    geom_hline(aes(yintercept=84), linetype="dashed", size =.2, color="black") +
    geom_vline(aes(xintercept=84), linetype="dashed", size =.2, color="black") +
    xlab("Ranger Cognitive Percentile") + 
    ylab("Ranger Physical Percentile") +
    geom_text (aes (y=16, x=0, label=("1sd below"), hjust=0, vjust=0), color="red", size = 3, angle=0) +
    geom_text (aes (y=84, x=0, label=("1sd above"), hjust=0, vjust=0), color="black", size = 3, angle=0) 
    })

###Ranger Datatable and Output 
output$table <- renderDataTable({
     df2() %>% select (ID, TotalScore, cluster, personalitydimensions) %>% 
    arrange(desc(TotalScore)) %>% 
    datatable(class = "display compact", filter = 'top', options = list (
      pageLength = 50,
      autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets=c(1:17)))),
      rownames= FALSE,
      colnames=c("Score" = "TotalScore", "Complex"="Complexity", "Innov"="Innovation", "Toleran"="Tolerance", "Cooper"="Cooperativeness", "Social"="Sociability", "Confid"="Confidence", "Astute"="Astuteness", "Organiz"="Organization", "Tradit"="Traditional", "Respon"="Responsibility"))
})

output$downloadData <- downloadHandler(
  filename = function(){ 
    paste("RangerFit_Datatable", "csv", sep=".") 
    },
    content=function(file){
      write.csv(df2() %>% select (ID, Rank, Age, MOS, TotalScore, Cognitive, Physical, cluster, personalitydimensions) %>% arrange(desc(TotalScore)), file)
   })

###Individual Report and Download
IndividualReport <- reactive({
  dfa2 %>% filter(ID==input$ID) %>% mutate(Measure=reorder(Measure, Score, FUN=sum)) %>% 
    ggplot(aes(x=Measure, y=Score, fill=Group)) +
    geom_col() +
    geom_text(aes( label = round(Score, 2), ),  hjust = 1,  size=3.5, color="black")+
    theme(legend.title= element_text(color="black", size=10), legend.position = "blank") +
    coord_flip() +
    ylim(0,100)+
    ylab("Ranger Percentile") +
    scale_fill_manual(values=c("Above"="lightgreen", "Normal"="gray", "Below" = "red")) +
    facet_grid(Category~., scales="free" ) +
    ggtitle("Report for ID# ", input$ID)
})

output$downloadPlot<- downloadHandler(
  filename = function(){ 
    paste("Test", "pdf", sep=".")
  },
  content= function(file) {
    ggsave(file, plot=IndividualReport(), device="pdf", width = 8.5, height = 11)
  })

output$report1<-renderPlot({
  dfa2 %>% filter(ID==input$ID, Category=="Physical") %>% mutate(Measure=reorder(Measure, Score, FUN=sum)) %>% 
    ggplot(aes(x=Measure, y=Score, fill=Group)) +
    geom_col() +
    geom_text(aes( label = round(Score, 2), ),  hjust = 1,  size=3.5, color="black")+
    theme(legend.title= element_text(color="black", size=10), legend.position = "blank") +
    coord_flip() +
    ylim(0,100)+
    xlab("Physical Component") +
    ylab("Ranger Percentile") +
    scale_fill_manual(values=c("Above"="lightgreen", "Normal"="gray", "Below" = "red")) +
    ggtitle("Physical Report for ID# ", input$ID)
  
})

output$report2<-renderPlot({
  dfa2 %>% filter(ID==input$ID, Category=="Cognitive") %>% mutate(Measure=reorder(Measure, Score, FUN=sum)) %>% 
    ggplot(aes(x=Measure, y=Score, fill=Group)) +
    geom_col() +
    geom_text(aes( label = round(Score, 2), ),  hjust = 1,  size=3.5, color="black")+
    theme(legend.title= element_text(color="black", size=10), legend.position = "blank") +
    xlab("Cognitive Test") +
    ylab("Ranger Percentile") +
    coord_flip() +
    ylim(0,100)+
    scale_fill_manual(values=c("Above"="lightgreen", "Normal"="gray", "Below" = "red")) +
    ggtitle("Cognitive Report for ID# ", input$ID)
  
})

output$report3<-renderPlot({
  dfa2 %>% filter(ID==input$ID, Category=="Personality") %>% mutate(Measure=reorder(Measure, Score, FUN=sum)) %>% 
    ggplot(aes(x=Measure, y=Score, fill=Group)) +
    geom_col() +
    geom_text(aes( label = round(Score, 2), ),  hjust = 1,  size=3.5, color="black")+
    theme(legend.title= element_text(color="black", size=10), legend.position = "blank") +
    xlab("Personality Dimension") +
    ylab("Ranger Percentile") +
    coord_flip() +
    ylim(0,100)+
    scale_fill_manual(values=c("Above"="lightblue", "Normal"="gray", "Below" = "lightblue")) +
    ggtitle("Personality Report for ID# ", input$ID)
  
})

###Physical Summary Plots x 4
output$plot3a <- renderPlot({
  standardplot(df1, "Pushup", Score) 
})

output$plot3b <- renderPlot({
  standardplot(df1, "Situp", Score) 
})

output$plot3c <- renderPlot({
  standardplot(df1, "Run", Score) 
})

output$plot3d <- renderPlot({
  standardplot(df1, "Chinup", Score) 
})

###Cognitive Summary Plots x 12
output$plot4 <- renderPlot ({
  df1 %>% filter (Measure %in% MABII) %>% ggplot() + geom_boxplot(aes(x=Measure, y=Score), fill="lightgreen") +
          xlab ("MAB II Test") +
          ylab("Raw Score") +
          theme(text = element_text(size = 14))
})

output$plot4a <- renderPlot({
  standardplot(df1, "GT Score", Score) + 
          xlab("GT Score") +
          theme(text = element_text(size = 14))
  })

output$plot4b <- renderPlot({
  standardplot(df1, "Information", Score) + xlim(0,50)
  })

output$plot4c <- renderPlot({
  standardplot(df1, "Comprehension", Score) + xlim(0,50)
  })

output$plot4d <- renderPlot({
  standardplot(df1, "Arithmetic", Score) + xlim(0,50)
})

output$plot4e <- renderPlot({
  standardplot(df1, "Similarities", Score) + xlim(0,50)
  })

output$plot4f <- renderPlot({
  standardplot(df1, "Vocabulary", Score) + xlim(0,50)
  })

output$plot4g <- renderPlot({
  standardplot(df1, "DS", Score) + xlim(0,50)
  })

output$plot4h <- renderPlot({
  standardplot(df1, "PC", Score) + xlim(0,50)
})

output$plot4i <- renderPlot({
  standardplot(df1, "Spatial", Score) + xlim(0,50)
})

output$plot4j <- renderPlot({
  standardplot(df1, "PA", Score) + xlim(0,50)
})

output$plot4k <- renderPlot({
  standardplot(df1, "OA", Score) + xlim(0,50)
})

###Personality Plots
output$plot5<- renderPlot ({
     dfcluster3%>%ggplot() + 
     geom_col(aes(x=factor(Category, levels = rev(levels(factor(Category)))), y=Score, fill=Score))+ 
     scale_fill_gradient(low = "red", high = "green") +
     coord_flip() + 
     xlab ("JPI-R Dimension") +
          ylab("Ranger Z Score") +
     facet_grid(.~cluster)  +
     theme(legend.title= element_text(color="blue", size=10), legend.position = "blank") +
          theme(text = element_text(size = 14))
})

output$plot6<- renderPlot ({
     dfPersonality %>% ggplot() +
        geom_boxplot(aes(x=Category, y=Score), fill="lightgreen") +
        ylab("Jackson T Score") + 
          xlab("JPI-R Dimension") +
          theme(text = element_text(size = 14))+ 
        theme(axis.text.x = element_text(angle = 30))
  
})

}
####################### Run the application 
shinyApp(ui = ui, server = server)