library(foreign)
library(tidyverse)
library (memisc)
library(reshape2)
library(factoextra)
library(stats)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(Rmisc)
library(cluster)
library(DT)
library(rsconnect)
library(shinycssloaders)
library(profvis)
library(janitor)
library(Hmisc)
library(chron)
library(hms)
library(lubridate)
library(randomForest)
library(corrplot)
library(xgboost)
library(data.table)
library(Matrix)
library(DecisionAnalysis)
library(rpart)
library(rpart.plot)
library(shinyjs)

#################Pre-Processing
# load and tidy data
df <- read.csv("R_DataRepository.csv")

df<- dplyr::rename(df, ID = ID, 
                   GTScore = GTScore, 
                   Complexity = S1, 
                   Breadth = S2, 
                   Innovation = S3, 
                   Tolerance = S4, 
                   Empathy = S5, 
                   Anxiety = S6, 
                   Cooperativeness = S7, 
                   Sociability = S8, 
                   Confidence = S9, 
                   Energy = S10, 
                   Astuteness = S11, 
                   Risk = S12, 
                   Organization = S13, 
                   Traditional = S14, 
                   Responsibility = S15) 

df$Graduate <- as.factor(df$Graduate)
df$ID <- as.character(df$ID)
df$Rank <-as.factor(df$Rank)
#df$Run <- as.POSIXct(df$Run,format="%M:%S")
#df <- df %>% mutate(Run=(minute(Run)+second(Run)/60))

#for(i in 1:ncol(df_impute)) {
#    df_impute[ , i][is.na(df_impute[ , i])] <- round(mean(df_impute[ , i], na.rm = TRUE),0) }

#Lists
physicaldimensions <- c("Pushup", "Situp", "Run", "Chinup")
personalitydimensions <- c("Complexity", "Breadth", "Innovation", "Tolerance", "Empathy", "Anxiety", "Cooperativeness", "Sociability", "Confidence", "Energy", "Astuteness", "Risk", "Organization", "Traditional", "Responsibility")
MABII <- c("Information", "Comprehension", "Arithmetic", "Similarities", "Vocabulary", "DS", "PC", "Spatial", "PA", "OA")
clusters <- c("Cluster 1", "Cluster 2","Cluster 3","Cluster 4","Cluster 5","Cluster 6","Cluster 7","Cluster 8")

#gather data
df1 <- df %>% dplyr::select (ID:Graduate, GTScore:Responsibility) %>% 
  gather(GTScore:Responsibility, key=Measure, value=Score) 

#scale scores for Physical, JPI-R and MAB-II; provide summary Verbal and Performance Score from MABII
dfa <- cbind(df[,-c(11:58)], round(100*pnorm(scale(df[,c(11:58)])),1)) %>% 
  mutate(Run=(100-Run), Verbal=((Information+Comprehension+Arithmetic+Similarities+Vocabulary)/5), Performance=((DS+PC+Spatial+PA+OA)/5))

dfa <- dfa %>% drop_na(Anxiety)

#Individual Reporting: gather data (Physical, Cognitive, Personality) and create Groups for Above, Below and Normal
dfa2 <- dfa %>% select(ID:Graduate, `GTScore`:Responsibility) %>% 
  gather(`GTScore`:Responsibility, key=Measure, value=Score) %>% 
  mutate(Category=if_else(Measure %in% physicaldimensions, "Physical", if_else(Measure %in% c("GTScore", MABII), "Cognitive", if_else(Measure %in% personalitydimensions, "Personality", "Other"))), Group = if_else(Score>= 69, "Above", if_else(Score>=31, "Normal", "Below")))

dfa2$Group <- as.factor (dfa2$Group)
dfa2$Category<-factor(dfa2$Category, levels = c("Physical", "Cognitive", "Personality"))

#dataframe for personality summary
dfPersonality <- df %>% select(ID, MTS1:MTS15)
dfPersonality<- dplyr::rename(dfPersonality, Complexity = MTS1, Breadth = MTS2, Innovation = MTS3, Tolerance = MTS4, Empathy = MTS5, Anxiety = MTS6, Cooperativeness = MTS7, Sociability = MTS8, Confidence = MTS9, Energy = MTS10, Astuteness = MTS11, Risk = MTS12, Organization = MTS13, Traditional = MTS14, Responsibility = MTS15) %>% select(ID, Complexity:Responsibility) %>% gather(Complexity:Responsibility, key=Category, value=Score, na.rm = TRUE) 

###K Means Clustering
set.seed(98)
dfcluster <- df %>% dplyr::select(ID , Complexity:Responsibility) %>% na.omit()
dfclustera<- data.frame(dfcluster, row.names=1) 
myclusters <- kmeans(dfclustera, centers = 8, nstart = 25)
dfcluster2 <- dfclustera %>% cbind(myclusters$cluster)
dfcluster2 <- dplyr::rename(dfcluster2, cluster = `myclusters$cluster`) 
myclusters$centers
myclusters$size

###Cluster profiles
dfcluster2a <- dfcluster2 %>% gather(Complexity:Responsibility, key=Category, value=Score)
dfcluster2a$cluster<-as.factor(dfcluster2a$cluster)
dfcluster2a$Category <-as.factor(dfcluster2a$Category) 
dfcluster2b <- dfcluster2a %>% group_by(Category) %>% dplyr::summarise(sd=sd(Score), mean=mean(Score))
dfcluster2c <- dfcluster2a %>% left_join(dfcluster2b)
dfcluster3<-dfcluster2c %>% 
  group_by(Category, cluster, sd, mean) %>% 
  dplyr::summarise(Score=mean(Score)) %>% 
  mutate(ClusterAve = if_else(Score<=(mean-.5*sd), "Low", if_else(Score>=(mean+.5*sd), "High", "Average")))
dfcluster3$cluster<- paste("Cluster", dfcluster3$cluster)
dfcluster3$ClusterAve <- factor (dfcluster3$ClusterAve, levels = c("Low", "Average", "High"))

dfb <- dfa %>% cbind(myclusters$cluster)
dfb <- dplyr::rename(dfb, cluster = `myclusters$cluster`) %>% drop_na(Pushup, Situp, Run, Chinup, GTScore, Verbal, Performance)
dfb$cluster <- paste("Cluster", dfb$cluster) 
dfb$cluster <- as.factor(dfb$cluster)
dfb$cluster <- factor(dfb$cluster, levels=c("Cluster 1", "Cluster 2","Cluster 3","Cluster 4","Cluster 5","Cluster 6","Cluster 7","Cluster 8","Cluster 9","Cluster 10"))

#Standard Plot function for use with GT and MABII summaries
standardplot <- function (data, measure, Score) {
  data %>% filter(Measure == measure) %>% na.omit() %>% ggplot() + 
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

#Random Forest Modeling
df_model <- df %>%   
  filter(Pushup>49 & Situp > 59 & Run<15) %>% 
  select(ID, GTScore, Pushup:Chinup, V,P, Complexity:Responsibility, Graduate) %>%   column_to_rownames("ID") %>% na.omit()
summary(df_model)

#df_randomforests <- df_randomforests %>% mutate_at(vars(GTScore:Responsibility), scale)

set.seed(103)
#Random Forest Model
rfmodel <- randomForest(Graduate ~ ., data = df_model, ntree = 500, mtry = 7, importance = TRUE, proximity=TRUE)
#summary(rfmodel)

#Logistics Regression Model
logmodel <- glm(Graduate ~ ., data = df_model, family = "binomial")
#summary(logmodel)

#Regression Trees Model
treemodel <- rpart(Graduate ~ ., data=df_model)
rpart.plot(treemodel, digits=2, fallen.leaves=TRUE, type=3, extra=101)
#summary(treemodel)

# Define UI for application ############################################################

ui <- 
  
  navbarPage (
    
    title= "R Dashboard",
    theme=shinytheme("united"),
    header=tagList(useShinydashboard()),
   
    tabPanel(title = "Dashboard",icon = icon("dashboard"), 
             
             sidebarLayout(
               
               sidebarPanel(
                 sliderInput("cognitive", "Cognitive Importance", min = 0, max = 1, value = .5, step = .1),
                 uiOutput("physical_slider"),
                 actionButton("go", "Data Calculation!", icon=icon("refresh")),
                 checkboxGroupInput(inputId = "clusters", label =  "Personality Profiles/Clusters:", choices = clusters, selected = clusters ),
                 #fileInput("file", label=("File input"))
               ) ,
               
               mainPanel(
                 
                 fluidRow(
                   tabsetPanel (
                     tabPanel (title="Pass RASP", status="warning", solidHeader = TRUE, background="light-blue", plotlyOutput("plot")), 
                     tabPanel (title="Fail RASP", status="warning", solidHeader = TRUE, plotlyOutput("plotb")) ) 
                   
                 ),
                 
                 fluidRow(
                   
                   box(title="Physical Component Tuning", status="primary", solidHeader = TRUE,
                       sliderInput("pushup", "Pushup Weighting", min = 0, max = 1, value = .25, step = .01),
                       sliderInput("situp", "Situp Weighting", min = 0, max = 1, value = .25, step = .01),
                       sliderInput("run", "Run Weighting", min = 0, max = 1, value = .25, step = .01),
                       sliderInput("chinup", "Chinup Weighting", min = 0, max = 1, value = .25, step = .01)
                   ),
                   
                   box(title="Cognitive Component Tuning", status="primary", solidHeader = TRUE,
                       sliderInput("GT", "GTScore Weighting",min = 0, max = 1, value = .33, step = .01),
                       sliderInput("verbal", "MABII: Verbal Weighting",min = 0, max = 1, value = .33, step = .01),
                       sliderInput("performance", "MABII: Performance Weighting",min = 0, max = 1, value = .33, step = .01)) 
                 ) 
               )) ),
    
    
    tabPanel(title = "datatable",icon = icon("table"),
             downloadButton("downloadData", "Download Data"),
             textInput("ID", label = ("ID input for Individual Report"), value = "Enter ID..."),
             dataTableOutput("table")
    ),
    
    tabPanel(title="individual report", icon = icon("bar-chart-o"),
             downloadButton("downloadPlot", "Download Report"),
             
             fluidRow(
               box(title="Fit Scores",
                   fluidRow(
                     valueBoxOutput("report4")),
                   fluidRow(
                     valueBoxOutput("report5")) ),
               box(title="Physical Report", plotOutput("report1")),
               
             ),
             
             fluidRow(
               box(title="Cogntive Report", plotOutput("report2")),  
               box (title="Personality Report", plotOutput("report3")) 
             )  ),
    
    navbarMenu("summaries", icon=icon("chart-bar"),
               
               tabPanel(title = "personality", icon = icon("check"),
                        
                        fluidRow(
                          tabsetPanel(
                            tabPanel(title="Most Common Personality Profiles", status="warning", solidHeader = TRUE, plotOutput("plot5")),
                            tabPanel(title="Cluster Pass/Fail Percentage", status="warning", solidHeader = TRUE, plotOutput("plot5b"))
                          ) ),
                        
                        fluidRow(
                          wellPanel(title="JPI-R Dimension Results (All Data)", plotOutput("plot6"))
                        ) ),
               
               tabPanel(title = "cognitive", icon = icon("check"),
                        
                        sidebarLayout(
                          
                          sidebarPanel(
                            
                            selectInput(inputId = "cog_select", label = "Cognitive Dimension", choices = c("GTScore", "V", "P", MABII), selected = "GTScore")) ,
                          
                          mainPanel(
                            
                            tabsetPanel(
                              tabPanel (title = "Cognitive Test Summary", status="primary", solidHeader = TRUE, plotOutput("plot4a")),
                              tabPanel (title= "MAB II Summary", plotOutput("plot4"))
                            )  
                          ) )  ) ,
               
               tabPanel(title = "physical", icon = icon("check"),
                        
                        sidebarLayout(
                          sidebarPanel(
                            
                            selectInput(inputId = "physical_select", label = "Physial Tests", choices = physicaldimensions, selected = "Pushup")) ,
                          
                          mainPanel(
                            fluidRow(
                              wellPanel(title = "Pushup Summary", status="primary", solidHeader = TRUE,plotOutput("plot3a"))
                              
                            ) )
                        ) ) ),
    
    tabPanel(title= "estimator", icon = icon("calculator"),
             useShinyjs(),
             
             
             fluidRow(
               
               valueBoxOutput("model1"),
               valueBoxOutput("model2"),
               valueBoxOutput("model3"),
               valueBoxOutput("cyberfit"),
               valueBoxOutput("cyberfit2")
             ),
             
             fluidRow(
               
               box(title="Physical Scores", status="primary", solidHeader = TRUE, collapsible = TRUE, id="reset1",
                   sliderInput("pushup2", "Pushup Total", min = 40, max = 97, value = 66, step = 1),
                   sliderInput("situp2", "Situp Total", min = 54, max = 105, value = 76, step = 1),
                   sliderInput("run2", "Run Time (nearest 1/4 min)", min = 11, max = 16.5, value = 13.25, step = .25),
                   sliderInput("chinup2", "Chinup Total", min = 4, max = 22, value = 8, step = 1),
                   actionButton("resetphyscial", "Reset Fields", icon=icon("refresh"))
               ),
               
               box(title="Cognitive Scores", status="primary", solidHeader = TRUE, collapsible = TRUE, id="reset2",
                   sliderInput("gtscore", "GT Score",min = 89, max = 143, value = 117, step = 1),
                   sliderInput("verbal2", "MABII Verbal (IQ Score)", min = 74, max = 136, value = 106, step = 1),
                   sliderInput("performance2", "MABII Performance (IQ Score)",min = 74, max = 136, value = 106, step = 1),
                   actionButton("resetcognitive", "Reset Fields", icon=icon("refresh"))
               )
             ),
             
             fluidRow(id="reset3",
                      
                      box(title="Analytical", status="primary", solidHeader = TRUE, width = 2,
                          numericInput("complexity", "Complexity", 8, 1, 20, 1),
                          numericInput("breadth", "Breadth", 11,1, 20, 1),
                          numericInput("innovation", "Innovation", 12, 1, 20, 1),
                          numericInput("tolerance", "Tolerance", 12, 1, 20, 1) ),
                      box(title="Emotional", status="primary", solidHeader = TRUE,  width = 2,
                          numericInput("empathy", "Empathy", 8,1, 20, 1),
                          numericInput("anxiety", "Anxiety", 7, 1, 20, 1),
                          numericInput("cooperativeness", "Cooperativeness", 5, 1, 20, 1)),
                      box(title="Extroverted", status="primary", solidHeader = TRUE, width = 2,
                          numericInput("sociability", "Sociability", 10, 1, 20, 1),
                          numericInput("confidence", "Confidence", 14,  1, 20, 1),
                          numericInput("energy", "Energy", 15, 1, 20, 1)),
                      box(title="Opportunistic", status="primary", solidHeader = TRUE, width = 2,
                          numericInput("astuteness", "Astuteness", 9, 1, 20, 1),
                          numericInput("risk", "Risk", 10, 1, 20, 1)),
                      box(title="Dependable", status="primary", solidHeader = TRUE, width = 2,
                          numericInput("organization", "Organization", 15, 1, 20, 1),
                          numericInput("traditional", "Traditional", 12,  1, 20, 1),
                          numericInput("responsibility", "Responsibility", 15, 1, 20, 1)),
                      actionButton("resetpersonality", "Reset Fields", icon=icon("refresh"))
             )) ,
    tabPanel(title = "value functions", icon = icon("chart-line"),
             useShinyjs(),
             
             tabsetPanel(
               tabPanel(title="Cognitive Value Functions",
                        fluidRow(
                          id="reset4",
                          
                          box(title="GT Scores", status="primary", solidHeader = TRUE,  footer="ref: 17C Min = xx 17C Mean ~ xx, 17C 3QT ~ xx", width=4,
                              numericInput("gt50", "50% Value", 100, 1, 140, 1),
                              numericInput("gt70", "70% Value", 109, 1, 140, 1),
                              numericInput("gt90", "90% Value", 125, 1, 140, 1),
                              numericInput("gt100", "100% Value", 130, 1, 140, 1)),
                          
                          box(title="MABII Verbal IQ Scores", status="primary", solidHeader = TRUE, footer="ref: MABII Tech Manual Table B-1; IQ = 100 (50 %ile), 109 (70 %ile), 119 (90 %ile), 133 (99 %ile)", width=4,
                              numericInput("mabv50", "50% Value", 100, 1, 140, 1),
                              numericInput("mabv70", "70% Value", 109,1, 140, 1),
                              numericInput("mabv90", "90% Value", 119, 1, 140, 1),
                              numericInput("mabv100", "100% Value", 133, 1, 140, 1)),
                          
                          box(title="MABII Performance IQ Scores", status="primary", solidHeader = TRUE,  footer="ref: same as MABII Verbal", width=4,
                              numericInput("mabp50", "50% Value", 100, 1, 140, 1),
                              numericInput("mabp70", "70% Value", 109, 1, 140, 1),
                              numericInput("mabp90", "90% Value", 119, 1, 140, 1),
                              numericInput("mabp100", "100% Value", 133,  1, 140, 1) ),
                          
                          actionButton("resetcognitivefunctions", "Reset Fields", icon=icon("refresh"))
                        ) ),
               
               tabPanel(title="Personality Value Functions", 
                        
                        fluidRow(
                          id="reset5",
                          box(title="Complexity Scores", status="primary", solidHeader = TRUE,  width = 3, footer="ref: mean=9.3, sd=3.4",
                              numericInput("complexity50", "50% Value", 9, 1, 20, 1),
                              numericInput("complexity90", "90% Value", 12, 1, 20, 1),
                              numericInput("complexity100", "100% Value", 15,  1, 20, 1) ),
                          
                          box(title="Breadth Scores", status="primary", solidHeader = TRUE,  width = 3,footer="ref: mean=10.8, sd=4.5",
                              numericInput("breadth50", "50% Value", 10, 1, 20, 1),
                              numericInput("breadth90", "90% Value", 15, 1, 20, 1),
                              numericInput("breadth100", "100% Value", 20,  1, 20, 1) ),
                          
                          box(title="Innovation Scores", status="primary", solidHeader = TRUE,  width = 3,footer="ref: mean=13.7, sd=4.4",
                              numericInput("innovation50", "50% Value", 13, 1, 20, 1),
                              numericInput("innovation90", "90% Value", 18, 1, 20, 1),
                              numericInput("innovation100", "100% Value", 20,  1, 20, 1) ),
                          
                          box(title="Energy Scores", status="primary", solidHeader = TRUE,  width = 3,footer="ref: mean=13.1, sd=4.0",
                              numericInput("energy50", "50% Value", 13, 1, 20, 1),
                              numericInput("energy90", "90% Value", 17, 1, 20, 1),
                              numericInput("energy100", "100% Value", 20,  1, 20, 1) ),
                          
                          box(title="Responsibility Scores", status="primary", solidHeader = TRUE,  width = 3,footer="ref: mean=13.4, sd=3.5",
                              numericInput("responsibility50", "50% Value", 13, 1, 20, 1),
                              numericInput("responsibility90", "90% Value", 17, 1, 20, 1),
                              numericInput("responsibility100", "100% Value", 20,  1, 20, 1) ),
                          
                          box(title="Organization Scores", status="primary", solidHeader = TRUE,  width = 3,footer="ref: mean=12.0, sd=3.9",
                              numericInput("organization50", "50% Value", 12, 1, 20, 1),
                              numericInput("organization90", "90% Value", 16, 1, 20, 1),
                              numericInput("organization100", "100% Value", 20,  1, 20, 1) ),
                          
                          box(title="Sociability Scores (rev)", status="primary", solidHeader = TRUE,  width = 3,footer="ref: mean=9.4, sd=4.4",
                              numericInput("sociability50", "50% Value", 10, 1, 20, 1),
                              numericInput("sociability90", "90% Value", 5, 1, 20, 1),
                              numericInput("sociability100", "100% Value", 1,  1, 20, 1) ),
                          
                          box(title="Empathy Scores (rev)", status="primary", solidHeader = TRUE,  width = 3,footer="ref: mean=10.5, sd=4.2",
                              numericInput("empathy50", "50% Value", 11, 1, 20, 1),
                              numericInput("empathy90", "90% Value", 6, 1, 20, 1),
                              numericInput("empathy100", "100% Value", 1,  1, 20, 1) ),
                          
                          actionButton("resetpersonalityfunctions", "Reset Fields", icon=icon("refresh"))
                        ) )
             ) ) )




####################### Define server logic for slider examples ----
server <- function(input, output) {
  
  observe({
    showNotification("FOR DEMONSTRATION PURPOSE ONLY  //  PLEASE CLOSE SESSION UPON COMPLETION OF DEMO   //  PLEASE ADDRESS QUESTIONS TO MATTHEW.DELOIA@NGC.COM", duration = 15, type="error") })
  output$file <- renderPrint({str(input$file)})
  output$samplesize <- renderText({ df %>% nrow() })
  output$GTAverage <- renderText ({round(mean(df$`GTScore`, na.rm = TRUE),0)})
  output$AgeAverage <- renderText ({round(mean(df$`Age`, na.rm = TRUE),0)})
  output$PushupAverage <- renderText ({round(mean(df$Pushup, na.rm=TRUE), 0)})
  output$SitupAverage <- renderText ({round(mean(df$Situp, na.rm=TRUE), 0)})
  output$RunAverage <- renderText ({round(mean(df$Run, na.rm=TRUE), 0)})
  output$ChinupAverage <- renderText ({round(mean(df$Chinup, na.rm=TRUE), 0)})
  
  #reactive dataframe (cyberfit)
  df_cyberfit <- eventReactive(input$go, { df %>% group_by(ID) %>% 
      mutate(Cyberfit = round((
        .5* (ifelse(GTScore<=input$gt50, GTScore*.5/input$gt50, ifelse(GTScore<=input$gt70, (.5+(GTScore-input$gt50)*.2/(input$gt70-input$gt50)), ifelse(GTScore<=input$gt90, (.7+(GTScore-input$gt70)*.2/(input$gt90-input$gt70)), (.9 + (GTScore-input$gt90)*.1/(input$gt100-input$gt90)) ))) + 
               ifelse(V<=input$mabv50, V*.5/input$mabv50, ifelse(V<=input$mabv70, (.5+(V-input$mabv50)*.2/(input$mabv70-input$mabv50)), ifelse(V<=input$mabv90, (.7+(V-input$mabv70)*.2/(input$mabv90-input$mabv70)), (.9 + (V-input$mabv90)*.1/(input$mabv100-input$mabv90)) ))) +
               ifelse(P<=input$mabp50, P*.5/input$mabp50, ifelse(P<=input$mabp70, (.5+(P-input$mabp50)*.2/(input$mabp70-input$mabp50)), ifelse(P<=input$mabp90, (.7+(P-input$mabp70)*.2/(input$mabp90-input$mabp70)), (.9 + (P-input$mabp90)*.1/(input$mabp100-input$mabp90)) )))) / 3 +
          .5* (ifelse(Energy<=input$energy50, Energy*.5/input$energy50, ifelse(Energy<=input$energy90, (.5+(Energy-input$energy50)*.4/(input$energy90-input$energy50)), (.9 + (Energy-input$energy90)*.1/(input$energy100-input$energy90)) ))   +
                 ifelse(Responsibility<=input$responsibility50, Responsibility*.5/input$responsibility50, ifelse(Responsibility<=input$responsibility90, (.5+(Responsibility-input$responsibility50)*.4/(input$responsibility90-input$responsibility50)), (.9 + (Responsibility-input$responsibility90)*.1/(input$responsibility100-input$responsibility90)))) +
                 ifelse(Breadth<=input$breadth50, Breadth*.5/input$breadth50, ifelse(Breadth<=input$breadth90, (.5+(Breadth-input$breadth50)*.4/(input$breadth90-input$breadth50)), (.9 + (Breadth-input$breadth90)*.1/(input$breadth100-input$breadth90)))) +
                 ifelse(Organization<=input$organization50, Organization*.5/input$organization50, ifelse(Organization<=input$organization90, (.5+(Organization-input$organization50)*.4/(input$organization90-input$organization50)), (.9 + (Organization-input$organization90)*.1/(input$organization100-input$organization90)) )) +
                 ifelse(Complexity<=input$complexity50, Complexity*.5/input$complexity50, ifelse(Complexity<=input$complexity90, (.5+(Complexity-input$complexity50)*.4/(input$complexity90-input$complexity50)), (.9 + (Complexity-input$complexity90)*.1/(input$complexity100-input$complexity90)) )) +
                 ifelse(Innovation<=input$innovation50, Innovation*.5/input$innovation50, ifelse(Innovation<=input$innovation90, (.5+(Innovation-input$innovation50)*.4/(input$innovation90-input$innovation50)), (.9 + (Innovation-input$innovation90)*.1/(input$innovation100-input$innovation90)) )) +
                 ifelse((21-Sociability)<=(21-input$sociability50), (21-Sociability)*.5/(21-input$sociability50), ifelse((21-Sociability)<=(21-input$sociability90), (.5+((21-Sociability)-(21-input$sociability50))*.4/((21-input$sociability90)-(21-input$sociability50))), (.9 + ((21-Sociability)-(21-input$sociability90))*.1/((21-input$sociability100)-(21-input$sociability90))) )) +
                 ifelse((21-Empathy)<=(21-input$empathy50), (21-Empathy)*.5/(21-input$empathy50), ifelse((21-Empathy)<=(21-input$empathy90), (.5+((21-Empathy)-(21-input$empathy50))*.4/((21-input$empathy90)-(21-input$empathy50))), (.9 + ((21-Empathy)-(21-input$empathy90))*.1/((21-input$empathy100)-(21-input$empathy90))) )) ) / 8 ),2) )  %>% select(ID, Cyberfit)
  })
  
  #reactive dataframe
  df2 <- eventReactive(input$go, { dfb %>% 
      mutate(Cognitive=round(((input$GT*(`GTScore`) + input$verbal*(Verbal) + input$performance*(Performance))/(input$GT+input$verbal+input$performance) ),1),
             Physical = round(((input$pushup*(Pushup)+input$situp*(Situp)+input$run*(Run)+input$chinup*(Chinup))/(input$pushup+input$situp+input$run+input$chinup)),1),
             Rfit = round(((input$physical*Physical + input$cognitive*Cognitive)/(input$physical+input$cognitive)),1)) %>%
      left_join(df_cyberfit()) %>% 
      mutate(Cyber_fit = if_else(Cyberfit>=.85, "Great Cyberfit", if_else (Cyberfit >=.75, "Good Cyberfit", if_else(Cyberfit>=.70, "Marginal Cyberfit", "Poor Cyberfit")))) %>% 
      filter(cluster %in% input$clusters)
  })
  
  #sliders
  output$physical_slider <- renderUI({
    if(is.null(input$cognitive)) {
      return()
    }
    sliderInput("physical", "Physical Importance",
                min = 0, max = 1, value = (1-input$cognitive), step = .01)
  })
  
  #Cognitive select 
  output$value <- renderPrint({input$select})
  
  ###R Dashboard Plot
  output$plot <- renderPlotly({
    
    df2 () %>% filter(Graduate=="1")  %>% 
      ggplot() + 
      geom_point(aes(group=ID,x=Cognitive, y=Physical, color=Cyber_fit)) + 
      ylim(0,100) + xlim(0,100)  +
      scale_color_manual(name="", values=c( "Great Cyberfit"="green", "Good Cyberfit"="blue", "Marginal Cyberfit"="skyblue", "Poor Cyberfit" = "lightgray")) +
      geom_hline(aes(yintercept=16), linetype="dashed", size =.2, color="red") +
      geom_vline(aes(xintercept=16), linetype="dashed", size =.2, color="red") +
      geom_hline(aes(yintercept=84), linetype="dashed", size =.2, color="black") +
      geom_vline(aes(xintercept=84), linetype="dashed", size =.2, color="black") +
      xlab("R Cognitive Percentile") + 
      ylab("R Physical Percentile") +
      geom_text (aes (y=16, x=0, label=("1sd below"), hjust=0, vjust=0), color="red", size = 3, angle=0) +
      geom_text (aes (y=84, x=0, label=("1sd above"), hjust=0, vjust=0), color="black", size = 3, angle=0) 
  })
  
  output$plotb <- renderPlotly({   
    df2 () %>% filter(Graduate=="0") %>% ggplot() + 
      geom_point(aes(group=ID,x=Cognitive, y=Physical, color=Cyber_fit)) + 
      ylim(0,100) + xlim(0,100) +
      theme(text = element_text(size = 10))  +
      scale_color_manual(name="", values=c("Great Cyberfit"="green", "Good Cyberfit"="blue", "Marginal Cyberfit"="skyblue", "Poor Cyberfit" = "lightgray")) +
      theme(legend.position="bottom")+
      geom_hline(aes(yintercept=16), linetype="dashed", size =.2, color="red") +
      geom_vline(aes(xintercept=16), linetype="dashed", size =.2, color="red") +
      geom_hline(aes(yintercept=84), linetype="dashed", size =.2, color="black") +
      geom_vline(aes(xintercept=84), linetype="dashed", size =.2, color="black") +
      xlab("R Cognitive Percentile") + 
      ylab("R Physical Percentile") +
      geom_text (aes (y=16, x=0, label=("1sd below"), hjust=0, vjust=0), color="red", size = 3, angle=0) +
      geom_text (aes (y=84, x=0, label=("1sd above"), hjust=0, vjust=0), color="black", size = 3, angle=0) 
  })    
  
  ###R Datatable and Output 
  output$table <- renderDataTable({
    df2() %>% select (ID, Rfit, Physical, Cognitive, Cyberfit, cluster, personalitydimensions) %>% 
      arrange(desc(Rfit)) %>% 
      datatable(class = "display compact", filter = 'top', options = list (
        pageLength = 50,
        autoWidth = TRUE,
        columnDefs = list(list(className = 'dt-center', targets=c(1:18)))),
        rownames= FALSE,
        colnames=c("Complex"="Complexity", "Innov"="Innovation", "Toleran"="Tolerance", "Cooper"="Cooperativeness", "Social"="Sociability", "Confid"="Confidence", "Astute"="Astuteness", "Organiz"="Organization", "Tradit"="Traditional", "Respon"="Responsibility"))
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){ 
      paste("RFit_Datatable", "csv", sep=".") 
    },
    content=function(file){
      write.csv(df2() %>% select (ID, Rank, Age, MOS, Rfit, Cognitive, Physical, Cyberfit, cluster, personalitydimensions) %>% arrange(desc(Rfit)), file)
    })
  
  ###Individual Report and Download
  individual_report <- reactive({
    df2() %>% filter(ID==input$ID) %>% select(Rfit, Cyberfit)
  })
  
  IndividualReport <- reactive({
    dfa2 %>% filter(ID==input$ID) %>% mutate(Measure=reorder(Measure, Score, FUN=sum)) %>% 
      ggplot(aes(x=Measure, y=Score, fill=Group)) +
      geom_col() +
      geom_text(aes( label = round(Score, 0) ),  hjust = 1,  size=3.5, color="black")+
      theme(legend.title= element_text(color="black", size=10), legend.position = "blank") +
      coord_flip() +
      ylim(0,100)+
      ylab("R Percentile") +
      scale_fill_manual(values=c("Above"="lightgreen", "Normal"="gray", "Below" = "coral2")) +
      facet_grid(Category~., scales="free" ) +
      ggtitle(paste("Report for ID# ", input$ID), subtitle = paste("R Fit Score:", individual_report()$Rfit, "Percentile / ", "Cyber Fit Score:", individual_report()$Cyberfit, "(0.7 threshold, 1.0 objective)" ))
  })
  
  output$downloadPlot<- downloadHandler(
    filename = function(){ 
      paste("Test", "pdf", sep=".")
    },
    content= function(file) {
      ggsave(file, plot=IndividualReport(), device="pdf", width = 8.5, height = 11)
    })
  
  output$report4 <- renderValueBox ({  
    valueBox (individual_report()$Rfit , "RFit Score",  color=if_else(individual_report()$Rfit >67, "lime", if_else(individual_report()$Rfit>=50, "green", "red")), icon=icon("star"))
  })
  
  output$report5 <- renderValueBox({
    valueBox(individual_report()$Cyberfit, "CyberFit Score", color=if_else(individual_report()$Cyberfit >.8, "lime", if_else(individual_report()$Cyberfit >=.7, "green", "red")), icon=icon("bolt"))
  })
  
  
  output$report1<-renderPlot({
    dfa2 %>% filter(ID==input$ID, Category=="Physical") %>% mutate(Measure=reorder(Measure, Score, FUN=sum)) %>% 
      ggplot(aes(x=Measure, y=Score, fill=Group)) +
      geom_col() +
      geom_text(aes( label = round(Score, 0) ),  hjust = 1,  size=3.5, color="black")+
      theme(legend.title= element_text(color="black", size=10), legend.position = "blank") +
      coord_flip() +
      ylim(0,100)+
      xlab("Physical Test") +
      ylab("R Percentile") +
      scale_fill_manual(values=c("Above"="lightgreen", "Normal"="gray", "Below" = "coral2")) +
      ggtitle(paste("Physical Report for ID# ", input$ID))
    
  })
  
  output$report2<-renderPlot({
    dfa2 %>% filter(ID==input$ID, Category=="Cognitive") %>% 
      mutate(Measure=reorder(Measure, Score, FUN=sum)) %>% 
      ggplot(aes(x=Measure, y=Score, fill=Group)) +
      geom_col() +
      geom_text(aes( label = round(Score, 0) ),  hjust = 1,  size=3.5, color="black")+
      theme(legend.title= element_text(color="black", size=10), legend.position = "blank") +
      xlab("Cognitive Test") +
      ylab("R Percentile") +
      coord_flip() +
      ylim(0,100)+
      scale_fill_manual(values=c("Above"="lightgreen", "Normal"="gray", "Below" = "coral2")) +
      ggtitle(paste("Cognitive Report for ID# ", input$ID))
    
  })
  
  output$report3<-renderPlot({
    dfa2 %>% filter(ID==input$ID, Category=="Personality") %>% 
      mutate(Measure=reorder(Measure, Score, FUN=sum)) %>% 
      ggplot(aes(x=Measure, y=Score, fill=Group)) +
      geom_col() +
      geom_text(aes( label = round(Score, 0) ),  hjust = 1,  size=3.5, color="black")+
      theme(legend.title= element_text(color="black", size=10), legend.position = "blank") +
      xlab("Personality Dimension") +
      ylab("R Percentile") +
      coord_flip() +
      ylim(0,100)+
      scale_fill_manual(values=c("Above"="skyblue", "Normal"="gray", "Below" = "skyblue")) +
      ggtitle(paste("Personality Report for ID# ", input$ID))
    
  })
  
  ###Physical Summary Plot
  output$plot3a <- renderPlot({
    standardplot(df1, input$physical_select, Score) + 
      xlab(input$physical_select) +
      theme(text = element_text(size = 14))
  })
  
  ###Cognitive Summary Plot
  output$plot4 <- renderPlot ({
    
    df1$Measure <- as.factor(df1$Measure)
    df1 %>% filter (Measure %in% MABII) %>% filter(Score>0) %>% 
      ggplot() +
      geom_boxplot(aes(x=reorder(Measure, Score, fun=mean), y=Score), fill="khaki") +
      xlab ("MAB II Test") +
      ylab("T Score") +
      theme(text = element_text(size = 14)) +
      geom_hline(yintercept = 50, linetype="dashed", color="red")
    
  })
  
  output$plot4a <- renderPlot({
    standardplot(df1, input$cog_select, Score) + 
      xlab(input$cog_select) +
      theme(text = element_text(size = 14))
  })
  
  
  ###Personality Plots
  output$plot5<- renderPlot ({
    dfcluster3%>%ggplot() + 
      geom_col(aes(x=factor(Category, levels = rev(levels(factor(Category)))), y=Score, fill=ClusterAve))+ 
      scale_fill_manual(values = c("skyblue", "gray", "lightgreen")) +
      coord_flip() + 
      xlab ("JPI-R Dimension") +
      ylab("R Raw Score") +
      facet_grid(.~cluster)  +
      theme(legend.title= element_text(color="blue", size=10), legend.position = "top") +
      theme(text = element_text(size = 14))
  })
  
  output$plot5b<- renderPlot ({
    TotalPass <- dfb %>% select(ID, Graduate) %>% 
      group_by(Graduate) %>% 
      dplyr::summarise(n=n()) %>% spread(key=Graduate, value=n) %>% 
      mutate(PassRate = (`1`/(`1` + `0`)))
    TotalRate <- TotalPass$PassRate
    
    dfb %>% select(ID, Graduate, cluster) %>% 
      group_by(cluster, Graduate) %>% 
      dplyr::summarise(n=n()) %>% 
      spread(key=Graduate, value=n) %>% 
      mutate(PassRate = (`1`/(`1` + `0`))) %>% 
      ggplot() + 
      geom_point(aes(x=cluster, y=PassRate), color="skyblue", size=3)+
      geom_text (aes(x=cluster, y=PassRate, label=round(PassRate,2), vjust=-.3), size=5 ) +
      geom_text (aes(x=1, y=TotalRate, label="Overall Pass Rate", vjust=-.3), size=4 ) +
      geom_hline(yintercept = TotalRate, linetype="dashed", color="red") +
      xlab ("") +
      ylab("Pass Rate") +
      ylim(0,.6)+
      theme(text = element_text(size = 14))
  })
  
  output$plot6<- renderPlot ({
    dfPersonality %>% ggplot() +
      geom_boxplot(aes(x=reorder(Category, Score, fun=mean), y=Score), fill="khaki") +
      geom_hline(yintercept = 50, linetype="dashed", color="red") +
      ylab("Jackson T Score") + 
      xlab("JPI-R Dimension") +
      theme(text = element_text(size = 14))+ 
      theme(axis.text.x = element_text(angle = 30)) 
    
  })
  
  observeEvent(input$resetphyscial, {
    reset("reset1")
  })
  
  observeEvent(input$resetcognitive, {
    reset("reset2")
  })
  
  
  observeEvent(input$resetpersonality, {
    reset("reset3")
  })
  
  observeEvent(input$resetcognitivefunctions, {
    reset("reset4")
  })
  
  observeEvent(input$resetpersonalityfunctions, {
    reset("reset5")
  })
  
  data_frame <- reactive( {
    data.frame(GTScore=input$gtscore, 
               Pushup=input$pushup2, 
               Situp=input$situp2, 
               Run=input$run2, 
               Chinup=input$chinup2, 
               V=input$verbal2, 
               P=input$performance2, 
               Complexity=input$complexity, 
               Breadth=input$breadth, 
               Innovation=input$innovation, 
               Tolerance=input$tolerance, 
               Empathy=input$empathy, 
               Anxiety=input$anxiety, 
               Cooperativeness=input$cooperativeness,
               Sociability=input$sociability, 
               Confidence=input$confidence,
               Energy=input$energy,
               Astuteness=input$astuteness,
               Risk=input$risk,
               Organization=input$organization,
               Traditional=input$traditional,
               Responsibility=input$responsibility)
  })
  
  predictionA <- reactive( {
    predict(logmodel, data_frame(), type="response")
  })
  
  predictionB <- reactive( {
    predict(treemodel, data_frame(), type="prob") %>% data.frame %>% select(`X1`)
  })
  
  predictionC<- reactive( {
    predict(rfmodel, data_frame(), type="prob") %>% data.frame %>% select(`X1`)
  })
  
  cyberfit <- reactive( {
    (ifelse(input$gtscore<=input$gt50, input$gtscore*.5/input$gt50, ifelse(input$gtscore<=input$gt70, (.5+(input$gtscore-input$gt50)*.2/(input$gt70-input$gt50)), ifelse(input$gtscore<=input$gt90, (.7+(input$gtscore-input$gt70)*.2/(input$gt90-input$gt70)), (.9 + (input$gtscore-input$gt90)*.1/(input$gt100-input$gt90)) ))) + 
       ifelse(input$verbal2<=input$mabv50, input$verbal2*.5/input$mabv50, ifelse(input$verbal2<=input$mabv70, (.5+(input$verbal2-input$mabv50)*.2/(input$mabv70-input$mabv50)), ifelse(input$verbal2<=input$mabv90, (.7+(input$verbal2-input$mabv70)*.2/(input$mabv90-input$mabv70)), (.9 + (input$verbal2-input$mabv90)*.1/(input$mabv100-input$mabv90)) ))) +
       ifelse(input$performance2<=input$mabp50, input$performance2*.5/input$mabp50, ifelse(input$performance2<=input$mabp70, (.5+(input$performance2-input$mabp50)*.2/(input$mabp70-input$mabp50)), ifelse(input$performance2<=input$mabp90, (.7+(input$performance2-input$mabp70)*.2/(input$mabp90-input$mabp70)), (.9 + (input$performance2-input$mabp90)*.1/(input$mabp100-input$mabp90)) )))) / 3
  })
  
  cyberfit2 <- reactive( {  
    (ifelse(input$energy<=input$energy50, input$energy*.5/input$energy50, ifelse(input$energy<=input$energy90, (.5+(input$energy-input$energy50)*.4/(input$energy90-input$energy50)), (.9 + (input$energy-input$energy90)*.1/(input$energy100-input$energy90)) ))   +
       ifelse(input$responsibility<=input$responsibility50, input$responsibility*.5/input$responsibility50, ifelse(input$responsibility<=input$responsibility90, (.5+(input$responsibility-input$responsibility50)*.4/(input$responsibility90-input$responsibility50)), (.9 + (input$responsibility-input$responsibility90)*.1/(input$responsibility100-input$responsibility90)))) +
       ifelse(input$breadth<=input$breadth50, input$breadth*.5/input$breadth50, ifelse(input$breadth<=input$breadth90, (.5+(input$breadth-input$breadth50)*.4/(input$breadth90-input$breadth50)), (.9 + (input$breadth-input$breadth90)*.1/(input$breadth100-input$breadth90)))) +
       ifelse(input$organization<=input$organization50, input$organization*.5/input$organization50, ifelse(input$organization<=input$organization90, (.5+(input$organization-input$organization50)*.4/(input$organization90-input$organization50)), (.9 + (input$organization-input$organization90)*.1/(input$organization100-input$organization90)) )) +
       ifelse(input$complexity<=input$complexity50, input$complexity*.5/input$complexity50, ifelse(input$complexity<=input$complexity90, (.5+(input$complexity-input$complexity50)*.4/(input$complexity90-input$complexity50)), (.9 + (input$complexity-input$complexity90)*.1/(input$complexity100-input$complexity90)) )) +
       ifelse(input$innovation<=input$innovation50, input$innovation*.5/input$innovation50, ifelse(input$innovation<=input$innovation90, (.5+(input$innovation-input$innovation50)*.4/(input$innovation90-input$innovation50)), (.9 + (input$innovation-input$innovation90)*.1/(input$innovation100-input$innovation90)) )) +
       ifelse((21-input$sociability)<=(21-input$sociability50), (21-input$sociability)*.5/(21-input$sociability50), ifelse((21-input$sociability)<=(21-input$sociability90), (.5+((21-input$sociability)-(21-input$sociability50))*.4/((21-input$sociability90)-(21-input$sociability50))), (.9 + ((21-input$sociability)-(21-input$sociability90))*.1/((21-input$sociability100)-(21-input$sociability90))) )) +
       ifelse((21-input$empathy)<=(21-input$empathy50), (21-input$empathy)*.5/(21-input$empathy50), ifelse((21-input$empathy)<=(21-input$empathy90), (.5+((21-input$empathy)-(21-input$empathy50))*.4/((21-input$empathy90)-(21-input$empathy50))), (.9 + ((21-input$empathy)-(21-input$empathy90))*.1/((21-input$empathy100)-(21-input$empathy90))) )) ) / 8  
  })
  
  output$model1 <- renderValueBox({  
    valueBox(round(predictionA(),2) , "RFit Model (Logistics Regression)",  color=if_else(predictionA()>.5, "green", "red"), icon=icon("star"))
  })
  
  output$model2 <- renderValueBox({  
    valueBox(round(predictionB(),2) , "RFit Model (Regression Trees)",  color=if_else(predictionB()>.5, "green", "red"), icon=icon("star"))
  })
  
  output$model3 <- renderValueBox({  
    valueBox(round(predictionC(),2) , "RFit Model (Random Forest)",  color=if_else(predictionC()>.5, "green", "red"), icon=icon("star"))
  })
  
  output$cyberfit <- renderValueBox({
    valueBox(round(cyberfit(),2), "CyberFit: Cognitive", color=if_else(cyberfit()>.8, "lime", if_else(cyberfit() >=.7, "green", "red")), icon=icon("bolt"))
  })
  
  output$cyberfit2 <- renderValueBox({
    valueBox(round(cyberfit2(),2), "CyberFit: Personality", color=if_else(cyberfit2()>.8, "lime", if_else(cyberfit2() >=.7, "green", "red")), icon=icon("bolt"))
  })
  
}
####################### Run the application 
shinyApp(ui = ui, server = server)

