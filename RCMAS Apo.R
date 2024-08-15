#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
# https://shiny.posit.co/
#

library(shiny)
library(plotly)
library(ggplot2)
require(readxl)

ui <- fluidPage(
  titlePanel("Data tracking for RCMAS-3"),
  sidebarLayout(      
    sidebarPanel(
      selectInput("yaxis",
                  label = "Choose a y-axis variable to display",
                  choices = list("n",
                                 "percent"),
                  selected = "percent"),
      
      selectInput("xaxis",
                  label = "Choose a x-axis variable to display",
                  choices = c("race",
                              "gender","ses","age"),
                  selected = "gender"),
      selectInput("form", 
                  label = "form(s)",
                  choices = c("parent","teacher","self","all"),
                  selected = "self")
    ),
    
    mainPanel(
      plotlyOutput(outputId = "plot1"),br(),
      plotlyOutput(outputId = "plot2"),br(),
      em("These plots were created using the REAL RCMAS3 data and US census reports. 
         In comparison plot, target values are retrieved from US census."),
      br(), br(), br()
    )
  )
)

server <- function(input, output){
  dat1 <- reactive({
    if (input$xaxis == "gender"  & input$form == "all") final[!is.na(final$gender)&final$form == "all",]
    else if (input$xaxis == "gender" & input$form == "parent") final[!is.na(final$gender)&final$form == "parent",]
    else if (input$xaxis == "gender" & input$form == "self") final[!is.na(final$gender)&final$form == "self",]
    else if (input$xaxis == "gender"& input$form == "teacher") final[!is.na(final$gender)&final$form == "teacher",]
    else if (input$xaxis == "ses" & input$form == "all") final[!is.na(final$ses)&final$form == "all",]
    else if (input$xaxis == "ses" & input$form == "parent") final[!is.na(final$ses)&final$form == "parent",]
    else if (input$xaxis == "ses" & input$form == "self") final[!is.na(final$ses)&final$form == "self",]
    else if (input$xaxis == "ses" & input$form == "teacher") final[!is.na(final$ses)&final$form == "teacher",]
    else if (input$xaxis == "age" & input$form == "all") final[!is.na(final$age)&final$form == "all",]
    else if (input$xaxis == "age" & input$form == "parent") final[!is.na(final$age)&final$form == "parent",]
    else if (input$xaxis == "age" & input$form == "self") final[!is.na(final$age)&final$form == "self",]
    else if (input$xaxis == "age" & input$form == "teacher") final[!is.na(final$age)&final$form == "teacher",]
    else if (input$xaxis == "race" & input$form == "all") final[!is.na(final$race)&final$form == "all",]
    else if (input$xaxis == "race" & input$form == "parent") final[!is.na(final$race)&final$form == "parent",]
    else if (input$xaxis == "race" & input$form == "self") final[!is.na(final$race)&final$form == "self",]
    else if (input$xaxis == "race" & input$form == "teacher") final[!is.na(final$race)&final$form == "teacher",]
  }) 
  dat2 <- reactive({
    if (input$xaxis == "gender" & input$form == "all") final[!is.na(final$gender)&final$form == "all",][c(1:2),]
    else if (input$xaxis == "gender" & input$form == "parent") final[!is.na(final$gender)&final$form == "parent",][c(1:2),]
    else if (input$xaxis == "gender" & input$form == "self") final[!is.na(final$gender)&final$form == "self",][c(1:2),]
    else if (input$xaxis == "gender" & input$form == "teacher") final[!is.na(final$gender)&final$form == "teacher",][c(1:2),]
    else if (input$xaxis == "ses"  & input$form == "all") final[!is.na(final$ses)&final$form == "all",][c(1:4),]
    else if (input$xaxis == "ses"  & input$form == "parent") final[!is.na(final$ses)&final$form == "parent",][c(1:4),]
    else if (input$xaxis == "ses"  & input$form == "self") final[!is.na(final$ses)&final$form == "self",][c(1:4),]
    else if (input$xaxis == "ses"  & input$form == "teacher") final[!is.na(final$ses)&final$form == "teacher",][c(1:4),]
    else if (input$xaxis == "age" & input$form == "all") final[!is.na(final$age)&final$form == "all",][c(1:16),]
    else if (input$xaxis == "age" & input$form == "parent") final[!is.na(final$age)&final$form == "parent",][c(1:16),]
    else if (input$xaxis == "age" & input$form == "self") final[!is.na(final$age)&final$form == "self",][c(1:16),]
    else if (input$xaxis == "age" & input$form == "teacher") final[!is.na(final$age)&final$form == "teacher",][c(1:16),]
    else if (input$xaxis == "race" & input$form == "all") final[!is.na(final$race)&final$form == "all",][c(1:6),]
    else if (input$xaxis == "race" & input$form == "parent") final[!is.na(final$race)&final$form == "parent",][c(1:6),]
    else if (input$xaxis == "race" & input$form == "self") final[!is.na(final$race)&final$form == "self",][c(1:6),]
    else if (input$xaxis == "race" & input$form == "teacher") final[!is.na(final$race)&final$form == "teacher",][c(1:6),]
  })
  
  x <- reactive({
    sampledata = cbind(
      final[!is.na(final$gender)&final$form == "all"&final$time == "current",]['n'],1200/2
    )
    colnames(sampledata) <- c("current","target")
    sumdata=data.frame(n=apply(sampledata,2,sum))
    rownames(sumdata) <- NULL
    sumdata$time=colnames(sampledata)
    sumdata
  })
  
  output$plot1 <- renderPlotly({
    if(input$dataorigin == TRUE){
      req(dat1(),dat2())
      fill <- if (input$dataorigin) "time"
      ggplot(dat1(), aes_string(x = input$xaxis, y = input$yaxis, fill = fill)) +
        geom_bar(stat = "identity", position = "dodge") + labs(title = "comparison plot (current vs target)") +
        geom_text(aes_string(label=input$yaxis), position=position_dodge(width=0.9),size=3.5) + 
        scale_x_discrete(labels = function(x){sub("\\s", "\n", x)}) +
        if(input$yaxis == "n" & input$form == "all"  & input$xaxis == "age" ) coord_cartesian(ylim = c(0, 300))
      else if(input$yaxis == "n" & input$form == "all" & input$xaxis == "race" ) coord_cartesian(ylim = c(0, 1000))
      else if(input$yaxis == "n" & input$form == "all"  & input$xaxis == "ses" ) coord_cartesian(ylim = c(0, 1000))
      else if(input$yaxis == "n" & input$form == "all"  & input$xaxis == "gender") coord_cartesian(ylim = c(0, 1000))
      else if(input$yaxis == "n" & input$form == "self" & input$xaxis == "age") coord_cartesian(ylim = c(0, 200))
      else if(input$yaxis == "n" & input$form == "self"  & input$xaxis == "race") coord_cartesian(ylim = c(0, 500))
      else if(input$yaxis == "n" & input$form == "self" & input$xaxis == "ses") coord_cartesian(ylim = c(0, 500))
      else if(input$yaxis == "n" & input$form == "parent" & input$xaxis == "ses") coord_cartesian(ylim = c(0, 500))
      else if(input$yaxis == "n" & input$form == "parent"  & input$xaxis == "race") coord_cartesian(ylim = c(0, 500))
      else if(input$yaxis == "n" & input$form == "parent" & input$xaxis == "age") coord_cartesian(ylim = c(0, 200))
      else if(input$yaxis == "n" & input$form == "teacher" & input$xaxis == "ses") coord_cartesian(ylim = c(0, 500))
      else if(input$yaxis == "n" & input$form == "teacher" & input$xaxis == "race") coord_cartesian(ylim = c(0, 500))
      else if(input$yaxis == "n" & input$form == "teacher" & input$xaxis == "age") coord_cartesian(ylim = c(0, 200))
      else if(input$yaxis == "n" & input$form == "teacher" & input$xaxis == "gender") coord_cartesian(ylim = c(0, 500))
      else if(input$yaxis == "n" & input$form == "all") coord_cartesian(ylim = c(0, 400))
      else if(input$yaxis == "percent") coord_cartesian(ylim = c(0, 100))
      else coord_cartesian(ylim = c(0, 300))
    } else{
      ggplot(dat2(), aes_string(x = isolate(input$xaxis), y = input$yaxis, fill = input$xaxis)) +
        geom_bar(stat = "identity", position = "dodge") + labs(title = " current data plot") +
        geom_text(aes_string(label=input$yaxis),size=3.5) + 
        scale_x_discrete(labels = function(x){sub("\\s", "\n", x)})+
        if(input$yaxis == "n" & input$form == "all" & input$xaxis == "age" ) coord_cartesian(ylim = c(0,300))
      else if(input$yaxis == "n" & input$form == "all"  & input$xaxis == "race") coord_cartesian(ylim = c(0,500))
      else if(input$yaxis == "n" & input$form == "all"  & input$xaxis == "gender") coord_cartesian(ylim = c(0,500))
      else if(input$yaxis == "n" & input$form == "all"  & input$xaxis == "ses") coord_cartesian(ylim = c(0,500))
      else if(input$yaxis == "n" & input$form == "parent"  & input$xaxis == "gender") coord_cartesian(ylim = c(0, 500))
      else if(input$yaxis == "n" & input$form == "parent"  & input$xaxis == "race") coord_cartesian(ylim = c(0, 500))
      else if(input$yaxis == "n" & input$form == "parent"  & input$xaxis == "ses") coord_cartesian(ylim = c(0, 500))
      else if(input$yaxis == "n" & input$form == "parent"  & input$xaxis == "age") coord_cartesian(ylim = c(0, 200))
      else if(input$yaxis == "n" & input$form == "teacher"  & input$xaxis == "ses") coord_cartesian(ylim = c(0, 500))
      else if(input$yaxis == "n" & input$form == "teacher"  & input$xaxis == "race") coord_cartesian(ylim = c(0, 500))
      else if(input$yaxis == "n" & input$form == "teacher"  & input$xaxis == "gender") coord_cartesian(ylim = c(0, 500))
      else if(input$yaxis == "n" & input$form == "teacher"  & input$xaxis == "age") coord_cartesian(ylim = c(0, 200))
      else if(input$yaxis == "n" & input$form == "self" & input$xaxis == "gender") coord_cartesian(ylim = c(0, 500))
      else if(input$yaxis == "n" & input$form == "self" & input$xaxis == "race") coord_cartesian(ylim = c(0, 500))
      else if(input$yaxis == "n" & input$form == "self" & input$xaxis == "ses") coord_cartesian(ylim = c(0, 500))
      else if(input$yaxis == "n" & input$form == "self" & input$xaxis == "age") coord_cartesian(ylim = c(0, 200))
      else coord_cartesian(ylim = c(0, 100))
    }
  })
  
  output$plot2 <- renderPlotly({
    ggplot(x(), aes(x=time, y=n, fill=time)) +
      geom_bar(stat="identity") +
      geom_text(aes(label=n),hjust = 5,size=3.5)+
      coord_flip() +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
  })
  
  output$table <- DT::renderDataTable({
    dat1()
  })
}

rcmas <- read_excel("rcmas-shiny-data.xlsx",sheet = 1)
rcmas1 <- rcmas[!duplicated(rcmas$`ID number:`),]
race <- subset(rcmas1,select = grep("Q16|Q17|Q18|Q19|Q20|Q21|Q22", names(rcmas1), value=TRUE))
race[race == "" ] <- NA # convert "" to NA
rcmas1$race <- ifelse(!is.na(race$Q16) == "TRUE",'American indian',
                      ifelse(!is.na(race$Q17) == "TRUE","Asian",
                             ifelse(!is.na(race$Q18) == "TRUE","Black",
                                    ifelse(!is.na(race$Q19) == "TRUE","Mideast",
                                           ifelse(!is.na(race$Q20) == "TRUE","Native Hawaiian",
                                                  ifelse(!is.na(race$Q21) == "TRUE","White", 
                                                         ifelse(!is.na(race$Q22) == "TRUE","Others","NA")))))))
tblFun <- function(x){
  tbl <- table(x)
  res <- cbind(tbl)
  colnames(res) <- c('current_N')
  res
}
rcmas_race <- lapply(rcmas1[c("race")],function(x) tblFun(x)) 
census_race_rcmas = c(5.27,11.88,12.11,2.4,0.1,65.24)
dd_race_rcmas <- data.frame(N = ceiling(census_race_rcmas * 1200/100),round(census_race_rcmas,2))
names(dd_race_rcmas) <- c("target_N","census_%")
rownames(dd_race_rcmas) <- rownames(rcmas_race$race)
rcmas_nation <- cbind(rcmas_race$race,dd_race_rcmas)
rcmas_nation[,paste0('complete_%')] = ifelse(is.na(rcmas_nation[,1]) == "FALSE", 
                                             round(rcmas_nation[,1]/sum(rcmas_nation[,1])*100,1), NA)
rcmas_nation_race <- rcmas_nation[, c(1, 2, 4, 3)]
#rownames(sp3d_nation_race) <- c("Asian","Black","Hispanics","Native", "Others","White")
colnames(rcmas_nation_race)[1] <- "N"

rcmas_nation_new1<- tibble::rownames_to_column(rcmas_nation_race, "race")
rcmas_race_long <- reshape(rcmas_nation_new1, idvar = "race", varying = list(c(2,3), c(4,5)), 
                           v.names=c("n","percent"), times=c("current","target"), direction="long")
#survey_nation_long$n[6:10] <- "" # get rid of the target sample size
rownames(rcmas_race_long) <- NULL
rcmas_gender <- lapply(rcmas1[c("Q9")],function(x) tblFun(x))
census_gender_rcmas <- c(50.5,49.5)
dd_gender_rcmas <- data.frame(N = round(census_gender_rcmas * 1200/100,0),round(census_gender_rcmas,2))
names(dd_gender_rcmas) <- c("target_N","census_%")
rownames(dd_gender_rcmas) <- rownames(rcmas_gender$Q9)
rcmas_nation_gender <- cbind(rcmas_gender$Q9,dd_gender_rcmas)
rcmas_nation_gender[,paste0('complete_%')] = ifelse(is.na(rcmas_nation_gender[,1]) == "FALSE", 
                                                    round(rcmas_nation_gender[,1]/sum(rcmas_nation_gender[,1])*100,1), NA)
rcmas_nation_gender_new <- rcmas_nation_gender[, c(1, 2, 4, 3)]
rownames(rcmas_nation_gender_new) <- c("Female","Male")
colnames(rcmas_nation_gender_new)[1] <- "N"
survey_nation_new2 <- tibble::rownames_to_column(rcmas_nation_gender_new, "gender")
rcmas_gender_long <- reshape(survey_nation_new2, idvar = "gender", varying = list(c(2,3), c(4,5)), 
                             v.names=c("n","percent"), times=c("current","target"), direction="long")
rownames(rcmas_gender_long) <- NULL
rcmas_ses <- lapply(rcmas1[c("Q26")],function(x) tblFun(x))
census_ses_rcmas <- c(31.7,25.7,11.9,30.6)
dd_ses_rcmas <- data.frame(N = round(census_ses_rcmas * 1200/100,0),round(census_ses_rcmas,2))

names(dd_ses_rcmas) <- c("target_N","census_%")
rownames(dd_ses_rcmas) <- rownames(rcmas_ses$Q26)
ses_nation_ses <- cbind(rcmas_ses$Q26,dd_ses_rcmas)
ses_nation_ses[,paste0('complete_%')] = ifelse(is.na(ses_nation_ses[,1]) == "FALSE", 
                                               round(ses_nation_ses[,1]/sum(ses_nation_ses[,1])*100,1), NA)
ses_nation_ses_new <- ses_nation_ses[, c(1, 2, 4, 3)]
rownames(ses_nation_ses_new) <- c("Bachelor or plus ","High school","No high school","Some associate degree")
colnames(ses_nation_ses_new)[1] <- "N"

survey_nation_new3 <- tibble::rownames_to_column(ses_nation_ses_new, "ses")
rcmas_ses_long <- reshape(survey_nation_new3, idvar = "ses", varying = list(c(2,3), c(4,5)), 
                          v.names=c("n","percent"), times=c("current","target"), direction="long")
rownames(rcmas_ses_long) <- NULL
rcmas_age <- lapply(rcmas[c("Q8")],function(x) tblFun(x))
census_age_rcmas = c(rep(6,16))
dd_age_rcmas <- data.frame(N = ceiling(census_age_rcmas * 1200/100),round(census_age_rcmas,2))
names(dd_age_rcmas) <- c("target_N","census_%")
rownames(dd_age_rcmas) <- names(table(rcmas$Q8))
rcmas_nation_age <- cbind(rcmas_age,dd_age_rcmas)
rcmas_nation_age[,paste0('complete_%')] = ifelse(is.na(rcmas_nation_age[,1]) == "FALSE", 
                                                 round(rcmas_nation_age[,1]/(rcmas_nation_age[,2])*100,1), NA)
rcmas_nation_age <- rcmas_nation_age[, c(1, 2, 4, 3)]
colnames(rcmas_nation_age)[1] <- "N"
rcmas_nation_new_age<- tibble::rownames_to_column(rcmas_nation_age, "age")

rcmas_age_long <- reshape(rcmas_nation_new_age, idvar = "age", varying = list(c(2,3), c(4,5)), 
                          v.names=c("n","percent"), times=c("current","target"), direction="long")
rownames(rcmas_age_long) <- NULL

rcmas_gender_long[setdiff(names(rcmas_race_long), names(rcmas_gender_long))] <- NA
rcmas_race_long[setdiff(names(rcmas_gender_long), names(rcmas_race_long))] <- NA
rcmas_gender_long[setdiff(names(rcmas_ses_long), names(rcmas_gender_long))] <- NA
rcmas_ses_long[setdiff(names(rcmas_gender_long), names(rcmas_ses_long))] <- NA
rcmas_race_long[setdiff(names(rcmas_ses_long), names(rcmas_race_long))] <- NA
rcmas_ses_long[setdiff(names(rcmas_race_long), names(rcmas_ses_long))] <- NA
rcmas_gender_long[setdiff(names(rcmas_age_long), names(rcmas_gender_long))] <- NA
rcmas_age_long[setdiff(names(rcmas_gender_long), names(rcmas_age_long))] <- NA
rcmas_race_long[setdiff(names(rcmas_age_long), names(rcmas_race_long))] <- NA
rcmas_age_long[setdiff(names(rcmas_race_long), names(rcmas_age_long))] <- NA
rcmas_ses_long[setdiff(names(rcmas_age_long), names(rcmas_ses_long))] <- NA
rcmas_age_long[setdiff(names(rcmas_ses_long), names(rcmas_age_long))] <- NA
gender_race_rcmas_age_merge <- rbind(cbind(rcmas_gender_long,origin='gender'),cbind(rcmas_race_long,origin='race'),cbind(rcmas_ses_long,origin='ses'),
                                     cbind(rcmas_age_long,origin='age')) 
###############################################################################################################################
rcmas_teacher <- subset(rcmas, form == 'teacher')
race_teacher <- subset(rcmas_teacher,select = grep("Q16|Q17|Q18|Q19|Q20|Q21|Q22", names(rcmas_teacher), value=TRUE))
race_teacher[race_teacher == "" ] <- NA 

rcmas_teacher$race <- ifelse(!is.na(race_teacher$Q16) == rep("TRUE",length(race_teacher$Q16)), 'American indian',
                             ifelse(!is.na(race_teacher$Q17) == rep("TRUE",length(race_teacher$Q17)),"Asian",
                                    ifelse(!is.na(race_teacher$Q18) == rep("TRUE",length(race_teacher$Q18)),"Black",
                                           ifelse(!is.na(race_teacher$Q19) == rep("TRUE",length(race_teacher$Q19)),"Mideast",
                                                  ifelse(!is.na(race_teacher$Q20) == rep("TRUE",length(race_teacher$Q20)),"Native Hawaiian",
                                                         ifelse(!is.na(race_teacher$Q21) == rep("TRUE",length(race_teacher$Q21)),"White",
                                                                ifelse(!is.na(race_teacher$Q22) == rep("TRUE",length(race_teacher$Q22)),"Others", "NA")))))))

rcmas_teacher_race <- lapply(rcmas_teacher[c("race")],function(x) tblFun(x))
census_race_rcmas_teacher = c(5.27,11.88,12.11,2.4,0.1,65.24)
dd_race_srs_rcmas_teacher <- data.frame(N = ceiling(census_race_rcmas_teacher * 400/100),round(census_race_rcmas_teacher,2))
names(dd_race_srs_rcmas_teacher) <- c("target_N","census_%")
rownames(dd_race_srs_rcmas_teacher) <- rownames(rcmas_teacher_race$race)
rcmas_teacher_nation <- cbind(rcmas_teacher_race$race,dd_race_srs_rcmas_teacher)
rcmas_teacher_nation[,paste0('complete_%')] = ifelse(is.na(rcmas_teacher_nation[,1]) == "FALSE", 
                                                     round(rcmas_teacher_nation[,1]/sum(rcmas_teacher_nation[,1])*100,1), NA)
rcmas_teacher_nation_race <- rcmas_teacher_nation[, c(1, 2, 4, 3)]
colnames(rcmas_teacher_nation_race)[1] <- "N"
rcmas_teacher_nation_new1 <- tibble::rownames_to_column(rcmas_teacher_nation_race, "race")
rcmas_teacher_race_long <- reshape(rcmas_teacher_nation_new1, idvar = "race", varying = list(c(2,3), c(4,5)), 
                                   v.names=c("n","percent"), times=c("current","target"), direction="long")
rownames(rcmas_teacher_race_long) <- NULL
rcmas_teacher_gender <- lapply(rcmas_teacher[c("Q9")],function(x) tblFun(x))
census_gender_rcmas_teacher <- c(50.5,49.5)
dd_gender_rcmas_teacher <- data.frame(N = round(census_gender_rcmas_teacher * 700/100,0),round(census_gender_rcmas_teacher,2))
names(dd_gender_rcmas_teacher) <- c("target_N","census_%")
rownames(dd_gender_rcmas_teacher) <- rownames(rcmas_teacher_gender$Q9)
rcmas_teacher_nation_gender <- cbind(rcmas_teacher_gender$Q9,dd_gender_rcmas_teacher)
rcmas_teacher_nation_gender[,paste0('complete_%')] = ifelse(is.na(rcmas_teacher_nation_gender[,1]) == "FALSE", 
                                                            round(rcmas_teacher_nation_gender[,1]/sum(rcmas_teacher_nation_gender[,1])*100,1), NA)
rcmas_teacher_nation_gender_new <- rcmas_teacher_nation_gender[, c(1, 2, 4, 3)]
rownames(rcmas_teacher_nation_gender_new) <- c("Female","Male")
colnames(rcmas_teacher_nation_gender_new)[1] <- "N"

rcmas_teacher_nation_new2 <- tibble::rownames_to_column(rcmas_teacher_nation_gender_new, "gender")
rcmas_teacher_gender_long <- reshape(rcmas_teacher_nation_new2, idvar = "gender", varying = list(c(2,3), c(4,5)), 
                                     v.names=c("n","percent"), times=c("current","target"), direction="long")
rownames(rcmas_teacher_gender_long) <- NULL
rcmas_teacher_ses <- lapply(rcmas_teacher[c("Q26")],function(x) tblFun(x))
census_ses_rcmas_teacher <- c(31.7,25.7,11.9,30.6)
dd_ses_rcmas_teacher <- data.frame(N = round(census_ses_rcmas_teacher * 400/100,0),round(census_ses_rcmas_teacher,2))
names(dd_ses_rcmas_teacher) <- c("target_N","census_%")
rownames(dd_ses_rcmas_teacher) <- rownames(rcmas_teacher_ses$Q26)
rcmas_teacher_nation_ses <- cbind(rcmas_teacher_ses$Q26,dd_ses_rcmas_teacher)
rcmas_teacher_nation_ses[,paste0('complete_%')] = ifelse(is.na(rcmas_teacher_nation_ses[,1]) == "FALSE", 
                                                         round(rcmas_teacher_nation_ses[,1]/sum(rcmas_teacher_nation_ses[,1])*100,1), NA)
rcmas_teacher_nation_ses_new <- rcmas_teacher_nation_ses[, c(1, 2, 4, 3)]
rownames(rcmas_teacher_nation_ses_new) <- c("Bachelor or plus ","High school","No high school","Some associate degree")
colnames(rcmas_teacher_nation_ses_new)[1] <- "N"
rcmas_teacher_nation_new3 <- tibble::rownames_to_column(rcmas_teacher_nation_ses_new, "ses")
rcmas_teacher_ses_long <- reshape(rcmas_teacher_nation_new3 , idvar = "ses", varying = list(c(2,3), c(4,5)), 
                                  v.names=c("n","percent"), times=c("current","target"), direction="long")
rownames(rcmas_teacher_ses_long) <- NULL
rcmas_teacher_age <- lapply(rcmas_teacher[c("Q8")],function(x) tblFun(x))
census_age_rcmas_teacher = c(rep(6,16))
dd_age_rcmas_teacher <- data.frame(N = ceiling(census_age_rcmas_teacher * 400/100),round(census_age_rcmas_teacher,2))
names(dd_age_rcmas_teacher) <- c("target_N","census_%")
rownames(dd_age_rcmas_teacher) <- names(table(rcmas_teacher$Q8))

rcmas_teacher_nation_age <- cbind(rcmas_teacher_age,dd_age_rcmas_teacher)

rcmas_teacher_nation_age[,paste0('complete_%')] = ifelse(is.na(rcmas_teacher_nation_age[,1]) == "FALSE", 
                                                         round(rcmas_teacher_nation_age[,1]/(rcmas_teacher_nation_age[,2])*100,1), NA)

rcmas_teacher_nation_age <- rcmas_teacher_nation_age[, c(1, 2, 4, 3)]
colnames(rcmas_teacher_nation_age)[1] <- "N"
rcmas_teacher_nation_new_age<- tibble::rownames_to_column(rcmas_teacher_nation_age, "age")
rcmas_teacher_age_long <- reshape(rcmas_teacher_nation_new_age, idvar = "age", varying = list(c(2,3), c(4,5)), 
                                  v.names=c("n","percent"), times=c("current","target"), direction="long")
rownames(rcmas_teacher_age_long) <- NULL

rcmas_teacher_gender_long[setdiff(names(rcmas_teacher_race_long), names(rcmas_teacher_gender_long))] <- NA
rcmas_teacher_race_long[setdiff(names(rcmas_teacher_gender_long), names(rcmas_teacher_race_long))] <- NA
rcmas_teacher_gender_long[setdiff(names(rcmas_teacher_ses_long), names(rcmas_teacher_gender_long))] <- NA
rcmas_teacher_ses_long[setdiff(names(rcmas_teacher_gender_long), names(rcmas_teacher_ses_long))] <- NA
rcmas_teacher_race_long[setdiff(names(rcmas_teacher_ses_long), names(rcmas_teacher_race_long))] <- NA
rcmas_teacher_ses_long[setdiff(names(rcmas_teacher_race_long), names(rcmas_teacher_ses_long))] <- NA
rcmas_teacher_gender_long[setdiff(names(rcmas_teacher_age_long), names(rcmas_teacher_gender_long))] <- NA
rcmas_teacher_age_long[setdiff(names(rcmas_teacher_gender_long), names(rcmas_teacher_age_long))] <- NA
rcmas_teacher_race_long[setdiff(names(rcmas_teacher_age_long), names(rcmas_teacher_race_long))] <- NA
rcmas_teacher_age_long[setdiff(names(rcmas_teacher_race_long), names(rcmas_teacher_age_long))] <- NA
rcmas_teacher_ses_long[setdiff(names(rcmas_teacher_age_long), names(rcmas_teacher_ses_long))] <- NA
rcmas_teacher_age_long[setdiff(names(rcmas_teacher_ses_long), names(rcmas_teacher_age_long))] <- NA
gender_race_rcmas_teacher_merge <- rbind(cbind(rcmas_teacher_gender_long,origin='gender'),cbind(rcmas_teacher_race_long,origin='race'),
                                         cbind(rcmas_teacher_ses_long,origin='ses'),
                                         cbind(rcmas_teacher_age_long,origin='age')) 
###############################################################################################################################################
rcmas_parent <- subset(rcmas, form == 'parent')
race_parent <- subset(rcmas_parent,select = grep("Q16|Q17|Q18|Q19|Q20|Q21|Q22", names(rcmas_parent), value=TRUE))
race_parent[race_parent == "" ] <- NA 

rcmas_parent$race <- ifelse(!is.na(race_parent$Q16) == "TRUE",'American indian',
                            ifelse(!is.na(race_parent$Q17) == "TRUE","Asian",
                                   ifelse(!is.na(race_parent$Q18) == "TRUE","Black",
                                          ifelse(!is.na(race_parent$Q19) == "TRUE","Mideast",
                                                 ifelse(!is.na(race_parent$Q20) == "TRUE","Native Hawaiian",
                                                        ifelse(!is.na(race_parent$Q21) == "TRUE","White", 
                                                               ifelse(!is.na(race_parent$Q22) == "TRUE","Others","NA")))))))
rcmas_parent_race <- lapply(rcmas_parent[c("race")],function(x) tblFun(x))
census_race_rcmas_parent = c(5.27,11.88,12.11,2.4,0.1,65.24)
dd_race_srs_rcmas_parent <- data.frame(N = ceiling(census_race_rcmas_parent * 400/100),round(census_race_rcmas_parent,2))
names(dd_race_srs_rcmas_parent) <- c("target_N","census_%")
rownames(dd_race_srs_rcmas_parent) <- rownames(rcmas_parent_race$race)
rcmas_parent_nation <- cbind(rcmas_parent_race$race,dd_race_srs_rcmas_parent)
rcmas_parent_nation[,paste0('complete_%')] = ifelse(is.na(rcmas_parent_nation[,1]) == "FALSE", 
                                                    round(rcmas_parent_nation[,1]/sum(rcmas_parent_nation[,1])*100,1), NA)
rcmas_parent_nation_race <- rcmas_parent_nation[, c(1, 2, 4, 3)]
colnames(rcmas_parent_nation_race)[1] <- "N"
rcmas_parent_nation_new1 <- tibble::rownames_to_column(rcmas_parent_nation_race, "race")
rcmas_parent_race_long <- reshape(rcmas_parent_nation_new1, idvar = "race", varying = list(c(2,3), c(4,5)), 
                                  v.names=c("n","percent"), times=c("current","target"), direction="long")
rownames(rcmas_parent_race_long) <- NULL
rcmas_parent_gender <- lapply(rcmas_parent[c("Q9")],function(x) tblFun(x))
census_gender_rcmas_parent <- c(50.5,49.5)
dd_gender_rcmas_parent <- data.frame(N = round(census_gender_rcmas_parent * 400/100,0),round(census_gender_rcmas_parent,2))

names(dd_gender_rcmas_parent) <- c("target_N","census_%")
rownames(dd_gender_rcmas_parent) <- rownames(rcmas_parent_gender$Q9) 
rcmas_parent_nation_gender <- cbind(rcmas_parent_gender$Q9,dd_gender_rcmas_parent)
rcmas_parent_nation_gender[,paste0('complete_%')] = ifelse(is.na(rcmas_parent_nation_gender[,1]) == "FALSE", 
                                                           round(rcmas_parent_nation_gender[,1]/sum(rcmas_parent_nation_gender[,1])*100,1), NA)
rcmas_parent_nation_gender_new <- rcmas_parent_nation_gender[, c(1, 2, 4, 3)]
rownames(rcmas_parent_nation_gender_new) <- c("Female","Male")
colnames(rcmas_parent_nation_gender_new)[1] <- "N"

rcmas_parent_nation_new2 <- tibble::rownames_to_column(rcmas_parent_nation_gender_new, "gender")
rcmas_parent_gender_long <- reshape(rcmas_parent_nation_new2, idvar = "gender", varying = list(c(2,3), c(4,5)), 
                                    v.names=c("n","percent"), times=c("current","target"), direction="long")
rownames(rcmas_parent_gender_long) <- NULL

rcmas_parent_ses <- lapply(rcmas_parent[c("Q26")],function(x) tblFun(x))
census_ses_rcmas_parent <- c(31.7,25.7,11.9,30.6)
dd_ses_rcmas_parent <- data.frame(N = round(census_ses_rcmas_parent * 400/100,0),round(census_ses_rcmas_parent,2))

names(dd_ses_rcmas_parent) <- c("target_N","census_%")
rownames(dd_ses_rcmas_parent) <- rownames(rcmas_parent_ses$Q26)
rcmas_parent_nation_ses <- cbind(rcmas_parent_ses$Q26,dd_ses_rcmas_parent)
rcmas_parent_nation_ses[,paste0('complete_%')] = ifelse(is.na(rcmas_parent_nation_ses[,1]) == "FALSE", 
                                                        round(rcmas_parent_nation_ses[,1]/sum(rcmas_parent_nation_ses[,1])*100,1), NA)
rcmas_parent_nation_ses_new <- rcmas_parent_nation_ses[, c(1, 2, 4, 3)]
rownames(rcmas_parent_nation_ses_new) <- c("Bachelor or plus ","High school","No high school","Some associate degree")
colnames(rcmas_parent_nation_ses_new)[1] <- "N"
rcmas_parent_nation_new3 <- tibble::rownames_to_column(rcmas_parent_nation_ses_new, "ses")
rcmas_parent_ses_long <- reshape(rcmas_parent_nation_new3 , idvar = "ses", varying = list(c(2,3), c(4,5)), 
                                 v.names=c("n","percent"), times=c("current","target"), direction="long")
rownames(rcmas_parent_ses_long) <- NULL
rcmas_parent_age <- lapply(rcmas_parent[c("Q8")],function(x) tblFun(x))
census_age_rcmas_parent = c(rep(6,16))
dd_age_rcmas_parent <- data.frame(N = ceiling(census_age_rcmas_parent * 400/100),round(census_age_rcmas_parent,2))
names(dd_age_rcmas_parent) <- c("target_N","census_%")
rownames(dd_age_rcmas_parent) <- names(table(rcmas_parent$Q8))
rcmas_parent_nation_age <- cbind(rcmas_parent_age,dd_age_rcmas_parent)
rcmas_parent_nation_age[,paste0('complete_%')] = ifelse(is.na(rcmas_parent_nation_age[,1]) == "FALSE", 
                                                        round(rcmas_parent_nation_age[,1]/sum(rcmas_parent_nation_age[,1])*100,1), NA)
rcmas_parent_nation_age <- rcmas_parent_nation_age[, c(1, 2, 4, 3)]
colnames(rcmas_parent_nation_age)[1] <- "N"
rcmas_parent_nation_new_age<- tibble::rownames_to_column(rcmas_parent_nation_age, "age")
rcmas_parent_age_long <- reshape(rcmas_parent_nation_new_age, idvar = "age", varying = list(c(2,3), c(4,5)), 
                                 v.names=c("n","percent"), times=c("current","target"), direction="long")
rownames(rcmas_parent_age_long) <- NULL

rcmas_parent_gender_long[setdiff(names(rcmas_parent_race_long), names(rcmas_parent_gender_long))] <- NA
rcmas_parent_race_long[setdiff(names(rcmas_parent_gender_long), names(rcmas_parent_race_long))] <- NA
rcmas_parent_gender_long[setdiff(names(rcmas_parent_ses_long), names(rcmas_parent_gender_long))] <- NA
rcmas_parent_ses_long[setdiff(names(rcmas_parent_gender_long), names(rcmas_parent_ses_long))] <- NA
rcmas_parent_race_long[setdiff(names(rcmas_parent_ses_long), names(rcmas_parent_race_long))] <- NA
rcmas_parent_ses_long[setdiff(names(rcmas_parent_race_long), names(rcmas_parent_ses_long))] <- NA
rcmas_parent_gender_long[setdiff(names(rcmas_parent_age_long), names(rcmas_parent_gender_long))] <- NA
rcmas_parent_age_long[setdiff(names(rcmas_parent_gender_long), names(rcmas_parent_age_long))] <- NA
rcmas_parent_race_long[setdiff(names(rcmas_parent_age_long), names(rcmas_parent_race_long))] <- NA
rcmas_parent_age_long[setdiff(names(rcmas_parent_race_long), names(rcmas_parent_age_long))] <- NA
rcmas_parent_ses_long[setdiff(names(rcmas_parent_age_long), names(rcmas_parent_ses_long))] <- NA
rcmas_parent_age_long[setdiff(names(rcmas_parent_ses_long), names(rcmas_parent_age_long))] <- NA
gender_race_rcmas_parent_merge <- rbind(cbind(rcmas_parent_gender_long,origin='gender'),cbind(rcmas_parent_race_long,origin='race'),
                                        cbind(rcmas_parent_ses_long,origin='ses'),
                                        cbind(rcmas_parent_age_long,origin='age')) 

#################################################################################################################################################
rcmas_self <- subset(rcmas, form == 'self')
race_self <- subset(rcmas_self,select = grep("Q16|Q17|Q18|Q19|Q20|Q21|Q22", names(rcmas_self), value=TRUE))
race_self[race_self == "" ] <- NA

rcmas_self$race <- ifelse(!is.na(race_self$Q16) == rep("TRUE",length(race_self$Q16)), 'American indian',
                          ifelse(!is.na(race_self$Q17) == rep("TRUE",length(race_self$Q17)),"Asian",
                                 ifelse(!is.na(race_self$Q18) == rep("TRUE",length(race_self$Q18)),"Black",
                                        ifelse(!is.na(race_self$Q19) == rep("TRUE",length(race_self$Q19)),"Mideast",
                                               ifelse(!is.na(race_self$Q20) == rep("TRUE",length(race_self$Q20)),"Native Hawaiian",
                                                      ifelse(!is.na(race_self$Q21) == rep("TRUE",length(race_self$Q21)),"White",
                                                             ifelse(!is.na(race_self$Q22) == rep("TRUE",length(race_self$Q22)),"Others", "NA")))))))
rcmas_self_race <- lapply(rcmas_self[c("race")],function(x) tblFun(x))
census_race_rcmas_self = c(5.27,11.88,12.11,2.4,0.1,65.24)
dd_race_srs_rcmas_self <- data.frame(N = ceiling(census_race_rcmas_self * 400/100),round(census_race_rcmas_self,2))
names(dd_race_srs_rcmas_self) <- c("target_N","census_%")
rownames(dd_race_srs_rcmas_self) <- rownames(rcmas_self_race$race)
rcmas_self_nation <- cbind(rcmas_self_race$race,dd_race_srs_rcmas_self)
rcmas_self_nation[,paste0('complete_%')] = ifelse(is.na(rcmas_self_nation[,1]) == "FALSE", 
                                                  round(rcmas_self_nation[,1]/sum(rcmas_self_nation[,1])*100,1), NA)
rcmas_self_nation_race <- rcmas_self_nation[, c(1, 2, 4, 3)]
colnames(rcmas_self_nation_race)[1] <- "N"
rcmas_self_nation_new1 <- tibble::rownames_to_column(rcmas_self_nation_race, "race")
rcmas_self_race_long <- reshape(rcmas_self_nation_new1, idvar = "race", varying = list(c(2,3), c(4,5)), 
                                v.names=c("n","percent"), times=c("current","target"), direction="long")
rownames(rcmas_self_race_long) <- NULL
rcmas_self_gender <- lapply(rcmas_self[c("Q9")],function(x) tblFun(x))
census_gender_rcmas_self <- c(50.5,49.5)
dd_gender_rcmas_self <- data.frame(N = round(census_gender_rcmas_self * 400/100,0),round(census_gender_rcmas_self,2))

names(dd_gender_rcmas_self) <- c("target_N","census_%")
rownames(dd_gender_rcmas_self) <- rownames(rcmas_self_gender$Q9) 
rcmas_self_nation_gender <- cbind(rcmas_self_gender$Q9,dd_gender_rcmas_self)
rcmas_self_nation_gender[,paste0('complete_%')] = ifelse(is.na(rcmas_self_nation_gender[,1]) == "FALSE", 
                                                         round(rcmas_self_nation_gender[,1]/sum(rcmas_self_nation_gender[,1])*100,1), NA)
rcmas_self_nation_gender_new <- rcmas_self_nation_gender[, c(1, 2, 4, 3)]
rownames(rcmas_self_nation_gender_new) <- c("Female","Male")
colnames(rcmas_self_nation_gender_new)[1] <- "N"
rcmas_self_nation_new2 <- tibble::rownames_to_column(rcmas_self_nation_gender_new, "gender")
rcmas_self_gender_long <- reshape(rcmas_self_nation_new2, idvar = "gender", varying = list(c(2,3), c(4,5)), 
                                  v.names=c("n","percent"), times=c("current","target"), direction="long")
rownames(rcmas_self_gender_long) <- NULL
rcmas_self_ses <- lapply(rcmas_self[c("Q26")],function(x) tblFun(x))
census_ses_rcmas_self <- c(31.7,25.7,11.9,30.6)
dd_ses_rcmas_self <- data.frame(N = round(census_ses_rcmas_self * 400/100,0),round(census_ses_rcmas_self,2))
names(dd_ses_rcmas_self) <- c("target_N","census_%")
rownames(dd_ses_rcmas_self) <- rownames(rcmas_self_ses$Q26)
rcmas_self_nation_ses <- cbind(rcmas_self_ses$Q26,dd_ses_rcmas_self)
rcmas_self_nation_ses[,paste0('complete_%')] = ifelse(is.na(rcmas_self_nation_ses[,1]) == "FALSE", 
                                                      round(rcmas_self_nation_ses[,1]/sum(rcmas_self_nation_ses[,1])*100,1), NA)
rcmas_self_nation_ses_new <- rcmas_self_nation_ses[, c(1, 2, 4, 3)]
rownames(rcmas_self_nation_ses_new) <- c("Bachelor or plus ","High school","No high school","Some associate degree")
colnames(rcmas_self_nation_ses_new)[1] <- "N"
rcmas_self_nation_new3 <- tibble::rownames_to_column(rcmas_self_nation_ses_new, "ses")
rcmas_self_ses_long <- reshape(rcmas_self_nation_new3 , idvar = "ses", varying = list(c(2,3), c(4,5)), 
                               v.names=c("n","percent"), times=c("current","target"), direction="long")
rownames(rcmas_self_ses_long) <- NULL
rcmas_self_age <- lapply(rcmas_self[c("Q8")],function(x) tblFun(x))
census_age_rcmas_self = c(rep(6,16))
dd_age_rcmas_self <- data.frame(N = ceiling(census_age_rcmas_self * 1200/100),round(census_age_rcmas_self,2))
names(dd_age_rcmas_self) <- c("target_N","census_%")
rownames(dd_age_rcmas_self) <- names(table(rcmas_self$Q8))
rcmas_self_nation_age <- cbind(rcmas_self_age,dd_age_rcmas_self)
rcmas_self_nation_age[,paste0('complete_%')] = ifelse(is.na(rcmas_self_nation_age[,1]) == "FALSE", 
                                                      round(rcmas_self_nation_age[,1]/sum(rcmas_self_nation_age[,1])*100,1), NA)
rcmas_self_nation_age <- rcmas_self_nation_age[, c(1, 2, 4, 3)]
colnames(rcmas_self_nation_age)[1] <- "N"
rcmas_self_nation_new_age<- tibble::rownames_to_column(rcmas_self_nation_age, "age")
rcmas_self_age_long <- reshape(rcmas_self_nation_new_age, idvar = "age", varying = list(c(2,3), c(4,5)), 
                               v.names=c("n","percent"), times=c("current","target"), direction="long")
rownames(rcmas_self_age_long) <- NULL
rcmas_self_gender_long[setdiff(names(rcmas_self_race_long), names(rcmas_self_gender_long))] <- NA
rcmas_self_race_long[setdiff(names(rcmas_self_gender_long), names(rcmas_self_race_long))] <- NA
rcmas_self_gender_long[setdiff(names(rcmas_self_ses_long), names(rcmas_self_gender_long))] <- NA
rcmas_self_ses_long[setdiff(names(rcmas_self_gender_long), names(rcmas_self_ses_long))] <- NA
rcmas_self_race_long[setdiff(names(rcmas_self_ses_long), names(rcmas_self_race_long))] <- NA
rcmas_self_ses_long[setdiff(names(rcmas_self_race_long), names(rcmas_self_ses_long))] <- NA
rcmas_self_gender_long[setdiff(names(rcmas_self_age_long), names(rcmas_self_gender_long))] <- NA
rcmas_self_age_long[setdiff(names(rcmas_self_gender_long), names(rcmas_self_age_long))] <- NA
rcmas_self_race_long[setdiff(names(rcmas_self_age_long), names(rcmas_self_race_long))] <- NA
rcmas_self_age_long[setdiff(names(rcmas_self_race_long), names(rcmas_self_age_long))] <- NA
rcmas_self_ses_long[setdiff(names(rcmas_self_age_long), names(rcmas_self_ses_long))] <- NA
rcmas_self_age_long[setdiff(names(rcmas_self_ses_long), names(rcmas_self_age_long))] <- NA
gender_race_rcmas_self_merge <- rbind(cbind(rcmas_self_gender_long,origin='gender'),cbind(rcmas_self_race_long,origin='race'),
                                      cbind(rcmas_self_ses_long,origin='ses'),
                                      cbind(rcmas_self_age_long,origin='age')) 
final <- rbind(cbind(gender_race_rcmas_teacher_merge,form = 'teacher'),cbind(gender_race_rcmas_parent_merge,form='parent'),
               cbind(gender_race_rcmas_self_merge,form='self'),cbind(gender_race_rcmas_age_merge,form='all'))

# Run the application 
shinyApp(ui = ui, server = server)
