#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(pen=999)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(survey)
library(dplyr)
library(ggplot2)
library(wesanderson)
library(data.table)

set.seed(0)


rm(list=ls())

raw=read.csv("C:\\Users\\Tress\\OneDrive\\Desktop\\Learn\\Data Challenge\\Rawds.csv",header=TRUE,sep=',',na.strings=c("","N/A"," ","NA",'NULL'))
head(raw)
proportions <- function(check_val='Infosources'){
    #print(check_val)
    switch(check_val,
           Infosources = {
               analysis_data_infosource=raw %>%
                   select(pers_wgt,fc_05a,	fc_05b,	fc_05c,	fc_05d,	fc_05e,	fc_05f,	fc_05g,	fc_05h,	fc_05i,	fc_05j,	fc_05k,	fc_05l) 
               col_names=c("pers_wgt",
                           "Social media posts from users/influencers",
                           "Social media posts from news orgs, magazines",
                           "Online newspapers or news sites",
                           "Online magazine on current events",
                           "Online forums",
                           "Online encyclopedia or repository",
                           "Blogs",
                           "Podcasts",
                           "Online video sharing platforms",
                           "Email from a friend or family",
                           "Other",
                           "Did not use Internet to find info on COVID-19")
               rel_fields=c(1)
               
           },
           Accuracy_check = {
               analysis_data_infosource=raw %>%
                   select(pers_wgt,fc_15a,	fc_15b,	fc_15c,	fc_15d,	fc_15e,	fc_15f,	fc_15g,	fc_15h)
               col_names=c("pers_wgt",
                           "Searched author/source for credibility",
                           "Consulted other sources",
                           "Clicked on the link to read the entire news article",
                           "Verified URL for credibility",
                           "Verified the date of the information",
                           "Read comments to see discussion on the topic/source",
                           "Consulted friends, family, online network",
                           "Other"
               )
               rel_fields=c(1)
           },
           Precautions = {
               analysis_data_infosource=raw %>%
                   select(pers_wgt,bh_20a,	bh_20b,	bh_20c,	bh_20d,	bh_20e,	bh_20f,	bh_20g,	bh_20h,	bh_20i,	bh_20j,	bh_20k,	bh_20l,	bh_20m,	bh_20n,	bh_20o)
               col_names=c("pers_wgt",
                           "Stocked up on essentials",
                           "Filled prescriptions",
                           "Made a plan for sick hhld members",
                           "Made a plan other non-hhld memb",
                           "Made a plan communicate",
                           "Avoided leaving the house",
                           "Used physical distancing in public",
                           "Avoided crowds and large gathering",
                           "Washed your hands more regularly",
                           "Avoided touching your face",
                           "Cancelled travel",
                           "Worked from home",
                           "Wore mask/other p.p.e",
                           "Other",
                           "None"
                           
               )
               rel_fields=c(1)
           },
           Concern = {
               analysis_data_infosource=raw %>%
                   select(pers_wgt,bh_55a,	bh_55b,	bh_55c,	bh_55d,	bh_55e,	bh_55f,	bh_55g,	bh_55h,	bh_55i,	bh_55j,	bh_55k,	bh_55l)
               col_names=c("pers_wgt",
                           "My own health",
                           "Member of household’s health",
                           "Vulnerable people’s health",
                           "Canadian population’s health",
                           "World population’s health",
                           "Overloading the health system",
                           "Civil disorder",
                           "Maintaining social ties",
                           "Ability to support in crisis",
                           "Ability to support post-crisis",
                           "Family stress from confinement",
                           "Violence in the home")
               rel_fields=c(2,3,4)
           },
           Activities= {
               analysis_data_infosource=raw %>%
                   select(pers_wgt,bh_35a,	bh_35b,	bh_35c,	bh_35d,	bh_35e)
               col_names=c("pers_wgt",
                           "Communication with friends and family",
                           "Meditation",
                           "Exercise outdoors",
                           "Exercise indoors",
                           "Changing my food choices")
               rel_fields=c(1,2,3)
           },
           Habits = {
               analysis_data_infosource=raw %>%
                   select(pers_wgt,bh_40a,bh_40b,bh_40c,	bh_40d,	bh_40e,	bh_40f,	bh_40g,	bh_40h)
               col_names=c("pers_wgt",
                           "Consuming alcohol",
                           "Using tobacco products",
                           "Consuming cannabis",
                           "Eating junk food or sweets",
                           "Watching television",
                           "Spending time on the internet",
                           "Playing video games",
                           "Playing board games")
               rel_fields=c(1)
           },
           Lastweek= {
               analysis_data_infosource=raw %>%
                   select(pers_wgt,bh_60a,bh_60b,	bh_60c)
               col_names=c("pers_wgt",
                           "Went shopping at the grocery store or drugstore",
                           "Used delivery service for groceries or drugstore",
                           "Used a food delivery service for prepared food"
               )
               rel_fields=c(1,2,3)
           }
           
    )
    colnames(analysis_data_infosource)=col_names
    tot_pers_wgt=sum(analysis_data_infosource$pers_wgt)
    prop=apply(analysis_data_infosource,2,function(x) round(100*sum(analysis_data_infosource$pers_wgt[x%in%rel_fields])/tot_pers_wgt,1))
    proportion=t(t(prop))
    tmp=data.frame(proportion,Indicator=rownames(proportion),row.names=NULL)[-1,] 
    tmp$Indicator=factor(tmp$Indicator,levels=tmp$Indicator)
    tmp=tmp%>% arrange(desc(proportion))
    #print(tmp)
    return(tmp)
    
    
} 

# Define UI for application that draws a histogram
ui <-dashboardPage(
    dashboardHeader(title = "COVID-19 Info Sources"),
    dashboardSidebar(sidebarMenu(
        menuItem("Overview", tabName = "overview", icon = icon("th")),
        menuItem("InfoSources", icon = icon("chart-bar"), tabName = "charts",
                 badgeColor = "green"),
        menuItem("Employment", icon = icon("dashboard"), tabName = "emp",
                 badgeColor = "green"),
        menuItem("Mental Health", icon = icon("ts"), tabName = "mhlth",
                  badgeColor = "green")
    )
 
    ),
    dashboardBody(theme = shinytheme("darkly"),
                  tabItems(
                      tabItem(tabName = "overview",
                              h2("Information Sources Consulted During the Pandemic- Team Zoom"),
                              
                              fluidRow(
                                  box(width=12,title = "Overview", status = "warning", solidHeader = TRUE,
                                      collapsible = F,
                                      tags$head(
                                          tags$style(HTML("
                                                        li {
                                                        font-size: 18px;
                                    
                                                        }
                                                        li span {
                                                        font-size: 18px;
                                                        }
                                                        ul {
                                                        list-style-type: square;
                                                        }

                                                    "))
                                      ),
                                      
                                      tags$div(tags$ul(
                                          tags$li(tags$span("Over 60 % consulted Online newspaper / news sites for Covid Info.")),
                                          tags$li(tags$span("More than half Consulted other sources to check accuracy.")),
                                          tags$li(tags$span("Over 90% took precautions by washing hands, using physical distancing and avoiding crowds.")),
                                          tags$li(tags$span("More than their own health, Canadians are concerned about others health, and overburdening health system. ")),
                                          tags$li(tags$span("Less than 10% are worried about violence at home. ")),
                                          tags$li(tags$span("Spending time on internet and watching television are the most increased habits during the week. ")),
                                          tags$li(tags$span("Close to 90% of canadians went to shopping atleast once during the week. "))
                                          
                                  )
                              )
                                  
                                  )
                              ),
                              fluidRow(
                                  #pickerInput("area","Select Analysis Area",choices = c('Infosources','Accuracy_check','Precautions','Concern','Activities for health','Weekly Habits','Last week activities'),
                                  #            options = list(`actions-box` = TRUE),selected = c('Infosources','Accuracy_check','Precautions','Concern','Activities','Habits','Lastweek'),multiple = T),                                     
                                  #status = "info",
                                  title = "Survey Dataset details"
                              ),
                              fluidRow(
                                  box(width=12,title = "Use of Information Sources and Accuracy check", status = "warning", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      splitLayout(cellWidths = c("50%", "50%"),
                                                  plotOutput("Infosources"),
                                                  plotOutput("Accuracy_check"))
                                                   
                                      
                                    ),
                                  box(width=12,title = "Precautions taken to reduce risk and concerns", status = "warning", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      splitLayout(cellWidths = c("50%", "50%"),
                                                  plotOutput("Precautions"),
                                                  plotOutput("Concern"))
                                                   
                                      
                                  ),
                                  box(width=12,title = "Health Activities and Habits?", status = "warning", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      splitLayout(cellWidths = c("50%", "50%"),
                                                  plotOutput("Activities"),
                                                  plotOutput("Habits"))
                                      
                                  ),
                                  box(width=12,title = "Last week activity frequency", status = "warning", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      splitLayout(cellWidths = c("50%"),
                                                  plotOutput("Lastweek"))
                                      
                                  )
                              )
                      )
                  )
                  
    )
    
)




# Define server logic required to draw a histogram
server <- function(input, output) {
    
     
    output$Infosources<-renderPlot({
    plot_tmp=proportions('Infosources') 
    plot_tmp$Indicator=factor(plot_tmp$Indicator, levels =plot_tmp$Indicator)
    ggplot(plot_tmp,aes(x=proportion,y=as.factor(Indicator),fill=-proportion),environment = environment())+
        geom_bar(stat="identity")+
        scale_y_discrete(limits = rev(levels(plot_tmp$Indicator)))+
        scale_x_continuous(breaks=seq(0,100,10))+
        geom_bar(stat="identity")+
        ggtitle("COVID info sources") +
        ylab("")+xlab("% of respondants") + theme(legend.position="none")+theme_classic()
    })
    ### Checking accuracy of info about COVID
    output$Accuracy_check<-renderPlot({
    plot_tmp=proportions('Accuracy_check')
    plot_tmp$Indicator=factor(plot_tmp$Indicator, levels =plot_tmp$Indicator)
    ggplot(plot_tmp,aes(x=proportion,y=as.factor(Indicator),fill=-proportion),environment = environment())+
        geom_bar(stat="identity")+
        scale_y_discrete(limits = rev(levels(plot_tmp$Indicator)))+
        scale_x_continuous(breaks=seq(0,100,10))+
        geom_bar(stat="identity")+
        ggtitle("COVID Info Accuracy_check") +
        ylab("")+xlab("% of respondants") + theme(legend.position="none")+theme_classic()

    
    })
    ### Precautions taken to reduce risk
    output$Precautions<-renderPlot({
    plot_tmp=proportions('Precautions')
    plot_tmp$Indicator=factor(plot_tmp$Indicator, levels =plot_tmp$Indicator)
    ggplot(plot_tmp,aes(x=proportion,y=as.factor(Indicator),fill=-proportion),environment = environment())+
        geom_bar(stat="identity")+
        scale_y_discrete(limits = rev(levels(plot_tmp$Indicator)))+
        scale_x_continuous(breaks=seq(0,100,10))+
        geom_bar(stat="identity")+
        ggtitle("Precautions taken to reduce risk") +
        ylab("")+xlab("% of respondants") + theme(legend.position="none")+theme_classic()
    })
 ### COVID-19 Impact Concern 
    output$Concern<-renderPlot({
    plot_tmp=proportions('Concern')
    plot_tmp$Indicator=factor(plot_tmp$Indicator, levels =plot_tmp$Indicator)
    ggplot(plot_tmp,aes(x=proportion,y=as.factor(Indicator),fill=-proportion),environment = environment())+
        geom_bar(stat="identity")+
        scale_y_discrete(limits = rev(levels(plot_tmp$Indicator)))+
        scale_x_continuous(breaks=seq(0,100,10))+
        geom_bar(stat="identity")+
        ggtitle("COVID-19 Impact Concern ") +
        ylab("")+xlab("% of respondants") + theme(legend.position="none")+theme_classic()
    })
    ### Doing activities for health
    output$Activities<-renderPlot({
    plot_tmp=proportions('Activities')
    plot_tmp$Indicator=factor(plot_tmp$Indicator, levels =plot_tmp$Indicator)
    ggplot(plot_tmp,aes(x=proportion,y=as.factor(Indicator),fill=-proportion),environment = environment())+
        geom_bar(stat="identity")+
        scale_y_discrete(limits = rev(levels(plot_tmp$Indicator)))+
        scale_x_continuous(breaks=seq(0,100,10))+
        geom_bar(stat="identity")+
        ggtitle("Doing activities for health ") +
        ylab("")+xlab("% of respondants") + theme(legend.position="none")+theme_classic()
    })
    ### Change in weekly habits
    output$Habits<-renderPlot({
    plot_tmp=proportions('Habits')
    plot_tmp$Indicator=factor(plot_tmp$Indicator, levels =plot_tmp$Indicator)
    ggplot(plot_tmp,aes(x=proportion,y=as.factor(Indicator),fill=-proportion),environment = environment())+
        geom_bar(stat="identity")+
        scale_y_discrete(limits = rev(levels(plot_tmp$Indicator)))+
        scale_x_continuous(breaks=seq(0,100,10))+
        geom_bar(stat="identity")+
        ggtitle("Change in weekly habits") +
        ylab("")+xlab("% of respondants") + theme(legend.position="none")+theme_classic()
    })
    ### Freq in last week
    output$Lastweek<-renderPlot({
    plot_tmp=proportions('Lastweek')
    plot_tmp$Indicator=factor(plot_tmp$Indicator, levels =plot_tmp$Indicator)
    ggplot(plot_tmp,aes(x=proportion,y=as.factor(Indicator),fill=-proportion),environment = environment())+
        geom_bar(stat="identity")+
        scale_y_discrete(limits = rev(levels(plot_tmp$Indicator)))+
        scale_x_continuous(breaks=seq(0,100,10))+
        geom_bar(stat="identity")+
        ggtitle("Freq in last week") +
        ylab("")+xlab("% of respondants") + theme(legend.position="none")+theme_classic()
    })
    output$Habits<-renderPlot({
        plot_tmp=proportions('Habits')
        plot_tmp$Indicator=factor(plot_tmp$Indicator, levels =plot_tmp$Indicator)
        ggplot(plot_tmp,aes(x=proportion,y=as.factor(Indicator),fill=-proportion),environment = environment())+
            geom_bar(stat="identity")+
            scale_y_discrete(limits = rev(levels(plot_tmp$Indicator)))+
            scale_x_continuous(breaks=seq(0,100,10))+
            geom_bar(stat="identity")+
            ggtitle("Change in weekly habits") +
            ylab("")+xlab("% of respondants") + theme(legend.position="none")+theme_classic()
        
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
