
library(shiny)
library(matlab)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(randomForest)
library(Metrics)

students <- read.csv('student_data.csv')
students$sex <- factor(students$sex, levels = c('Male', 'Female'))
students$famrel <- factor(students$famrel, levels = c('Very Bad', 'Bad', 'Moderate', 'Good', 'Excellent'))
students$freetime <- factor(students$freetime, levels = c('None', 'Little', 'Moderate', 'A Lot', 'Exponential'))
students$goout <- factor(students$goout, levels = c('Never', 'Ocassionally', 'Often', 'Frequently', 'Very Frequently'))
students$health <- factor(students$health, levels = c('Very Bad', 'Bad', 'Moderate', 'Good', 'Excellent'))
students$Dalc <- factor(students$Dalc, levels = c('None to Very Low', 'Low', 'Moderate', 'High', 'Very High'))
students$Walc <- factor(students$Walc, levels = c('None to Very Low', 'Low', 'Moderate', 'High', 'Very High'))

students$schoolsup <- as.factor(students$schoolsup)
students$famsup <- as.factor(students$famsup)
students$paid <- as.factor(students$paid)
students$activities <- as.factor(students$activities)
students$higher <- as.factor(students$higher)
students$internet <- as.factor(students$internet)
students$romantic <- as.factor(students$romantic)

train_set <- read.csv('student_train.csv')
train_set$Walc <- as.factor(train_set$Walc)
train_set$sex <- as.factor(train_set$sex)
train_set$famrel <- as.factor(train_set$famrel)
train_set$freetime <- as.factor(train_set$freetime)
train_set$goout <- as.factor(train_set$goout)
train_set$health <- as.factor(train_set$health)
train_set$Dalc <- as.factor(train_set$Dalc)

train_set$schoolsup <- as.factor(train_set$schoolsup)
train_set$famsup <- as.factor(train_set$famsup)
train_set$paid <- as.factor(train_set$paid)
train_set$activities <- as.factor(train_set$activities)
train_set$higher <- as.factor(train_set$higher)
train_set$internet <- as.factor(train_set$internet)
train_set$romantic <- as.factor(train_set$romantic)

test_set <- read.csv('student_test.csv')
test_set$Walc <- as.factor(test_set$Walc)
test_set$sex <- as.factor(test_set$sex)
test_set$famrel <- as.factor(test_set$famrel)
test_set$freetime <- as.factor(test_set$freetime)
test_set$goout <- as.factor(test_set$goout)
test_set$health <- as.factor(test_set$health)
test_set$Dalc <- as.factor(test_set$Dalc)

test_set$schoolsup <- as.factor(test_set$schoolsup)
test_set$famsup <- as.factor(test_set$famsup)
test_set$paid <- as.factor(test_set$paid)
test_set$activities <- as.factor(test_set$activities)
test_set$higher <- as.factor(test_set$higher)
test_set$internet <- as.factor(test_set$internet)
test_set$romantic <- as.factor(test_set$romantic)

levels(test_set$sex) <- levels(train_set$sex)
levels(test_set$Walc) <- levels(train_set$Walc)
levels(test_set$health) <- levels(train_set$health)
levels(test_set$famrel) <- levels(train_set$famrel)
levels(test_set$goout) <- levels(train_set$goout)
levels(test_set$freetime) <- levels(train_set$freetime)
levels(test_set$Dalc) <- levels(train_set$Dalc)
levels(test_set$activities) <- levels(train_set$activities)
levels(test_set$higher) <- levels(train_set$higher)
levels(test_set$internet) <- levels(train_set$internet)
levels(test_set$romantic) <- levels(train_set$romantic)
levels(test_set$schoolsup) <- levels(train_set$schoolsup)
levels(test_set$famsup) <- levels(train_set$famsup)
levels(test_set$paid) <- levels(train_set$paid)

#initialize fields with dummy values
sex_field <- 'b'
freetime_field <- 'c'
walc_field <- 'd'
fam_field <- 'e'
health_field <- 'f'
g1_field <- 6777777
g2_field <- 7777777
abs_field <- 8777777
study_field <- 9777777
index <- 1777777

#Importing model
model_rf <- readRDS(file = './rf.rda')
y_pred = predict(model_rf, newdata = test_set)
mae_rf = mae(test_set$G3, y_pred)
rmse_rf = rmse(test_set$G3, y_pred)

#R Shiny UI
ui <- dashboardPage(
    
    dashboardHeader(title = 'Student Grade Predictor', titleWidth = 290),
    
    #Sidebar layout
    dashboardSidebar(width = 290,
                     sidebarMenu(menuItem("Plots", tabName = "plots", icon = icon('poll')),
                                 menuItem("Dashboard", tabName = "dash", icon = icon('tachometer-alt')),
                                 menuItem("Prediction", tabName = 'pred', icon = icon('search')))),
    
    #Tabs layout
    dashboardBody(tags$head(tags$style(HTML('.main-header .logo {font-weight: bold;}'))),
                  #Plots tab content
                  tabItems(tabItem('plots', 
                                   #Histogram filter
                                   box(status = 'primary', title = 'Filter for the histogram plot',
                                       selectInput('num', "Numerical variables:", c('Age', 'Study Time', 'Failures', 'Absences', 'Period 1 Math Grade', 'Period 2 Math Grade', 'Final Math Grade')),
                                       footer = 'Histogram plot for numerical variables'),
                                   #Frecuency plot filter
                                   box(status = 'primary', title = 'Filter for the frequency plot',
                                       selectInput('cat', 'Categorical variables:', c('Sex', 'Extra Educational Support', 'Family Educational Support', 'Related Extra Paid Classes', 'Activities', 'Higher Education', 'Internet Access', 'Romantic Pursuits', 'Home Life', 'Free Time', 'Outgoing', 'Daily Alcohol Consumption', 'Weekly Alcohol Consumption', 'Health')),
                                       footer = 'Frequency plot for categorical variables'),
                                   #Boxes to display the plots
                                   box(plotOutput('histPlot')),
                                   box(plotOutput('freqPlot'))),
                           
                           #Dashboard tab content
                           tabItem('dash',
                                   #Dashboard filters
                                   box(title = 'Filters', status = 'primary', width = 12,
                                       splitLayout(cellWidths = c('4%', '32%', '40%'),
                                                   div(),
                                                   radioButtons('sex', 'Sex:', c('Male', 'Female', 'Both')),
                                                   radioButtons('absences', 'Anomalies:', c('Absences', 'Failures')),
                                                   radioButtons('dalc', 'Daily Alcohol Consumption:', c('All', 'None to Very Low', 'Low', 'Moderate', 'High', 'Very High')))),
                                   #Boxes to display the plots
                                   box(plotOutput('linePlot')),
                                   box(plotOutput('barPlot'), 
                                       height = 330)),
                           
                           #Prediction tab content
                           tabItem('pred',
                                   #Filters for categorical variables
                                   box(title = 'Categorical variables', status = 'primary', width = 12,
                                       splitLayout(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                                   cellWidths = c('0%', '12%', '4%', '13%', '4%', '19%', '4%', '19%', '4%', '13%'),
                                                   selectInput('p_sex', 'Sex', c("Male", "Female")),
                                                   div(),
                                                   selectInput('p_free', 'Free Time', c('None', 'Little', 'Moderate', 'A Lot', 'Exponential')),
                                                   div(),
                                                   selectInput('p_walc', 'Weekly Alcohol Consumption', c('None to Very Low', 'Low', 'Moderate', 'High', 'Very High')),
                                                   div(),
                                                   selectInput('p_fam', 'Home Life', c('Very Bad', 'Bad', 'Moderate', 'Good', 'Excellent')),
                                                   div(),
                                                   selectInput('p_health', 'Student Health', c('Very Bad', 'Bad', 'Moderate', 'Good', 'Excellent')))),
                                   #Filters for numeric variables
                                   box(title = 'Numerical variables', status = 'primary', width = 12,
                                       splitLayout(cellWidths = c('22%', '4%','21%', '4%', '21%', '4%', '21%'),
                                                   sliderInput('p_g1', 'Period 1 Math Grade', min = 0, max = 20, value = 0),
                                                   div(),
                                                   sliderInput('p_g2', 'Period 2 Math Grade', min = 0, max = 20, value = 0),
                                                   div(),
                                                   sliderInput('p_abs', 'Absences', min = 0, max = 80, value = 0),
                                                   div(),
                                                   sliderInput('p_study', 'Daily Study Time (Hrs)', min = 0, max = 4, value = 0))),
                                   #Box to display the prediction results
                                   box(title = 'Prediction result', status = 'success', solidHeader = TRUE, width = 4, height = 260,
                                       div(h5('Final Math Grade (out of 20):')),
                                       verbatimTextOutput("value", placeholder = TRUE),
                                       div(h5('Range of Final Math Grade (out of 20):')),
                                       verbatimTextOutput("range", placeholder = TRUE),
                                       actionButton('cal','Calculate', icon = icon('calculator'))),
                                   #Box to display information about the model
                                   box(title = 'Model explanation', status = 'success', width = 8, height = 310,
                                       helpText('The following model will predict the final grade of a secondary school student on the subject Math taking into consideration major contributing factors such as his/her home life, allocation of free time, and health etc.'),
                                       helpText('The model for this case study was trained using the sub-dataset from the "Student Alcohol Consumption Dataset" from the Kaggle website, which contains 395 observations and 33 attributes related to various aspects of Secondary School Math Students from two particular schools in Portugal.'),
                                       helpText(sprintf('The prediction is based on a random forest supervised machine learning model. Furthermore, the models deliver a mean absolute error (MAE) of %s for a student\'s final grade in Math, and a root mean squared error (RMSE) of %s for the student\'s final grade in Math.', round(mae_rf, digits = 3), round(rmse_rf, digits = 3)))))
                  )
    )
)


# R Shiny server
server <- shinyServer(function(input, output) {
    
    output$histPlot <- renderPlot({
        
        #Column name variable
        num_val = ifelse(input$num == 'Age', 'age',
                         ifelse(input$num == 'Study Time', 'studytime',
                                ifelse(input$num == 'Failures', 'failures',
                                       ifelse(input$num == 'Absences', 'absences',
                                              ifelse(input$num == 'Period 1 Math Grade', 'G1',
                                                     ifelse(input$num == 'Period 2 Math Grade', 'G2', 'G3'))))))
        
        #Histogram plot
        ggplot(data = students, aes(x = students[[num_val]]))+ 
            geom_histogram(stat = "bin", fill = 'steelblue3', color = 'lightgrey')+
            theme(axis.text = element_text(size = 12),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(size = 16, face = 'bold'))+
            labs(title = sprintf('Histogram plot of the variable %s', num_val),
                 x = sprintf('%s', input$num),y = 'Frequency')+
            stat_bin(geom = 'text', 
                     aes(label = ifelse(..count.. == max(..count..), as.character(max(..count..)), '')),
                     vjust = -0.6)
        
    })
    
    output$freqPlot <- renderPlot({
        
        #Column name variable
        cat_val = ifelse(input$cat == 'Sex', 'sex',
                         ifelse(input$cat == 'Extra Educational Support', 'schoolsup',
                                ifelse(input$cat == 'Family Educational Support', 'famsup',
                                       ifelse(input$cat == 'Related Extra Paid Classes', 'paid',
                                              ifelse(input$cat == 'Activities', 'activities',
                                                     ifelse(input$cat == 'Higher Education', 'higher',
                                                            ifelse(input$cat == 'Internet Access', 'internet',
                                                                   ifelse(input$cat == 'Romantic Pursuits', 'romantic',
                                                                          ifelse(input$cat == 'Home Life', 'famrel',
                                                                                 ifelse(input$cat == 'Free Time', 'freetime',
                                                                                        ifelse(input$cat == 'Outgoing', 'goout',
                                                                                               ifelse(input$cat == 'Daily Alcohol Consumption', 'Dalc',
                                                                                                      ifelse(input$cat == 'Weekly Alcohol Consumption', 'Walc',
                                                                                                             'health')))))))))))))
        
        #Frecuency plot
        ggplot(data = students, aes(x = students[[cat_val]]))+
            geom_bar(stat = 'count', fill = 'mediumseagreen', 
                     width = 0.5)+
            stat_count(geom = 'text', size = 4,
                       aes(label = ..count..),
                       position = position_stack(vjust = 1.03))+
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.text = element_text(size = 12),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(size = 16, face="bold"))+
            labs(title = sprintf('Frecuency plot of the variable %s', cat_val),
                 x = sprintf('%s', input$cat), y = 'Count')
        
    })
    
    #Dashboard analysis
    output$linePlot <- renderPlot({
        if(input$sex != 'Both'){
            
            #Creating a table filter by sex for the line plot
            counts <- students %>% group_by(Walc) %>% filter(sex == input$sex) %>% summarise(absences = sum(absences), failures = sum(failures))
            
        } else{
            
            #Creating a table for the line plot
            counts <- students %>% group_by(Walc) %>% summarise(absences = sum(absences), failures = sum(failures))
            
        }
        
        #Column name variable
        abs_val = ifelse(input$absences == 'Absences', 'absences', 'failures')
        
        #Line plot
        ggplot(counts, aes(x = Walc, y = counts[[abs_val]],
                           group = 1))+
            geom_line(size = 1.25)+
            geom_point(size = 2.25,
                       color = ifelse(counts[[abs_val]] == max(counts[[abs_val]]), 'red','black'))+
            labs(title = sprintf('%s & Weekly Alcohol Consumption', input$absences),
                 subtitle = sprintf('Gender: %s \nMaximum # of %s: %s \nTotal # of %s: %s', input$sex, abs_val, max(counts[[abs_val]]), abs_val, sum(counts[[abs_val]])),
                 x = 'Weekly Alcohol Consumption', y = 'Total Aggregation')+
            theme(axis.text = element_text(size = 12),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(size = 16, face = 'bold'),
                  plot.subtitle = element_text(size = 14))+
            ylim(NA, 800)+
            geom_text(aes(label = ifelse(counts[[abs_val]] == max(counts[[abs_val]]), as.character(counts[[abs_val]]),'')),
                      col ='red',hjust = 0.5, vjust = -0.7)
        
    })
    
    output$barPlot <- renderPlot({
        
        if(input$sex != 'Both'){
            
            if(input$dalc != 'All'){
                
                #Creating a table filter by famrel and Dalc for the bar plot
                alc <- students %>% group_by(famrel, Dalc) %>% filter(sex == input$sex) %>%  summarise(absences = sum(absences), failures = sum(failures))
                
                alc <- alc %>% filter(Dalc == input$dalc)
                
            } else{
                
                #Creating a table filter by famrel for the bar plot
                alc <- students %>% group_by(famrel, Dalc) %>% filter(sex == input$sex) %>%  summarise(absences = sum(absences), failures = sum(failures))
                
            }
            
        } else{
            
            if(input$dalc != 'All'){
                
                #Creating a table filter by Dalc for the bar plot
                alc <- students %>% group_by(famrel, Dalc) %>% filter(Dalc == input$dalc) %>%  summarise(absences = sum(absences), failures = sum(failures))
                
            } else{
                
                #Creating a table for the bar plot
                alc <- students %>% group_by(famrel, Dalc) %>%  summarise(absences = sum(absences), failures = sum(failures))
                
            }
        }
        
        #Column name variable
        abs_val = ifelse(input$absences == 'Absences', 'absences', 'failures')
        
        #Bar plot
        ggplot(alc, aes(x = famrel, y = alc[[abs_val]], 
                        fill = Dalc))+
            geom_bar(stat = 'identity', position=position_dodge())+
            geom_text(aes(label = alc[[abs_val]]),
                      vjust = -0.3, position = position_dodge(0.9), 
                      size = 4)+
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.text = element_text(size = 12),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(size = 16, face = 'bold'),
                  plot.subtitle = element_text(size = 14),
                  legend.text = element_text(size = 12))+
            ylim(NA, 200)+
            labs(title = sprintf('%s rate per Alcohol & Home Life', input$absences),
                 subtitle = sprintf('Gender: %s', input$sex),
                 x = 'Quality of Home Life', 
                 y = sprintf('%s', input$absences))+
            scale_fill_manual(values = c('None to Very Low' = 'salmon2', 'Low' = 'steelblue3', 'Moderate' = 'mediumseagreen', 'High' = 'tomato4', 'Very High' = 'blueviolet'),
                              name = 'Daily Alcohol Consumption')
    })
    
    #Prediction model
    #React value when using the action button
    a <- reactiveValues(result = NULL)
    
    observeEvent(input$cal, {
        #Copy of the test data without the dependent variable
        test_pred <- test_set[-21]
        if(strcmp(input$p_sex, sex_field) & strcmp(input$p_free, freetime_field) & strcmp(input$p_walc, walc_field) & strcmp(input$p_fam, fam_field) & strcmp(input$p_health, health_field) & (input$p_g1 == g1_field) & (input$p_g2 == g2_field) & (input$p_abs == abs_field) & (input$p_study == study_field)) {
            index <<- index
        }
        else {
            sex_field <<- input$p_sex
            freetime_field <<- input$p_free
            walc_field <<- input$p_walc
            fam_field <<- input$p_fam
            health_field <<- input$p_health
            g1_field <<- input$p_g1
            g2_field <<- input$p_g2
            abs_field <<- input$p_abs
            study_field <<- input$p_study
            index <<- floor(runif(1, min=1, max=77))
        }
        #Dataframe for the single prediction
        values = data.frame(sex = sex_field, 
                            freetime = freetime_field,
                            Walc = walc_field,
                            famrel = fam_field,
                            health = health_field,
                            G1 = g1_field,
                            G2 = g2_field, 
                            absences = abs_field, 
                            studytime = study_field)
        
        values$sex <- factor(values$sex)
        values$freetime <- factor(values$freetime)
        values$Walc <- factor(values$Walc)
        values$famrel <- factor(values$famrel)
        values$health <- factor(values$health)
        
        test_pred <- bind_rows(test_pred,values)
        
        a$result <-  round(predict(model_rf, 
                                   newdata = test_pred[index,]), 
                           digits = 0)
    })
    
    output$value <- renderText({
        #Display the prediction value
        paste(a$result)
    })
    
    output$range <- renderText({
        #Display the range of prediction value using the MAE value
        input$cal
        isolate(sprintf('(%s) - (%s)', 
                        round(a$result - mae_rf, digits = 0), 
                        round(a$result + mae_rf, digits = 0)))
    })
    
})

shinyApp(ui, server)  
