#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
#install.packages("gridExtra")
library(gridExtra)
library(stats)
library(readxl)
library(ggcorrplot)
library(MASS)

data <- read_excel("Final project data viz version.xlsx")
data <- data[,(-5)]

data <- data.frame(data)
data <- na.omit(data)

data$What.are.your.post.HS.Plans <- as.factor(data$What.are.your.post.HS.Plans)
data$Are.you.currently.employed. <- as.factor(data$Are.you.currently.employed.)
data$ethnicity <- as.factor(data$ethnicity)
data$race <- as.factor(data$race)
data$Graduated <- as.factor(data$Graduated)
data$Graduated.Early <- as.factor(data$Graduated.Early)
data$Graduated.on.Time <- as.factor(data$Graduated.on.Time)
data$Hardship.Diploma <- as.factor(data$Hardship.Diploma)
data$Virtual <- as.factor(data$Virtual)
data$any_GU_part_yn <- as.factor(data$any_GU_part_yn)
data$ELL_merged <- as.factor(data$ELL_merged)
data$gender_merged <- as.factor(data$gender_merged)
data$school_merged <- as.factor(data$school_merged)
data$FRL_merged <- as.factor(data$FRL_merged)
data$SPED_merged <- as.factor(data$SPED_merged)

subset <- data.frame(cbind(data$Are.you.currently.employed., data$Average.score,
                           data$race, data$Virtual, data$any_GU_part_yn,
                           data$cumGpa, data$ELL_merged, data$gender_merged, data$FRL_merged, data$SPED_merged))
colnames(subset) <- c("Are.you.currently.employed.","Average.score",
                      "race", "Virtual", "any_GU_part_yn",
                      "cumGpa", "ELL_merged", "gender_merged", "FRL_merged", "SPED_merged")
model.matrix(~0+., data=subset) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)
ggsave(filename = "Correlations for Satisfaction.png", units = "cm", 
       width = 25, height = 18, dpi = 600)



bestlm <- lm(Average.score ~ school_merged + race + gender_merged + What.are.your.post.HS.Plans, data = data)
summary(bestlm)


regs <- lm(Average.score ~ school_merged + race + gender_merged + Are.you.currently.employed. + 
             What.are.your.post.HS.Plans + ELL_merged + SPED_merged + FRL_merged + cumGpa + Virtual + any_GU_part_yn, data = data)
summary(regs)

step3 <- stepAIC(regs, trace = FALSE)
step3$anova


bestmodel <- lm(Average.score ~ school_merged + race + Are.you.currently.employed. + 
                  What.are.your.post.HS.Plans + ELL_merged + cumGpa + Virtual + 
                  any_GU_part_yn, data = data)
summary(bestmodel)$coefficients
anova(bestmodel)
newdata <-with(data,
               data.frame(
                 cumGpa = rep(seq(min(cumGpa, na.rm = TRUE), max(cumGpa, na.rm = TRUE), length.out = 100),2),
                 race = as.factor(rep(c(1), times = 200)),
                 Are.you.currently.employed. = as.factor(rep(c(0),200)),
                 gender_merged = as.factor(rep(c(0),200)),
                 any_GU_part_yn = as.factor(rep(c(0),200))
               )
)


bestmodel <- lm(Average.score ~ school_merged + race + Are.you.currently.employed. + 
                  What.are.your.post.HS.Plans + ELL_merged + cumGpa + Virtual + 
                  any_GU_part_yn, data = data)

summary(bestmodel)

ui <- fluidPage(

    titlePanel("High School Satisfaction"),
verticalLayout(
    sidebarLayout(
      selectInput("Race", h3("Select box"),
                  choices = list("Hispanic/Latino" = 1,  
                                 "Asian" = 3, "Black or African American" = 4, 
                                 "White" = 6)),

    mainPanel(
           plotOutput("distplot1")
        )
    )
),

sidebarLayout(
  sidebarPanel(
    selectInput("ELL", h3("Select box"),
                choices = list("ELL" = 1, "Not ELL" = 0))
    ),
    
    mainPanel(
      plotOutput("distplot2")
    )
  ),


sidebarLayout(
  sidebarPanel(
    selectInput("FRL", h3("Select box"),
                choices = list("Free Lunch" = 2, "Reduced Price" = 1, "Full Price" = 0))
  ),
  
  mainPanel(
    plotOutput("distplot3")
  )
),


sidebarLayout(
  sidebarPanel(
    selectInput("Post.HS.Plans", h3("Select box"),
                choices = list("College", "Work", "Military", "Other"))
  ),
  
  mainPanel(
    plotOutput("distplot4")
  )
),
titlePanel("Prediction"),
sidebarLayout(
  sidebarPanel(
    helpText("Create an example student to predict their satisfaction with the district!"),
    selectInput("Prediction.a", label = "race", choices = list("Hispanic/Latino" = 1,  "Asian" = 3, "Black or African American" = 4, 
                               "White" = 6)),
          selectInput("Prediction.b", label = "ELL", choices = list("ELL" = 1, "Not ELL" = 0)),
          selectInput("Prediction.c", label = "FRL", choices = list("Free Lunch" = 2, "Reduced Price" = 1, "Full Price" = 0)),
          selectInput("Prediction.d", label = "Plans", choices = list("College", "Work", "Military", "Other")),
          selectInput("Prediction.e", label = "School", choices = list("School 1" = 1, "School 2" = 2, "School 3" = 3, "School 4" = 4, "School 5" = 5)),
          selectInput("Prediction.f", label = "Employed", choices = list("Currently Employed" = 1, "Not Currently Employed" = 0)),
          selectInput("Prediction.g", label = "GEAR UP", choices = list("Has participated in GU" = 1, "Has not participated in GU" = 0)),
          selectInput("Prediction.i", label = "Virtual", choices = list("Is virtual" = 1, "Not virtual" =0)),
          selectInput("Prediction.h", label = "GPA", choices = list(1,1.5,2,2.5,3,3.5,4,4.5))
    ),
  
  mainPanel(
    textOutput("selected_var")
  )
),
)


server <- function(input, output, session) {

    output$distplot1 <- renderPlot({
        x    <- data$Average.score[data$race == input$Race]
        bins <- 30

        hist(x, breaks = bins, col = "blue", border = 'white',
             xlab = 'Average Satisfaction Score',
             main = 'Histogram of Satisfaction by Race/Ethnicity')
    })
    output$distplot2 <- renderPlot({
      y    <- data$Average.score[data$ELL_merged == input$ELL]
      bins <- 30
      
      hist(y, breaks = bins, col = "red", border = 'white',
           xlab = 'Average Satisfaction Score',
           main = 'Histogram of Satisfaction by ELL Status')
      
    })
    
    output$distplot3 <- renderPlot({
      z    <- data$Average.score[data$FRL_merged == input$FRL]
      bins <- 30
      
      hist(z, breaks = bins, col = "green", border = 'white',
           xlab = 'Average Satisfaction Score',
           main = 'Histogram of Satisfaction by FRL Status')
      
    })
    output$distplot4 <- renderPlot({
      a    <- data$Average.score[data$What.are.your.post.HS.Plans == input$Post.HS.Plans]
      bins <- 30
      
      hist(a, breaks = bins, col = "purple", border = 'white',
           xlab = 'Average Satisfaction Score',
           main = 'Histogram of Satisfaction by FRL Status')
      
    })
    
    output$selected_var <- renderPrint({
      bestmodel <- lm(Average.score ~ school_merged + race + Are.you.currently.employed. + 
                        What.are.your.post.HS.Plans + ELL_merged + cumGpa + Virtual + 
                        any_GU_part_yn, data = data)
      
      predt <- data.frame(race = as.factor(input$Prediction.a), 
                               ELL_merged = as.factor(input$Prediction.b), 
                               FRL_merged = as.factor(input$Prediction.c), 
                               What.are.your.post.HS.Plans = as.factor(input$Prediction.d),
                              school_merged = as.factor(input$Prediction.e), 
                              Are.you.currently.employed. = as.factor(input$Prediction.f), 
                              any_GU_part_yn = as.factor(input$Prediction.g), 
                              Virtual = as.factor(input$Prediction.i), 
                              cumGpa = as.numeric(input$Prediction.h))
     b <- predict(bestmodel, predt)
    b <- round(b,2)
     paste("Your student would likely rate the district a ", b, "out of 5!")
      
    })
  }


shinyApp(ui = ui, server = server)
