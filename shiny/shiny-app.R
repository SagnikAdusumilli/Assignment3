library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(mice)
library(VIM)
library(pROC)
library(caret)
library(sqldf)
library(shiny)
library(data.table)

load("./nb_model.rda")

testData = read.csv("./tranied_data.csv", header = TRUE, na.strings =  c("NA","","#NA"))
clus1Num = sum(testData$cluster == 1)
clus2Num = sum(testData$cluster == 2)
clus3Num = sum(testData$cluster == 3)
table <- data.table(testData)

val1 <- nrow(table[table$y == "yes" & table$cluster == 1])
val2 <- nrow(table[table$y == "yes" & table$cluster == 2])
val3 <- nrow(table[table$y == "yes" & table$cluster == 3])




ui <- fluidPage(
  
  #pie-chart of the cluster
  plotOutput("cluster_plot"),
  
  radioButtons("cluster", "Clusters:", c("Cluster 1"= 1, "Cluster 2" = 2, "Cluster 3" = 3)),
  
  plotOutput("selectedAgePlot"),
  plotOutput("selectedAgePlotY"),
  
  plotOutput("selectedEurPlot"),
  plotOutput("selectedEurPlotY"),
  
  plotOutput("selectedJobPlot"),
  plotOutput("selectedJobPlotY"),
  
  plotOutput("selectedEduationPlot"),
  plotOutput("selectedEduationPlotY"),
  
  
  plotOutput("selectedMaritalPlot"),
  plotOutput("selectedMaritalPlotY"),
  
  plotOutput("clusterBarPlot")
  
  
  
)

server <- function(input, output) ({
  output$cluster_plot <- renderPlot ({
    pie(c(clus1Num, clus2Num, clus3Num),
        labels = c("cluster 1", "cluster 2", "cluster 3"),
        main = "Cluster distribution of the data-set")
  })
  
  output$selectedEurPlot <- renderPlot ({
    index <- input$cluster
    cluster <- table[table$cluster == index]
    hist(cluster$euribor3m, breaks = 20, xlim = c(1,5), ylim = c(0, 3000), main = paste("Distrubtion of Euribor 3 month rate for cluster", index))
  })
  
  output$selectedEurPlotY <- renderPlot ({
    cluster <- table[table$y == "yes"]
    hist(cluster$euribor3m, breaks = 20, xlim = c(1,5), ylim = c(0, 3000), main = paste("Distrubtion of Euribor 3 month rate for postive outcome"))
  })
  
  output$selectedAgePlot <- renderPlot ({
    index <- input$cluster
    cluster <- table[table$cluster == index]
    hist(cluster$age,  xlab = "Age", xlim = c(15,100), main = paste("Distrubtion of Age in Years for cluster", index))
  })
  
  output$selectedAgePlotY <- renderPlot ({
    cluster <- table[table$y == "yes"]
    hist(cluster$age, xlab = "Age", xlim = c(15,100),  main = paste("Distrubtion of Age in years for postive outcome"))
  })
  
  output$selectedJobPlot <- renderPlot ({
    index <- input$cluster
    cluster <- table[table$cluster == index]
    barplot(prop.table(table(cluster$job)), main = paste("Distrubtion of job types for cluster", index))
  })
  
  output$selectedJobPlotY <- renderPlot ({
    cluster <- table[table$y == "yes"]
    barplot(prop.table(table(cluster$job)), main = paste("Distrubtion of job types for postive outcome"))
  })
  
  
  
  output$selectedMaritalPlot <- renderPlot ({
    index <- input$cluster
    cluster <- table[table$cluster == index]
    barplot(prop.table(table(cluster$marital)), main = paste("Distrubtion of marital status for cluster", index))
  })
  
  output$selectedMaritalPlotY <- renderPlot ({
    cluster <- table[table$y == "yes"]
    barplot(prop.table(table(cluster$marital)), main = paste("Distrubtion of marital status for postive outcome"))
  })
  
  
  output$selectedEduationPlot <- renderPlot ({
    index <- input$cluster
    cluster <- table[table$cluster == index]
    barplot(prop.table(table(cluster$education)), main = paste("Distrubtion of eduation for cluster", index))
  })
  
  output$selectedEduationPlotY <- renderPlot ({
    cluster <- table[table$y == "yes"]
    barplot(prop.table(table(cluster$education)), main = paste("Distrubtion of eduation for postive outcome"))
  })
  
  output$clusterBarPlot <- renderPlot ({
    barplot(c(val1, val2, val3), names.arg = c("cluster 1", "cluster 2", "cluster 3"), main = paste("Comparions of the clusters with positive outcome"))
  })
  
})

shinyApp(ui, server)