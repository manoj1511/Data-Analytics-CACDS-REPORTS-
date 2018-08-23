#Load Libraries
library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gtools)
#library(here)
#setwd(here("2018 Spring/Attendance/"))

filename = list.files(pattern="*.xlsx")
for (name in 1:length(filename))
  {

  
  #read the dataset
  dataset <- assign(filename[name], read_excel(filename[name],col_names = TRUE, skip =3))

  #select the necessary columns
  dataset <- select(dataset, ends_with("All students"), P )
  dataset <- filter(dataset, P >= 2)
  dataset <- select(dataset,ends_with("All students") )
  dataset <- data.frame(dataset)

  #convert char columns to factors
  col <- ncol(dataset) #no. of columns
  for (i in 1:col)
    {
    dataset[,i] <- factor(dataset[,i], levels = c("A (0/2)","P (2/2)"))
    }

  #create a new data frame with present and absent
  out <- matrix(NA, nrow = col,ncol = 2)
  names <- matrix(NA, nrow = col ,ncol = 1)
  for(j in 1:col)
    {
    tmp <- data.frame(table(dataset[,j]))
    out[j,] <- as.matrix(tmp$Freq)
    names[j,] <- as.matrix(paste("Session ", j))
    }
  colnames(out) <- c("Absent","Present")
  out <- data.frame(out)

  #adding new column for total students in each class
  out <- mutate(out, Total = Absent + Present) 
  out <- mutate(out, Sessions = names)
  out$Sessions <- factor(out$Sessions, levels = mixedsort(out$Sessions))

  #gathering values
  out <-  gather(out, key, value, -Total, -Sessions)
  #plotting stacked bar pot 
  fill <- c("Red", "darkGreen") 
  p <-  ggplot(out, aes(x = Sessions, y = value)) +
 
    geom_col(aes(fill = key)) +
    #adding values for Absent
    geom_text(data = out[1:nrow(out)/2,], aes(x = Sessions, y =((Total-(Total-value))/2)+Total-value, label = paste0(value)), size=4)+
    #adding values for Present
    geom_text(data = out[(nrow(out)/2)+1:nrow(out),], aes(x = Sessions, y =(value/2), label = paste0(value)), size=4)+
    #adding colour
    scale_fill_manual(values=fill) +
    #giving title
    ggtitle(label = filename[name], subtitle = paste0("Total number of Students: ",nrow(dataset)))+
    ylab("Number of Students")
  
  plot(p)
  
  }