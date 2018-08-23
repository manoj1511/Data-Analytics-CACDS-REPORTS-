library(readxl)
library(ggplot2)
library(dplyr)
library(rowr)

filenames = list.files(pattern="*.xlsx")

readdata <- function(filename) {
  df <- read_excel(filename)
  vec <- select(df, ends_with("Course total (Letter)"), ends_with("Attendance (Real)"))
  vec <- filter(vec, vec[2]> 30)
  vec <- select(vec,ends_with("Course total (Letter)") )
  return(vec)
}



result <-lapply(filenames, readdata)

result[["fill"]] <- NA

result <- do.call("cbind.fill", result)

col <- ncol(result) #no. of columns
for (i in 1:col)
{
  result[,i] <- factor(result[,i])
}

out <- matrix(NA, nrow = col,ncol = 2)

for(j in 1:col)
{
  tmp <- data.frame(table(result[,j]))
  out[j,] <- as.matrix(tmp$Freq)
}

out <- data.frame(out)

colnames(out) <- c("Fail","Pass")
out <- mutate(out, Total = Fail+Pass) 
out <- mutate(out, Subject = filenames)
out <- gather(out, key, value, -Total, -Subject)


#ploting

fill <- c("Red", "darkGreen") 
p <-  ggplot(out, aes(x = Subject, y = value)) +
  geom_col(aes(fill = key)) +
  #adding values for Absent
  geom_text(data = out[1:nrow(out)/2,], aes(x = Subject, y =((Total-(Total-value))/2)+Total-value, label = paste0(value)), size=4)+
  #adding values for Present
  geom_text(data = out[(nrow(out)/2)+1:nrow(out),], aes(x = Subject, y =(value/2), label = paste0(value)), size=4)+
  #adding colour
  geom_text(data = out[(nrow(out)/2)+1:nrow(out),], aes(x = Subject, y = Total+3, label = paste0("Total = ",Total)), size=4)+
  scale_fill_manual(values=fill) +
  #giving title
  ggtitle(label = "Grade Report")+
  ylab("Number of Students")+
  theme(axis.text.x=element_text(angle=45,hjust=1))

plot(p)
