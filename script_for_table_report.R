#Load Libraries
library(readxl)
library(dplyr)

users <- read_excel("users/Users.xlsx",col_names = TRUE)
colnames(users)[2] <- "Username"

dataset <- read_excel("Summer 18 Visualization_Attendances_20180723-2226.xlsx",col_names = TRUE, skip =3)

dataset <- merge(users,dataset,by = "Username")

#select the necessary columns
dataset <- select(dataset,Username, ends_with("All students"), P,profile_field_Department,profile_field_Position,profile_field_Organization )
dataset <- filter(dataset, P >= 1)

dataset$profile_field_Department <- as.factor(dataset$profile_field_Department)
dataset$profile_field_Position <- as.factor(dataset$profile_field_Position)
dataset$profile_field_Organization <- as.factor(dataset$profile_field_Organization)

summary(dataset)