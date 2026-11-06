library(dplyr)
library(ggplot2)
data <- read.csv("mentalhealth.csv")
summary(data)

head(data)
summary(data)
colSums(is.na(data))
data<- data%>%
  distinct()
ggplot(data,aes (x=variable)) +
  geom_histogram() 

