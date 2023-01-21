knitr::opts_chunk$set(echo = TRUE)
library(readr)
options(readr.show_col_types = FALSE)
library(ggplot2)
library(lubridate)
library(scales)
library(dplyr)
library(grid)



df=read_csv("Coffee_Price_Since_1973.csv")
df$date<- mdy(df$date)





ggplot(data = df, aes(x = date, y = value, group = 1))+
  geom_line()+
  scale_x_date(breaks="5 years", labels = date_format("%Y"))


head(df)



PVF <- function (days){
  
  df <- df %>% mutate(past = lag(value,n=days))
  df <- df %>% mutate_at(c("value"), tibble::lst("future"=lead), n = days)
  
  df <- df %>% mutate(next_performance=(future/value-1)*100)
  df  <- df %>% mutate(past_performance=(value/past-1)*100)
  
  grob = grobTree(textGrob(paste("Correlation : ", round(cor(df$past_performance, df$next_performance, use = "complete.obs"),4)), x = 0.75, y = 0.95, gp = gpar(col = "red", fontsize = 12)))
  
  print(cor(df$past_performance, df$next_performance, use = "complete.obs")) 
  
  ggplot(df, aes(x=past_performance, y=next_performance)) + geom_point(size=0.5) + annotation_custom(grob) + theme(aspect.ratio=1)
  
}

PVF(1)
PVF(5)
PVF(20)
PVF(240)
PVF(480)
PVF(1200)


