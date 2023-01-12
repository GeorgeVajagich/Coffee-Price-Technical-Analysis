---
  title: "Coffee Price Technical Analysis"
author: "George Vajagich"
date: "11/30/2022"
output: html_document
---

knitr::opts_chunk$set(echo = TRUE)
library(readr)
options(readr.show_col_types = FALSE)
library(ggplot2)
library(lubridate)
library(scales)
library(dplyr)





df=read_csv("Coffee_Price_Since_1973.csv")
df$date<- mdy(df$date)




ggplot(data = df, aes(x = date, y = value, group = 1))+
  geom_line()+
  scale_x_date(breaks="5 years", labels = date_format("%Y"))



head(df)



df <- df %>% # now shift it
  mutate(one_day_past = lag(value,n=1))




df %>% mutate_at(c("value"), tibble::lst("one_day_future"=lead), n = 1)




df  %>% mutate(P1Dperformance=(value/one_day_past-1)*100)

