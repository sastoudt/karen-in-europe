#Find the correlation with US Karen data and the other three countries (assume only female Karens).
#Need to make a fair comparison because different years of data, so trim to the years they have in common.


library(devtools)
library(dplyr)

## Correlation with Karen, females

## name_data: subset of names dataset for one name and one country (assumes only females)
## karen_data: subset of names dataset of karen from one country (assumes only females) 
helperCor <- function(name_data, karen_data) {
  cor(name_data$prop, karen_data$prop, method = "kendall", use = "complete.obs")
}

name_data<-full_names_data_nice %>%
  filter(country== "United States") %>%
  filter(name== "Karen")%>%
  filter(sex== "F") %>%
  filter(year >= 1997) %>% 
  filter( year <= 2014)
  

SC<- full_names_data_nice %>%
  filter(country== "Scotland") %>%
  filter(name== "Karen")%>%
  filter(sex== "Girl") %>%
  filter(year >= 1997) %>% 
  filter( year <= 2014)

NI<-full_names_data_nice %>%
  filter(country== "Nothern Ireland")%>%
  filter(name== "Karen")%>%
  filter(sex== "Girl") %>%
  filter(year >= 1997) %>% 
  filter( year <= 2014)

E<-full_names_data_nice %>%
  filter(country== "England/Wales")%>%
  filter(name== "Karen")%>%
  filter(sex== "Girl") %>%
  filter(year >= 1997) %>% 
  filter( year <= 2014)

helperCor(name_data, E)

