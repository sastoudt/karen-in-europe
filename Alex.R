#Find the correlation with US Karen data and the other three countries (assume only female Karens).
#Need to make a fair comparison because different years of data, so trim to the years they have in common.


library(devtools)
library(dplyr)

## Correlation with Karen, females

## name_data: subset of names dataset for one name and one country (assumes only females)
## karen_data: subset of names dataset of karen from one country (assumes only females) 
helperCor <- function(full-names-data-nice, karen_data) {
  cor(name_data$prop, karen_data$prop, method = "kendall", use = "complete.obs")
}