## Correlation with Karen, females

## name_data: subset of names dataset for one name and one country (assumes only females)
## karen_data: subset of names dataset of karen from one country (assumes only females) 
helperCor <- function(name_data, karen_data) {
  cor(name_data$prop, karen_data$prop, method = "kendall", use = "complete.obs")
}

## Past and Future Karens, females

## old Karen, push other name forward (past Karen)
## name_data: subset of names dataset for one name and one country (assumes only females)
## lagN: number of years to lag
## karen_data: subset of names dataset of karen from one country (assumes only females) 
helperCorLagOther <- function(name_data, lagN, karen_data) {
  tryCatch(cor(lag(name_data$prop, lagN), karen_data$prop, method = "kendall", use = "complete.obs"), error = function(i) {
    return(NA)
  })
}

## new Karen, push Karen forward (future Karen)
## name_data: subset of names dataset for one name and one country (assumes only females)
## lagN: number of years to lag
## karen_data: subset of names dataset of karen from one country (assumes only females) 
helperCorLagKaren <- function(name_data, lagN, karen_data) {
  tryCatch(cor(name_data$prop, lag(karen_data$prop, lagN), method = "kendall", use = "complete.obs"), error = function(i) {
    return(NA)
  })
}