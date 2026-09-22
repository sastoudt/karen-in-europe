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


## Inputs
## other_country_karen: dataset of female Karen data for one European country,
##                      must have lowercase column names
## karen_us: dataset of female Karen data for US,
##                      must have lowercase column names
## Outputs
## data frame with both US and other country's Karen data in right shape
## You must now make two different subsets of this to feed into your
## original helper function.
## After you make your subsets, make sure they are in the right year order.
## Use arrange if need be.
help_line_up <- function(other_country_karen, karen_us) {
  name_of_us <- karen_us$country[1]
  name_of_other_country <- other_country_karen$country[1]
  try_this <- full_join(other_country_karen, karen_us, by = c("year"))

  karen_nice <- try_this %>%
    select(year, name.y, prop.y, country.y)

  names(karen_nice)[2:4] <- c("name", "prop", "country")
  karen_nice$country[which(is.na(karen_nice$country))] <- name_of_us

  other_nice <- try_this %>%
    select(year, name.x, prop.x, country.x)

  names(other_nice)[2:4] <- c("name", "prop", "country")
  other_nice$country[which(is.na(other_nice$country))] <- name_of_other_country

  toReturn <- rbind(karen_nice, other_nice)
  toReturn$name <- rep("Karen", nrow(toReturn))
  return(toReturn)
}
