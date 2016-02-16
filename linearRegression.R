# load plyr before dplyr may cause problem
if("package:dplyr" %in% search() ){
  detach("package:dplyr", unload=TRUE)
}
library(ggplot2)
library(plyr)
library(dplyr)
library(testthat)

## Read data from the output of "summaryPlot and convertDataFormat.R"
gapminder <- read.delim("gapminder.csv", header = T, sep = "\t")

## Fit a linear regression of life expectancy on year within each country.
#Write the estimated intercepts, slopes, and residual error variance (or sd) to file.

#' Hw 5 function varOnYear
#'
#' the function print out a data.frame contains the intercepts, slopes, and residual
#' error variance of a linear regression
#'
#' @param data: data from one singe country
#' @param var: a string, variable name in gapminder, e.g. pop, lifeExp, etc.
#' @param offset: first year point
#' 
#' @return d: a dataframe with the intercept, slope and residual error variance for the 
#' linear regression of one particular country
varOnYear <- function(data, var = "lifeExp", offset = 1952){
  
  test_that("please input data with valid variable",{
    expect_equal(is.null(data[,var]), F)
    expect_equal(is.null(data[,"year"]), F)
  })
  test_that("Please input data from one single country",{
    expect_equal(length(unique(data[,"country"])), 1)
  })
  data$year <- data$year - 1952
  f <- paste0(var, " ~ year" )
  l <- lm(f, data = data)
  d <- data.frame(intercepts = coefficients(l)[1], 
                          slope = coefficients(l)[2],
                          residual.error.variance = sd(residuals(l)))
  return(d)
}

# Use ddply to get the linear regression result for each country
cat("Use ddply to get the linear regression result for each country \n")
lifeExpOnYear <- plyr::ddply(gapminder, ~country, function(x) varOnYear(x))
lifeExpOnYear
# print the output with the name "lifeExpOnYear.csv"
cat("save the output into file with the name \"lifeExpOnYear.csv\" \n")
write.csv(lifeExpOnYear, "lifeExpOnYear.csv", quote = FALSE, row.names = FALSE)

# print the "United States", "China", "Japan", lifeExp plot with 
#linear result in (change geom_smooth to stat_smooth)
cat("print the \"United States\", \"China\", \"Japan\" lifeExp plot with 
  linear result in (change geom_smooth to stat_smooth) \n")
linearPlot <- function(data, x = "year", y = "lifeExp"){
  myPlot <- ggplot(data, aes_string(x = x, y = y, fill = "country")) +
    geom_point(aes_string(color = "country")) +
    stat_smooth(method = "lm", aes(color = country))
        
  print(myPlot)
}
A <- gapminder %>% subset(country %in% c("China", "United States", "Japan"))
linearFrame <- lifeExpOnYear %>% subset(country %in% c("China", "United States","Japan"))
linearPlot(A, "year", "lifeExp")
# Save it to file "Top3EconomylifeExp"
ggsave(filename = "Top3EconomylifeExp.png", plot = last_plot(), path = ".",  width = 6, height = 6, dpi = 400)
cat("The plot has been saved to Top3EconomylifeExp.png \n")