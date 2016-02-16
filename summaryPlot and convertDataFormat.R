library(ggplot2)
library(plyr)
library(tidyr)
library(dplyr)

## Read data
gapminder <- read.delim("gapminder.tsv")


## print a summary of the data
cat("A summary of the data \n")
str(gapminder)
summary(gapminder)


## Save a couple descriptive plots to file with highly informative names.
cat("\n")
cat("\n")
cat("Save a couple descriptive plots to file with highly informative names. \n")

## Life Expectancy/ Population/ GDP per Capita over time and continent
#' Hw 5 function overTimeTable
#'
#' the function print out a table of the variable's mean value over continent and year
#'
#' @param data: gapminder or a subset of gapminder, should be a data.frame
#' @param variable: a string, variable name in gapminder, e.g. pop, lifeExp, etc. 
#' @return 
overTimeTable <- function(data, variable){
  cat("Plot the table for mean value of", variable, "over time and continent \n")
  variableOverTime <- data %>% 
    ddply(.variables = c("continent", "year"), 
                           function(x){dplyr::summarise(x, mean = mean(x[,variable]))})
  table <- variableOverTime %>% spread(continent, mean)
  print(table)
  tablePlot <- ggplot(variableOverTime, aes(x = year, y = mean)) +
    geom_point(aes(color = continent)) +
    geom_smooth(aes(color = continent))
  return(tablePlot)
}
## Life Expectancy over time and continent
lifeExpPlot <- overTimeTable(gapminder, "lifeExp")
print(lifeExpPlot)
## Population over time and continent
popPlot <- overTimeTable(gapminder, "pop")
print(popPlot)
## GPD per Capita over time and continent
gdpPercapPlot <- overTimeTable(gapminder, "gdpPercap")
print(gdpPercapPlot)

cat("Output those plot to current folder, the plots will be named as 
lifeExpPlot.png, 
    popPlot.png, 
    gdpPercapPlot.png \n")
ggsave(filename = "lifeExpPlot.png", plot = lifeExpPlot, path = ".",  
       width = 6, height = 6, dpi = 400)
ggsave(filename = "popPlot.png", plot = popPlot, path = ".",  
       width = 6, height = 6, dpi = 400)
ggsave(filename = "gdpPercapPlot.png", plot = gdpPercapPlot, path = ".",  
       width = 6, height = 6, dpi = 400)


## Life Expectancy/ Population/ GDP per Capita over time and country
#' Hw 5 function overTimeTable
#'
#' the function print out a plot of the variable's value over country and year with reordering 
#' the country
#'
#' @param data: gapminder or a subset of gapminder, should be a data.frame
#' @param variable: a string, variable name in gapminder, e.g. pop, lifeExp, etc. 
variableOverCountry <- function(data, variable){ 
  cat("Plot the graph for", variable, "for each country over time \n")  
  data1 <- data
  data1$v <- data[,variable]
  data1 <- data1 %>% mutate(country = reorder(x = country,X = v, min))
  myPlot <- ggplot(data1, aes(x = year, y = v, fill = country)) + 
    geom_point(aes(color = country)) +
    geom_smooth(aes(color = country)) +
    facet_wrap(~continent)
  rm(data1)
  return(myPlot)  
}

cat("Life Expectancy/ Population/ GDP per Capita over time and country \n")
# Apply the function on Life Expectancy/ Population/ GDP per Capita
lifeExpOverCountry <- variableOverCountry(gapminder, "lifeExp")
print(lifeExpOverCountry)
popOverCountry <- variableOverCountry(gapminder, "pop")
print(popOverCountry)
gdpPercapOverCountry <- variableOverCountry(gapminder, "gdpPercap")
print(gdpPercapOverCountry)

cat("Output those plot to current folder, the plots will be named as 
lifeExpOverCountry.png, 
    popOverCountry.png, 
    gdpPercapOverCountry.png \n" )
ggsave(filename = "lifeExpOverCountry.png", plot = lifeExpOverCountry, path = ".",  
       width = 6, height = 6, dpi = 400)
ggsave(filename = "popOverCountry.png", plot = popOverCountry, path = ".",  
       width = 6, height = 6, dpi = 400)
ggsave(filename = "gdpPercapOverCountry.png", plot = gdpPercapOverCountry, path = ".",  
       width = 6, height = 6, dpi = 400)


## Reorder the continents based on life expectancy mean
cat("\n")
cat("\n")
cat("Reorder the continents based on life expectancy mean. \n")

gapminder.ordered <- gapminder %>%  
  mutate(continent.ordered = reorder(x = continent, X = lifeExp, min))

## compare the data before and after the reordering
#' Hw 5 function orderPlot
#'
#' the function print a plot of y ~ x, with the color "fill"
#'
#' @param data: gapminder or a subset of gapminder, should be a data.frame
#' @param x: x vertex
#' @param y: y vertex
#' @param fill: variable to distinguish the colors
#' 
#' 
orderPlot <- function(data, x, y, fill){
  myPlot <- ggplot(data, aes_string(x = x, y = y, fill = fill)) +
    geom_point(aes_string(color = fill)) +
    geom_smooth(aes_string(color = fill))
  print(myPlot)
}

## Plot before reordering
cat("Plot before reordering \n")
orderPlot(gapminder, "year", "lifeExp", "continent")
## Plot after reordering
cat("Plot after reordering \n")
orderPlot(gapminder.ordered, x = "year", y = "lifeExp", fill = "continent.ordered")


##Sort the actual data in a deliberate fashion. You decide the details,
## but this should at least implement your new continent ordering.
cat("Sort the data with the mulitiple value of GDP per Capita and Population. \n")
gapminder.sorted <- gapminder %>%
  group_by(year) %>%
  arrange(., desc(gdpPercap * pop))
gapminder.sorted
head(gapminder)


## Save the data in gapminder.csv
cat("\n")
cat("Save the data in gapminder.csv \n")
write.table(gapminder, "gapminder.csv", quote = FALSE, row.names = FALSE, sep = "\t")
print("Save the file to gapminder.csv completely")
  

