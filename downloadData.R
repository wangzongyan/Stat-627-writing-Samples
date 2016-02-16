require("downloader")
url <- "http://raw.githubusercontent.com/jennybc/gapminder/master/inst/gapminder.tsv"
downloader::download(url = url, destfile = "gapminder.tsv")
cat("gapminder.tsv has been downloaded from 'https://raw.githubusercontent.com/jennybc/
                     gapminder/master/inst/gapminder.tsv'")
