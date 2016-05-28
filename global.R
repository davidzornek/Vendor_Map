library(shiny)
library(shinydashboard)
library(tidyr)
library(magrittr)
library(plyr)
library(dplyr)
library(DT)
library(rCharts)
library(maps)
library(leaflet)


palette <- c("#8e44ad", "#c0392b", "#27ae60", "#2980b9")
vendors <- c("Massive Dynamic", "Wolfram & Hart","The Dyad Institute","Lex Corp")

# data import

fee <- readRDS("data/fee.rds")
cost <- readRDS("data/paid.rds")
total <- readRDS("data/total.rds")
exposure <- readRDS("data/exposuredata.rds")

# The maps package encodes non-continuous regions, e.g. michigan or anything with islands, separately.
# The data uses just state names, though, and state names are what we want to display.
# The following object is used for handling these regions.

extra.regions <- c("massachusetts:martha's vineyard","massachusetts:main","massachusetts:nantucket",
                   "michigan:north","michigan:south","new york:manhattan","new york:main","new york:staten island",
                   "new york:long island","north carolina:knotts","north carolina:main","north carolina:spit",
                   "virginia:chesapeake","virginia:chincoteague","virginia:main","washington:san juan island",
                   "washington:lopez island","washington:orcas island","washington:whidbey island","washington:main")

#### Define Functions ####

# getStateName parses extracts state names from map() regions

getStateName <- function(id) {
  strsplit(id, ":")[[1]][1]
}

#simpleCap capitalizes the first letter of every word, given a text string input.
#map() regions are all lower case. This function formats state names for use in the mouseover info box

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
