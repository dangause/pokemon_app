library(data.table)
library(ggplot2)
library(stringr)
library(tidyverse)
library(rvest)
library(readxl)
library(httr)
library(shiny)
library(plotly)
library(leaflet)
library(geojsonio)

pokemon.data <- read.csv("Pokemon.csv")


### Test 2

# Question 1
# We must give a brief description of a pokemon
# User must type in name of pokemon, and the app will return
# first wikipedia paragraph on that pokemon.

"https://en.wikipedia.org/wiki/Charizard" %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/p[1]') %>%
  html_text

test <- paste("https://en.wikipedia.org/wiki/","Charizard", sep = "")
test %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/p[1]') %>%
  html_text
  

paste("https://en.wikipedia.org/wiki/","Charizard", "Charizard", "_Pokémon", sep = "")




### Question 2
# First, add ranking columns
pokemon.data.test$HPRank <- NULL
order.scores<-order(pokemon.data.test$HP, pokemon.data.test$Name)
pokemon.data.test$HPRank[order.scores] <- 1:nrow(pokemon.data.test)

pokemon.data.test$AttackRank <- NULL
order.scores <- order(pokemon.data.test$Attack, pokemon.data.test$Name)
pokemon.data.test$AttackRank[order.scores] <- 1:nrow(pokemon.data.test)

pokemon.data.test$DefenseRank <- NULL
order.scores<-order(pokemon.data.test$Defense, pokemon.data.test$Name)
pokemon.data.test$DefenseRank[order.scores] <- 1:nrow(pokemon.data.test)

pokemon.data.test$Sp..AtkRank <- NULL
order.scores<-order(pokemon.data.test$Sp..Atk, pokemon.data.test$Name)
pokemon.data.test$Sp..AtkRank[order.scores] <- 1:nrow(pokemon.data.test)

pokemon.data.test$Sp..DefRank <- NULL
order.scores<-order(pokemon.data.test$Sp..Def, pokemon.data.test$Name)
pokemon.data.test$Sp..DefRank[order.scores] <- 1:nrow(pokemon.data.test)

pokemon.data.test$SpeedRank <- NULL
order.scores<-order(pokemon.data.test$Speed, pokemon.data.test$Name)
pokemon.data.test$SpeedRank[order.scores] <- 1:nrow(pokemon.data.test)

pokemon.stats <- pokemon.data.test %>%
  filter(Name == "Pikachu")
best.rank <- which.max(pokemon.stats[,14:19])
best.rank <- names(best.rank)

skill.frame <- data.frame(Skill = c("HPRank", "AttackRank", "DefenceRank", "Sp..AtkRank",
                           "Sp..DefRank", "SpeedRank"),
                 Rank = c(pokemon.stats$HPRank, pokemon.stats$AttackRank, pokemon.stats$DefenseRank,
                          pokemon.stats$Sp..AtkRank, pokemon.stats$Sp..DefRank, pokemon.stats$SpeedRank))

skill.frame %>%
  ggplot(aes(x = Skill,
             y = Rank,
             fill = factor(ifelse(Skill == best.rank,
                                  "Highlighted",
                                  "Normal")))) +
  geom_bar(stat = "identity") +
  theme(legend.position="none")
  





#### Question 3

install.packages("rgdal")
library(rgdal)
library(geojsonio)
library(geojson)


states.url <- "http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_5m.json"
states <- geojson_read(states.url, what = "sp")

# Need to assign each pokemon to a random state. Generate a random number 1:50 for each pokemon,
# mutate that number to a new column. 

pokemon.data.test <- 

random.numbers <- sample(1:52, 800, replace = TRUE)
pokemon.data.test$state.number <- random.numbers

pokemon.in.state <- NULL
for (i in 1:52) {
  pokemon.in.state[i] <- pokemon.data.test %>%
    filter(state.number == i) %>%
    nrow()
}



states.pokemon <- states


states.list <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
                 "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho,", "Illinois", "Indiana", "Iowa", 
                 "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", 
                 "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
                 "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", 
                 "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico", "Rhode Island",
                 "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
                 "Washington", "West Virginia", "Wisconsin", "Wyoming")

## Adding column to pokemon.data.test for corresponding state
state.number <- c(1:52)
new.states.list <- data.frame(states.list,
           state.number)
pokemon.data.test <- left_join(pokemon.data.test, new.states.list, by = "state.number")


# Now include pokemon density in state table, to be used for color function
state.density.table <- data.frame(new.states.list, pokemon.in.state)

## Making function to color states by density in leaflet
states.pokemon@data <- states@data %>%
  mutate(thecolor = state.density.table$pokemon.in.state)

factpal <- colorFactor("Greens", states.pokemon@data$thecolor)

# No lets actually plot!

leaflet(states.pokemon) %>%
  setView(-96, 37.8, 4) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~factpal(thecolor),
              weight = 2,
              opacity = 1,
              dashArray = "1",
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = FALSE))


test <- NULL
test <- pokemon.data.test %>%
  filter(states.list == "California")
c(test[,2])


expr[filter(pokemon.data.test$states.list  == 'California')]

pokemon.data.test$states.list










states.list <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
                 "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho,", "Illinois", "Indiana", "Iowa", 
                 "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", 
                 "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
                 "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", 
                 "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico", "Rhode Island",
                 "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
                 "Washington", "West Virginia", "Wisconsin", "Wyoming")


sample(states.list, 800, replace = TRUE)

pokemon.data.test$state <- sample(states.list, 800, replace = TRUE)
new.states.list <- data.frame(c(1:52), states.list)

pokemon.data.test <- left_join(pokemon.data.test, new.states.list, by = c("state" = "states.list"))
pokemon.data.test$state.number <- pokemon.data.test$c.1.52.
pokemon.data.test$c.1.52. <- NULL

pokemon.data.test

capture.output({message("hi")})




## Now I have to fix the input button for the first page, and highlight the state
# that the pokemon lives in


//*[@id="tab-basic-1"]/div[1]/div[1]/p[1]/a/img





