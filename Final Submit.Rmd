---
title: "Final Project Preliminary EDA"
output: html_notebook
---


## Set Up
```{r}
rm(list = ls()) ## cleans up your R environment

# load packages
library(tidyverse)
library(DataComputing)
library(mosaic)
library(lubridate)
library(leaflet)
library(rvest)
```

## Datasets
```{r}
Poverty <- read.csv("~/Stat 184/Final Project/Poverty.csv")
GDP <- read.csv("~/Stat 184/Final Project/state gdp per capita.csv")
```

### Research Question
Does per capita GDP reflect the poverty rate accurately?

### Data Analysis

```{r}
glimpse(Poverty)
glimpse(GDP)
```
    
    Each case in the Poverty dataset represents a county during a specific year, their total population, and the number of    people in poverty.
  
    Each case in the GDP dataset represents a state and their gdp in a specific year.
  
    The variables that this research question will utilize is the State, 2015, GDP, and povertyest variables.
  the State variable is so that we can observe trends stateside not just nation wide, the GDP is essential to the research    question as it displays the per capita gdp by state so that it can be compared to the povertyest which shows the total
  number of people in poverty in a specific state. Lastly the year variable is so that we have consistancy in our data,
  it would only be fair to compare data that took place in the same year.

```{r}
head(Poverty)
head(GDP)
```

### Let's take a more visual analysis

### Why don't we take a closer look using county poverty rates.
```{r}
yearlyPoverty <-
  Poverty %>%
  spread( key=year, value = (povertyest) )

povertySpread <-
  yearlyPoverty %>%
  rename(poverty2015 = "2015", population2015 = population_inferred) %>%
  mutate(poverty2015 = as.numeric(poverty2015)) %>%
  select(county_name, poverty2015, population2015) %>%
  drop_na(county_name, poverty2015) %>%
  mutate(percentPoverty = (poverty2015/population2015))
head(povertySpread, 100)
```

This graph is just to show that poverty is linearly correlated with population and that we have no outliers.

```{r}
ggplot(data=povertySpread,aes(x=population2015,y=poverty2015))+
  geom_line() +
  geom_smooth() +
  xlab("Population") +
  ylab("# of people in Poverty")
```


### The last one was a little messy, let's step further back and take a broader look at stateside poverty
```{r}
poverty2015 <-
  Poverty %>%
  select(year, state_name, povertyest, population_inferred) %>%
  filter(year == 2015) %>%
  rename(state = state_name) %>%
  group_by(state) %>%
  summarise(poverty = sum(povertyest), population = sum(population_inferred), percent = (poverty/population)*100)
head(poverty2015)
```

### Lets compare this graphically to our previous graph.
```{r}
ggplot(data=poverty2015,aes(x=population,y=poverty))+
  geom_line() +
  geom_smooth() +
  xlab("Population") +
  ylab("# of people in Poverty")
```


### Now lets check out GDP Stats.
```{r}
state2015 <-
  GDP %>%
  select(Location, "X2015") %>%
  rename(state = Location, gdp = "X2015") %>%
  mutate(year = "2015")
head(state2015)
```
```{r}
ggplot(data=state2015,aes(x=state,y=gdp ))+
  geom_bar(stat='identity',position='stack', width=.9)
```
It looks like we have an outlier here, we can deal with it later after we combine our 2 tables to check for a relation between gdp and poverty.

```{r}
combine <-
  left_join(poverty2015, state2015, by = "state")
head(combine)
```
```{r}
ggplot(data=combine,aes(x=percent,y=gdp))+
  aes(colour=state) +
  geom_point() +
  ylab("GDP per capita(person)")+
  xlab("Poverty %")
```
### What is this outlier???

```{r}
combine %>%
  filter(gdp >= 100000)
```

  The outlier is the nations Capital!!! Washington D.C, where there is a poverty rate of 17.7% at the same time as having the highest GDP in the nation by nearly 100,000 gap. Of course there are many reasons for this outlier, government positions are located in DC and their salary overcompensates for those in poverty which skews the calculation which only account for average, this being said, our research question asks if GDP accurately represents poverty, in this case it does not. However it is an extreme outlier so why don't we take care of it.
  
```{r}
combineClean <-
  combine %>%
  filter(gdp <= 100000)
head(combineClean)
```

```{r}
ggplot(data=combineClean,aes(x=percent,y=gdp))+
  geom_point() +
  geom_smooth() +
  ylab("GDP per capita(person)")+
  xlab("Poverty %")
```
There we go, much better. Without the outlier our graph indicates that there is a accurate relationship between gdp and poverty rates, the higher the gdp, the lower the poverty it seems.
































































































