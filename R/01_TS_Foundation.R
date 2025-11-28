library(tidyverse)
library(tsibble)
library(fpp3)

# TSIBBLE OBJECT

## time index "year"
y <- tsibble(
  Year = 2015:2019,
  Observations = c(123,39,78,52,110),
  index = Year
)


## time index and keys
olympic_running

olympic_running |> 
  filter(Length==100) |> 
  na.omit() |> 
  autoplot()


## trying to declare a 4 year time index
z <- tsibble(
  year = seq(1,100,4),
  value = rnorm(25) + (1:25)/25,
  index=year
)



