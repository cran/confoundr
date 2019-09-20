## ----setup, include = FALSE----------------------------------------------
library(confoundr)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
data("example_sml")

## ------------------------------------------------------------------------
drops <- c("h_0", "h_1", "h_2")
mydata <- example_sml[ , !(names(example_sml) %in% drops)]

mydata.history <- makehistory.one(input=mydata,
                                  id="id",
                                  exposure="a",
                                  name.history="h",
                                  times=c(0,1,2))

## ------------------------------------------------------------------------
mydata.tidy <- lengthen(
  input=example_sml, #mydata.history,
  id="id",
  diagnostic=3,
  censoring="no",
  times.exposure=c(0,1,2),
  times.covariate=c(0,1,2),
  exposure="a",
  temporal.covariate=c("l","m","o"),
  static.covariate=c("n","p"),
  history="h",
  weight.exposure="wax"
)

## ------------------------------------------------------------------------
mydata.tidy.omit <- omit.history(
  input=mydata.tidy,
  omission="relative",
  covariate.name=c("l","m","o"),
  distance=1
  )

## ------------------------------------------------------------------------
mytable <- balance (
input=mydata.tidy.omit,
diagnostic=3,
approach="weight", 
censoring="no",
scope="all",
times.exposure=c(0,1,2),
times.covariate=c(0,1,2),
exposure="a",
history="h",
weight.exposure="wax",
ignore.missing.metric="no",
sort.order= c("l","m","o","n","p")
)

## ------------------------------------------------------------------------
myplot <- makeplot (
input=mytable,	
diagnostic	=3,
approach="weight",
scope="all",
metric="SMD"
)

## ------------------------------------------------------------------------
myplot

## ------------------------------------------------------------------------
mydata.tidy <- lengthen(
  input=mydata.history,
  diagnostic=1,
  censoring="no",
  id="id",
  times.exposure=c(0,1,2),
  times.covariate=c(0,1,2),
  exposure="a",
  temporal.covariate=c("l","m","n","o","p"),
  history="h"
)  

head(mydata.tidy)


## ------------------------------------------------------------------------

library(dplyr)
library(broom)

mydata.tidy.reg <- mutate(mydata.tidy,
                          time=time.exposure,
                          distance=time.exposure-time.covariate,
                          history=h)

output <- mydata.tidy.reg %>% 
  group_by(name.cov) %>% #note, you can include other stratifying variables here or in the model
    filter(time.exposure>=time.covariate) %>% #lengthen actually arealdy took care of this, provided here for clarity
      do(tidy(lm(formula=value.cov~a+time+distance+history,.))) %>% #same model form used for every covariate
        filter(term=="a") %>% ungroup()

table.reg <- output %>% 
               select(name.cov,estimate) %>% 
                 rename(D=estimate)

print(table.reg)

## ------------------------------------------------------------------------

table.std <- balance(input=mydata.tidy,
  diagnostic=1,
  approach="none",
  censoring="no",
  scope="average",
  average.over="distance",
  ignore.missing.metric="no",
  times.exposure=c(0,1,2),
  times.covariate=c(0,1,2),
  exposure="a",
  history="h"
) 

print(table.std)

