## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(dplyr)
library(confoundr)

## ------------------------------------------------------------------------
data("catie_sim")

## ------------------------------------------------------------------------
catie_sim %>% filter(time==0) %>% select(starts_with("age.grp."),white,black,other,exacer,unemployed,starts_with("site")) %>% summarize_all("mean")

## ------------------------------------------------------------------------
catie_sim %>% group_by(time) %>% select(Chg.pansstotal,cs14,cs16,calg1,epsmean,qoltot,Chg.pansstotal,pct.gain,phase.change.cum,studydisc) %>% summarize_all("mean")

## ------------------------------------------------------------------------
print(catie_sim %>% select(CATIEID,time,treat.grp,studydisc,pct.gain) %>% filter(CATIEID==1440))

## ------------------------------------------------------------------------
catie_sim.w <- widen(
 input=catie_sim,
 id="CATIEID",
 time="time",
 exposure="studydisc",
 covariate=c("treat.grp","phase.change.cum",
             "Chg.pansstotal","cs14",
             "cs16","pct.gain",
             "epsmean","qoltot",
             "exacer","Bpansstotal",
             "unemployed"),
 weight.exposure="wtr"             
)
glimpse(catie_sim.w %>% select(CATIEID,starts_with("treat.grp"),starts_with("studydisc"),starts_with("pct.gain")) %>% filter(CATIEID==1440))

## ------------------------------------------------------------------------
catie_sim.wh <- makehistory.two(
 input=catie_sim.w,
 id="CATIEID",
 exposure.a="treat.grp",
 exposure.b="studydisc",
 times=c(0,1,3,6,9,12,15,18),
 name.history.a="ht",
 name.history.b="hs"
)
glimpse(catie_sim.wh %>% select(CATIEID,starts_with("pct.gain"),starts_with("treat.grp"),starts_with("studydisc"),starts_with("ht"),starts_with("hs")) %>% filter(CATIEID==1440))

## ------------------------------------------------------------------------
catie_sim.whl1 <- lengthen(
  input=catie_sim.wh,
  id="CATIEID",
  diagnostic=1,
  censoring="no",
  exposure="studydisc",
  temporal.covariate=c("phase.change.cum","Chg.pansstotal",
                       "cs14","cs16",
                       "pct.gain","epsmean",
                       "qoltot"),
  times.exposure=c(0,1,3,6,9,12,15,18), 
  times.covariate=c(0,1,3,6,9,12,15,18),
  history="hs"
)
glimpse(catie_sim.whl1 %>% filter(CATIEID==1440 & name.cov=="pct.gain"))

## ------------------------------------------------------------------------
catie_sim.whlo1 <- omit.history(
  input=catie_sim.whl1,
  covariate.name=c("phase.change.cum","Chg.pansstotal",
                   "cs14","cs16","pct.gain",
                   "epsmean","qoltot"),
  omission="relative",
  distance="1"
)
glimpse(catie_sim.whlo1 %>% filter(CATIEID==1440 & name.cov=="pct.gain"))

## ------------------------------------------------------------------------
baltbl1 <- balance(
  input=catie_sim.whlo1,
  diagnostic=1,
  approach="none",
  censoring="no",
  scope="recent",
  recency=0,
  exposure="studydisc",
  history="hs",
  times.exposure=c(0,1,3,6,9,12,15,18),
  times.covariate=c(0,1,3,6,9,12,15,18),
  sd.ref="yes"
)
print(baltbl1 %>% filter(name.cov=="pct.gain" & time.exposure<=6) %>% mutate_at(c("D","SMD"),round,3))

## ---- fig.width=7, fig.height=5------------------------------------------
balplot1 <-makeplot(
  input=baltbl1,
  diagnostic=1,
  approach="none",
  censoring="no",
  scope="recent",
  label.exposure="Study Dropout",
  label.covariate="Covariate"
)
balplot1

## ------------------------------------------------------------------------
catie_sim.whl3 <- lengthen(
  input=catie_sim.wh,
  id="CATIEID",
  diagnostic=3,
  censoring="no",
  exposure="studydisc",
  temporal.covariate=c("phase.change.cum","Chg.pansstotal",
                       "cs14","cs16",
                       "pct.gain","epsmean",
                       "qoltot"),
  static.covariate=c("exacer","Bpansstotal",
                     "unemployed"),
  times.exposure=c(0,1,3,6), 
  times.covariate=c(0,1,3,6),
  history="hs",
  weight.exposure="wtr"
)
glimpse(catie_sim.whl3 %>% filter(CATIEID==1440 & name.cov=="pct.gain"))

## ------------------------------------------------------------------------
catie_sim.whlo3 <- omit.history(
  input=catie_sim.whl3,
  covariate.name=c("phase.change.cum","Chg.pansstotal","cs14","cs16","pct.gain","epsmean","qoltot"),
  omission="relative",
  distance=1
)
glimpse(catie_sim.whlo3 %>% filter(CATIEID==1440 & name.cov=="pct.gain"))

## ------------------------------------------------------------------------
baltbl3 <- balance(
  input=catie_sim.whlo3,
  diagnostic=3,
  approach="weight",
  censoring="no",
  scope="all",
  exposure="studydisc",
  history="hs",
  times.exposure=c(0,1,3,6),
  times.covariate=c(0,1,3,6),
  sd.ref="yes",
  weight.exposure="wtr"
)
print(baltbl3 %>% filter(name.cov=="pct.gain" & time.exposure<=6) %>% mutate_at(c("D","SMD","N","Nexp"),round,3))

## ---- fig.width=7, fig.height=5------------------------------------------
balplot3 <- makeplot(
  input=baltbl3,
  diagnostic=3,
  approach="weight",
  censoring="no",
  scope="all",
  label.exposure="Study Dropout",
  label.covariate="Covariate"
)
balplot3

## ------------------------------------------------------------------------
catie.alt <- catie_sim %>%
  group_by(CATIEID) %>%
  arrange(CATIEID,time) %>% 
  mutate(wtr.lag=ifelse(.data$time==0,1,lag(.data$wtr)),
         studydisc.lag=ifelse(.data$time==0,0,lag(.data$studydisc)))
glimpse(catie.alt %>% select(CATIEID,time,studydisc,studydisc.lag,wtr,wtr.lag,pct.gain) %>% filter(CATIEID==1440))  

## ------------------------------------------------------------------------
catie.alt.w <- widen(
 input=catie.alt,
 id="CATIEID",
 time="time",
 exposure="treat.grp",
 covariate=c("phase.change.cum","Chg.pansstotal",
             "cs14","cs16",
             "pct.gain","epsmean",
             "qoltot",
             "exacer","Bpansstotal","unemployed"),
 censor="studydisc.lag",
 weight.censor="wtr.lag"             
)
glimpse(catie.alt.w %>% select(CATIEID,starts_with("treat.grp"),starts_with("studydisc"),starts_with("pct.gain")) %>% filter(CATIEID==1440))

catie.alt.wl2 <- lengthen(
  input=catie.alt.w,
  id="CATIEID",
  diagnostic=2,
  censoring="yes",
  exposure="treat.grp",
  temporal.covariate=c("phase.change.cum","Chg.pansstotal",
                       "cs14","cs16",
                       "pct.gain","epsmean",
                       "qoltot"),
  times.exposure=0, 
  times.covariate=c(0,1,3,6,9,12,15,18),
  censor="studydisc.lag",
  weight.censor="wtr.lag"
)
glimpse(catie.alt.wl2 %>% filter(CATIEID==1440 & name.cov=="pct.gain"))

## ------------------------------------------------------------------------
baltbl2 <- balance(
  input=catie.alt.wl2,
  diagnostic=2,
  approach="none",
  censoring="yes",
  scope="average",
  average.over="distance",
  periods=list(0:3,6:9,12:18),
  exposure="treat.grp",
  times.exposure=0,
  times.covariate=c(0,1,3,6,9,12,15,18),
  weight.censor="wtr.lag",
  sort.order=c("Chg.pansstotal","cs16",
               "pct.gain","epsmean",
               "qoltot","phase.change.cum",
               "cs14")
)
print(baltbl2 %>% filter(name.cov=="pct.gain") %>% mutate_at(c("D","SMD"),round,3))

## ---- fig.width=7, fig.height=5------------------------------------------
balplot2 <- makeplot(
  input=baltbl2,
  diagnostic=2,
  approach="none",
  censoring="yes",
  scope="average",
  average.over="distance",
  label.exposure="Treatment Group",
  label.covariate="Covariate"
)
balplot2

## ---- fig.width=7, fig.height=5------------------------------------------
diagtbl3 <- diagnose(
  input=catie_sim.wh,
  id="CATIEID",
  diagnostic=3,
  approach="weight",
  censoring="no",
  scope="all",
  exposure="studydisc",
  history="hs",
  temporal.covariate=c("phase.change.cum",
                       "Chg.pansstotal","cs14",
                       "cs16","pct.gain",
                       "epsmean","qoltot"),
  times.exposure=c(0,1,3,6,9,12,15,18),
  times.covariate=c(0,1,3,6,9,12,15,18),
  weight.exposure="wtr"
)

diagtbl3.o <- omit.history(
  input=diagtbl3,
  covariate.name=c("phase.change.cum","Chg.pansstotal",
                   "cs14","cs16",
                   "pct.gain","epsmean",
                   "qoltot"),
  omission="relative",
  distance=1
)

diagtbl3.oa <- apply.scope(
  input=diagtbl3,#input=diagtbl3.o,
  diagnostic=3,
  approach="weight",
  scope="recent",
  recency=0
)

diagplot3.oa <- makeplot(
  input=diagtbl3.oa,
  diagnostic=3,
  approach="weight",
  censoring="no",
  scope="recent",
  average.over="distance",
  label.exposure="Treatment Group",
  label.covariate="Covariate"
)
diagplot3.oa

## ------------------------------------------------------------------------
catie_sim.x <- catie_sim %>% filter(time==0) %>% select(-time)
print(catie_sim.x %>% select(CATIEID,treat.grp,studydisc,pct.gain,lead.pansstotal) %>% filter(CATIEID==1440))

## ------------------------------------------------------------------------
#add time index, widen, and add history
catie_sim.0 <- catie_sim.x %>% mutate(time=0)

catie_sim.0.w <- widen(
 input=catie_sim.0,
 id="CATIEID",
 time="time",
 exposure=c("studydisc","treat.grp"),
 covariate=c("cs14","cs16","weight",
             "pansstotal","epsmean",
             "qoltot",
             "exacer","unemployed",
             "white","black",
             "age.grp.1824","age.grp.2534")
) 

catie_sim.0.wh <- makehistory.two(
 input=catie_sim.0.w,
 id="CATIEID",
 exposure.a="treat.grp",
 exposure.b="studydisc",
 times=0,
 name.history.a="ht",
 name.history.b="hs"
)

#balance across treatment arm
diagtbl.0t <- diagnose(
  input=catie_sim.0.wh,
  id="CATIEID",
  diagnostic=1,
  approach="none",
  censoring="no",
  scope="all",
  exposure="treat.grp",
  history="ht",
  temporal.covariate=c("phase.change.cum","Chg.pansstotal",
                       "cs14","cs16","pct.gain",
                       "epsmean","qoltot"),
  static.covariate=c("exacer","Bpansstotal","unemployed",
                     "white","black",
                     "age.grp1824","age.grp.25344"),
  times.exposure=0,
  times.covariate=0
)
glimpse(diagtbl.0t)

#balance across censoring given treatment arm
diagtbl.0s <- diagnose(
  input=catie_sim.0.wh,
  id="CATIEID",
  diagnostic=1,
  approach="none",
  censoring="no",
  scope="all",
  exposure="studydisc",
  history="hs",
  temporal.covariate=c("phase.change.cum","Chg.pansstotal",
                       "cs14","cs16","pct.gain",
                       "epsmean","qoltot"),
  static.covariate=c("exacer","Bpansstotal",
                     "unemployed","white","black",
                     "age.grp1824","age.grp.25344"),
  times.exposure=0,
  times.covariate=0
)
glimpse(diagtbl.0s)

## ------------------------------------------------------------------------

stdcov <- function(x) {
  y <- (x-mean(x))/sd(x)
}

catie_sim.s <- catie_sim %>%
 mutate(Chg.pansstotal=stdcov(.data$Chg.pansstotal),
  cs14=stdcov(.data$cs14),
  cs16=stdcov(.data$cs16),
  pct.gain=stdcov(.data$pct.gain),
  epsmean=stdcov(.data$epsmean),
  qoltot=stdcov(.data$qoltot)
)

catie_sim.sw <- widen(
 input=catie_sim.s,
 id="CATIEID",
 time="time",
 exposure="studydisc",
 covariate=c("treat.grp","phase.change.cum",
             "Chg.pansstotal",
             "cs14","cs16",
             "pct.gain","epsmean","qoltot",
             "exacer","Bpansstotal",
             "unemployed"),
 weight.exposure="wtr"             
)

catie_sim.swh <- makehistory.two(
 input=catie_sim.sw,
 id="CATIEID",
 exposure.a="treat.grp",
 exposure.b="studydisc",
 times=c(0,1,3,6,9,12,15,18),
 name.history.a="ht",
 name.history.b="hs"
)

catie_sim.swhl3 <- lengthen(
  input=catie_sim.swh,
  id="CATIEID",
  diagnostic=3,
  censoring="no",
  exposure="studydisc",
  temporal.covariate=c("phase.change.cum","Chg.pansstotal",
                       "cs14","cs16","pct.gain",
                       "epsmean","qoltot"),
  times.exposure=c(0,1,3,6,9,12,15,18), 
  times.covariate=c(0,1,3,6,9,12,15,18),
  history="hs",
  weight.exposure="wtr"
)

catie_sim.swhlo3 <- omit.history(
  input=catie_sim.swhl3,
  covariate.name=c("phase.change.cum","Chg.pansstotal",
                   "cs14","cs16","pct.gain",
                   "epsmean","qoltot"),
  omission="relative",
  distance="1"
)

## ------------------------------------------------------------------------

library(broom)

modtbl <- catie_sim.swhlo3 %>%
  mutate(time=time.exposure,
         history=hs) %>%
  group_by(name.cov,history,time) %>%
  do(tidy(lm(formula=value.cov~studydisc,weights=wtr,.))) %>%
  filter(term=="studydisc") %>% 
  select(name.cov,time,history,term,estimate) %>%
  mutate(E=ifelse(term=="studydisc",1,0),
         time.exposure=time,
         time.covariate=time,
         H=history,
         SMD=estimate)%>%
         select(-term,time,history) %>%
         ungroup()
print(modtbl)


## ---- fig.width=7, fig.height=5------------------------------------------
modplot <- makeplot(
  input=modtbl,
  diagnostic=3,
  approach="weight",
  censoring="no",
  scope="recent",
  label.exposure="Treatment Group",
  label.covariate="Covariate"
)
modplot

