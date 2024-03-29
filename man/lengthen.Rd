% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RFunctions_1_0_2.r
\name{lengthen}
\alias{lengthen}
\title{Function to create a "tidy" dataframe where the key observation is the pairing of exposure and covariate measurement times}
\usage{
lengthen(input, diagnostic, censoring, id, times.exposure, times.covariate,
  exposure, temporal.covariate, static.covariate = NULL,
  history = NULL, weight.exposure = NULL, censor = NULL,
  weight.censor = NULL, strata = NULL)
}
\arguments{
\item{input}{dataframe in wide format (e.g., indexed by person)}

\item{diagnostic}{diagnostic of interest e.g. 1, 2, or 3}

\item{censoring}{use censoring indicators/weights e.g. "yes" or "no"}

\item{id}{unique observation identifier e.g. "id"}

\item{times.exposure}{a vector of exposure measurement times e.g. c(0,1,2)}

\item{times.covariate}{a vector of covariate measurement times e.g. c(0,1,2)}

\item{exposure}{the root name for exposure measurements e.g. "a"}

\item{temporal.covariate}{a vector of root names for covariates whose values change over time e.g. c("l","m","n","o","p")}

\item{static.covariate}{a vector of root names for covariates whose values do not change (covariates listed here should not appear in the temporal.covariate argument)}

\item{history}{the root name for history measurements e.g. "h"}

\item{weight.exposure}{the root name for exposure weights e.g. "wa"}

\item{censor}{the root name for censoring indicators e.g. "s"}

\item{weight.censor}{the root name for censoring weights e.g. "ws"}

\item{strata}{the root name for propensity-score strata e.g. "e"}
}
\value{
A "tidy" dataframe where each record is indexed by the observation identifier,
exposure measurement time, exposure value, covariate name, covariate measurement time
and possibly exposure history and/or propensity score strata. Weights for exposure
and/or censoring will appear as additional columns. The dataframe will be restricted to
the uncensored if censoring rules were applied.
}
\description{
Function to create a "tidy" dataframe where the key observation is the pairing of exposure and covariate measurement times
}
\details{
The input dataset should have one record per observation (wide format) with the timing of variables indexed by an underscore followed by the time index (underscores should NOT appear anywhere else in the variable name). Any indexing scheme can be used (e.g. "var_1","var_4","var_9"), but it may be easiest to assign zero as the baseline index and increase it by one the unit for each subsequent measurement (e.g. "var_0","var_1","var_2"). You can use widen() to transform a person-time dataset into this format. The common referent value—to which all other exposure levels are compared—should be coded as the lowest value. Data with artificial censoring rules should contain a vector of time-indexed censoring indicators (1=censored, 0 otherwise).
}
\examples{
# Simulate wide data set with history
id <- as.numeric(c(1, 2))
a_0 <- as.numeric(c(0, 1))
a_1 <- as.numeric(c(1, 1))
a_2 <- as.numeric(c(1, 0))
l_0 <- as.numeric(rbinom(2, 1, 0.5))
l_1 <- as.numeric(rbinom(2, 1, 0.5))
l_2 <- as.numeric(rbinom(2, 1, 0.5))
m_0 <- as.numeric(rbinom(2, 1, 0.5))
m_1 <- as.numeric(rbinom(2, 1, 0.5))
m_2 <- as.numeric(rbinom(2, 1, 0.5))
n_0 <- as.numeric(rbinom(2, 1, 0.5))
n_1 <- as.numeric(rbinom(2, 1, 0.5))
n_2 <- as.numeric(rbinom(2, 1, 0.5))
h_0 <- as.character(c("H", "H"))
h_1 <- as.character(c("H0", "H1"))
h_2 <- as.character(c("H01", "H11"))

mydata.history <- data.frame(id, a_0, a_1, a_2,
                             l_0, l_1, l_2,
                             m_0, m_1, m_2,
                             n_0, n_1, n_2,
                             h_0, h_1, h_2,
                             stringsAsFactors=FALSE)

# Run the lengthen() function
mydata.long <- lengthen(input=mydata.history,
                        diagnostic=1,
                        censoring="no",
                        id="id",
                        times.exposure=c(0,1,2),
                        times.covariate=c(0,1,2),
                        exposure="a",
                        temporal.covariate=c("l","m"),
                        static.covariate=c("n"),
                        history="h"
                        )
}
