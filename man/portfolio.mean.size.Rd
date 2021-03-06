\name{portfolio.mean.size}
\alias{portfolio.mean.size}

\title{
Calculate Mean Sub-Portfolio Size
}
\description{
Primarily used in the case of an unconditional sort - this function computes the average number of securities in each sub-portfolio across time.  
}
\usage{
portfolio.mean.size(sort.output)
}

\arguments{
  \item{sort.output}{
object returned from either the conditional.sort or unconditional.sort function. 
}
}

\author{
Alexander Dickerson and Jonathan Spohnholtz
}

\examples{
# Load the included data
library(portsort)
data(Factors)

# Specifiy the sort dimension - in this case, a double-sort on lagged returns and Bitcoin volumes
dimA = 0:3/3
dimB = 0:3/3

# Specify the factors
# Lagged returns, lagged volumes are stored in the Factors list
R.Forward = Factors[[1]]; R.Lag = Factors[[2]]; V.Lag = Factors[[3]]

# Subset the data from late 2017
R.Forward = R.Forward["2017-12-01/"]
R.Lag = R.Lag["2017-11-30/2018-09-05"]
V.Lag = V.Lag["2017-11-30/2018-09-05"]

Fa = R.Lag
Fb = V.Lag

# Conduct an unconditional sort (in this case) or a conditional sort
sort.output = unconditional.sort(Fa = Fa, Fb = Fb , R.Forward = R.Forward, dimA = dimA, dimB = dimB)

# We want to compute the average size of each sub-portfolio

portfolio.mean.size(sort.output)
}