\name{Prweibull}
\alias{Prweibull}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Prweibull
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Prweibull(Variable,Xlabel="Observed Value",MainTitle="Log Normal Probability Paper",
                   color="gray")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Variable}{
%%     ~~Describe \code{x} here~~
Array for random variable to be plotted}
\item{Xlabel}{Default is "Observed Value"}
\item{MainTitle}{Default is "Reverse Weibull Probability Paper"}
  \item{color}{Lattice color:default is "gray"
%%     ~~Describe \code{color} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Shinsuke Sakai
}
\note{
%%  ~~further notes~~
Determine lacation parameter "loc" by trial and error. Substitute loc-x for Variable.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
loc<-10
x <- rrweibull(100,loc=loc)
Prweibull(Variable=loc-x,Xlabel="test var",MainTitle="Reverse weibull test")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
