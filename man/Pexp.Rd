\name{Pexp}
\alias{Pexp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\strong{Pexp}
}
\description{
Probability paper for exponential distribution
}
\usage{
Pexp( Variable,Xlabel="Observed Value",MainTitle="Exponential Probability Paper", color = "gray")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Variable}{
%%     ~~Describe \code{x} here~~
Array for random variable to be plotted}
\item{Xlabel}{Default is "Observed Value"}
\item{MainTitle}{Default is "Exponential Probability Paper"}
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
Shinsuke Sakai
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x<-rexp(100,rate=1)
Pexp(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
