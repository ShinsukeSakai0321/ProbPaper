\name{PnormL}
\alias{PnormL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
PnormL
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Probability paper for normal distribution for multi collection of variables given in list structure
}
\usage{
PnormL(Variable, Xlabel = "Observed Value", MainTitle = "Normal Probability Paper", color = "gray", names = c("1", "2", "3"), pchs = c(0, 1, 2))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Variable}{
%%     ~~Describe \code{Variable} here~~
  multi collection of variables given in list structure
}
  \item{Xlabel}{
%%     ~~Describe \code{Xlabel} here~~
Default is "Observed Value"
}
  \item{MainTitle}{
%%     ~~Describe \code{MainTitle} here~~
Default is "Gumbel Probability Paper"
}
  \item{color}{
%%     ~~Describe \code{color} here~~
Lattice color:default is "gray"
}
  \item{names}{
%%     ~~Describe \code{names} here~~
array of symbol names in character
}
  \item{pchs}{
%%     ~~Describe \code{pchs} here~~
array of pch number
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
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x1 <- rnorm(100,mean=0,sd=1)
x2 <- rnorm(200,mean=2,sd=2)
x3 <- rnorm(1250,mean=4,sd=0.5)
xx<-list(x1,x2,x3)
PnormL(Variable=xx,Xlabel="test var",MainTitle="Normal test",names=c("T1","T2","T3"),pchs=c(4,5,6))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
