\name{PgumbelL}
\alias{PgumbelL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
PgumbelL
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Probability paper for gumbel distribution for multi collection of variables given in list structure
}
\usage{
PgumbelL(Variable, Xlabel = "Observed Value", MainTitle = "Gumbel Probability Paper", color = "gray")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Variable}{
  multi collection of variables given in list structure
%%     ~~Describe \code{Variable} here~~
}
\item{Xlabel}{Default is "Observed Value"}
\item{MainTitle}{Default is "Gumbel Probability Paper"}
  \item{color}{Lattice color:default is "gray"
%%     ~~Describe \code{color} here~~
}
\item{names}{array of symbol names in character}
\item{pchs}{array of pch number}
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
library(evd)
x1 <- rgumbel(100,loc=0.1,scale=0.2)
x2 <- rgumbel(200,loc=0.2,scale=0.4)
x3 <- rgumbel(1250,loc=0.5,scale=0.1)
xx<-list(x1,x2,x3)
PgumbelL(Variable=xx,Xlabel="test var",MainTitle="Gumbel test",names=c("T1","T2","T3"),pchs=c(4,5,6))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
