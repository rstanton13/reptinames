\name{get.info}
\alias{get.info}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gather species distribution and/or reproduction information from The Reptile Database
}
\description{
This function returns distribution information and/or reproductive biology information from The Reptile Databse for a valid species or vector of species.
}
\usage{
get.info(species, data.type=c("distribution","reproduction"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{species}{
A vector of valid, currently recognized species names. The genus and specific epithet must be separated by a space or an underscore in each element of the vector.
}
  \item{data.type}{
The type of data to gather. If "distribution", will return only distribution information. If "reproduction", will return only reproduction information. If a vector contianing both, will return both.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data frame with the requested type of data for each species in the input species vector.
}
\references{
Uetz, P., Freed, P, Aguilar, R., Reyes, F. & Hošek, J. (eds.) (2023) The Reptile Database, http://www.reptile-database.org
}
\author{
Riley Stanton
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{get.species}
}
\examples{
## gather distribution and reproductive data for snakes in the genus Thamnophis
thamnophis <- get.species(group="Thamnophis", type="genus")
tham.info <- get.info(species=thamnophis, data.type=c("distribution","reproduction"))
}

