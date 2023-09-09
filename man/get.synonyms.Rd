\name{get.synonyms}
\alias{get.synonyms}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compile a list of synonyms for recognized reptile species
}
\description{
Searches The Reptile Database for synonyms of provided valid species names. This function searches through The Reptile Database in real time, and consequently may take a while to run to completion if a large number of species are provided.
}
\usage{
get.synonyms(species, year=2000, show.progress=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{species}{
A character string of currently recognized reptile species to search for synonyms for. Can be created using \link{get.current.species} (recommended usage), or made manually. If the names of any species in this vector are not recognized as currently valid by The Reptile Database, this function will not work.
}
  \item{year}{
An integer representing a year. Synonyms used/proposed before this year will not be retained in the dataset. Very old names for species (e.g., those from the 1800s) are now widespread as historical synonyms across many species, which would result in many species having these old names placed in their lists. Since these old names are unlikely to be encountered in modern datasets, this provides you with the ability to filter them out to retain only a more informative, smaller list of sysnonyms for each species. A more recent year will result in a smaller number of synonyms being retained for each species. Setting this to a very recent year is not recommended, as this will remove almost all synonyms for each species. Defaults to 2000.
}
  \item{show.progress}{
Logical. If TRUE, will print each species for which synonyms have been compiled for and indicate as a percentage the progress towards completion for all species. If FALSE, will not print these updates. Defaults to TRUE.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a named list of synonyms for each species provided. Can be used in \link{update.species.names} to update species names in phylogenies or datasets.
}
\references{
Uetz, P., Freed, P, Aguilar, R., Reyes, F. & Hošek, J. (eds.) (2023) The Reptile Database, http://www.reptile-database.org
}
\author{
Riley Stanton
}
\note{
Though it is designed to accomodate some abnormailities in formatting on synonym lists in The Reptile Databas, some formatting errors on the website may cause this function to fail, even if all species provided are valid. Please contact me if such an error is encountered.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{get.species}

\link{update.names}

\link{get.updated.names}
}
\examples{
## gather synonyms for snakes in the genus Thamnophis
thamnophis <- get.species(group="Thamnophis", type="genus")
tham_synonyms <- get.synonyms(species=thamnophis, year=2000, show.progress=TRUE)
tham_synonyms
}
