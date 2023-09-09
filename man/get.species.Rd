\name{get.species}
\alias{get.species}
get.species
\title{
Compile all currently recognized species in a given reptile taxon
}
\description{
Searches The Reptile Database for a given taxonomic group and returns a vector containing all (or nearly all) of the species listed as being valid members of that group.
}
\usage{
get.species(group, type)
}
\arguments{
  \item{group}{
A character string containing a single valid (according to The Reptile Database) reptilian taxonomic group to search for species within. Must be at the genus level or higher.
}
\item{type}{
A character string specifying the level of organization of the taxon of interest. Either "genus" (for genera) or "taxon" (for all other higher-level taxonomic groups, i.e., families, orders, e).
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a string of all currently recognized species names within the specified taxonomic group. Can be used in \link{get.synonyms} to find synonyms for these species.
}
\references{
Uetz, P., Freed, P, Aguilar, R., Reyes, F. & Hošek, J. (eds.) (2023) The Reptile Database, http://www.reptile-database.org
}
\author{
Riley Stanton
}
\note{
Sometimes returns marginally less (e.g., 1 alethinophidian species out of over 3000) than all the species in the specified group, for reasons that are unclear at the moment. This is unlikely to create problems later down the line in your workflow, as missing a currently recognized species will simply  highlight the species (if it is in the dataset) for manual inspection but leave its name unchanged upon failing to find the  as a synonym or recognized form.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{get.species}

\link{get.synonyms}

\link{update.names}

\link{get.updated.names}
}
\examples{
## gather all currently valid names of snakes in the genus Thamnophis
thamnophis <- get.species(group="Thamnophis", type="genus")
thamnophis
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
