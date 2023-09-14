\name{get.updated.names}
\alias{get.updated.names}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get updated reptile species names
}
\description{
This function runs the same procedure as \link{update.names} to update reptile species names in a phylogeny or dataset, but instead of changing the species names of the data, saves the old and new names into a dafa frame. Additionally lists all possible species an original name could refer to if it matched as a synonym for more than one species, making manual determination of what the original name referred to easier. Also labels names for which no match was found as such to facilitate easier manual replacement.
}
\usage{
get.updated.names(dat, synonyms)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
Either a phylogenetic tree of class "phylo", or a dataset in the form of a data frame or named vector. If a data frame, species names must be included in a row called "species". If a named vector, species names should be the names of the vector.
}
  \item{synonyms}{
A list of synonyms for any number of valid reptile species. This can (and usually should) be produced by using \link{get.synonyms}. Only the species contained in this list will be used to search for synonyms matching species names, so the group contained in this list should encompass or exceed the entirety of the species diversity of the phylogeny/dataset used.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data frame with three columns. The first column contains the species names in the original dataset. The second column containts the currently recognized species which the first row matched to, either as a currently vaild form in itself or as a synonym. If the original name matched as a synonym to multiple currently recognized species, the cell lists all of these species. If the original name did not match to any currently valid species name, simply copies the original name into this cell.

The third row contains a note for each row. If "Valid," the original name was unambiguously referring to one valid species, so was kept as the updated name. If "Synonym," the original name was found as a synonym for unambiguously one valid species, which became the updated name. If "Multiple," the original name matched as a synonym to multiple currently recognized species names, which are listed in the updated names. If "Unrecognized", the original name was unrecognized as a synonym or a valid form for any species, so was left unchanged in the updated names column. Manual inspection is recommended to resolve updating names with a note of "Multiple" or "Unrecognized".
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

\link{get.synonyms}

\link{update.names}
}
\examples{
## gather synonyms for snakes in the genus Thamnophis
thamnophis <- get.species(group="Thamnophis", type="genus")
tham_synonyms <- get.synonyms(species=thamnophis, year=2000, show.progress=TRUE)

## load a phylogeny for Thamnophis (from Zaher et al., 2019)
tham_phy <- read.tree(text="((Thamnophis_sirtalis:4.706845,(Thamnophis_proximus:1.96124,Thamnophis_sauritus:1.96124):2.745606):1.185519,(((Thamnophis_nigronuchalis:3.462315,Thamnophis_rufipunctatus:3.462315):1.04709,((Adelophis_foxi:2.70963,(Thamnophis_melanogaster:2.535622,Thamnophis_valida:2.535622):0.174008):0.23529,((Thamnophis_errans:1.997499,Thamnophis_exsul:1.997499):0.557615,(Thamnophis_godmani:2.230129,((Thamnophis_sumichrasti:0.837483,Thamnophis_mendax:0.837483):0.564692,(Thamnophis_scaliger:0.869561,Thamnophis_scalaris:0.869561):0.532615):0.827954):0.324985):0.389806):1.564486):0.728537,((Thamnophis_chrysocephalus:3.448062,Thamnophis_fulvus:3.448062):1.534097,((Thamnophis_cyrtopsis:2.419571,Thamnophis_pulchrilatus:2.419571):1.606883,((Thamnophis_eques:1.985027,Thamnophis_marcianus:1.985027):0.828093,((Thamnophis_elegans:1.489543,(Thamnophis_brachystoma:0.39799,(Thamnophis_radix:0.151231,Thamnophis_butleri:0.151231):0.246759):1.091553):0.682727,(Thamnophis_hammondii:2.055541,(Thamnophis_ordinoides:1.839431,(Thamnophis_gigas:1.088956,(Thamnophis_couchii:0.91839,Thamnophis_atratus:0.91839):0.170566):0.750475):0.216111):0.116729):0.640851):1.213333):0.955706):0.255783):0.654421);")

## create a data table documenting what name changes would be made to the phylogeny
updated_names <- get.updated.names(dat=tham_phy, synonyms = tham_synonyms)
updated_names
}
