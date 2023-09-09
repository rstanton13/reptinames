\name{update.names}
\alias{update.names}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Update reptile species names in phylogenetic trees and datasets
}
\description{
This function uses a list of synonyms produced using \link{get.synoyms} or created manually (not recommended) to replace outdated species names in phylogenies and datasets with the currently recoggnized forms they refer to.
}
\usage{
update.names(dat, synonyms, plot.changes = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
Either a phylogenetic tree of class "phylo", or a dataset in the form of a data frame or named vector. If a data frame, species names must be included in a row called "species". If a named vector, species names should be the names of the vector.
}
  \item{synonyms}{
A list of synonyms for any number of valid reptile species. This can (and usually should) be produced by using \link{get.synonyms}. Only the species contained in this list will be used to search for synonyms matching species names, so the group contained in this list should encompass or exceed the entirety of the species diversity of the phylogeny/dataset used.
}
  \item{plot.changes}{
Logical. Only appliies of dat is a phylogeny. If TRUE, will plot the input phylogeny and the output phylogeny, highlighting tips that were updated to their currently accepted forms (blue for synonms, green for the form it was changed to), tips that matched as synonyms for multiple currently accepted forms (orange), and tips that matched no currently accepted forms or synonyms (red). Tips that were originally valid forms are colored black.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
If dat is a phylogenetic tree, returns an identical phylogenetic tree, but with updated tip labels wherever possible.

If dat is a data frame, returns an indentical data frame, but with updated names in species column wherever possible.

If dat is a named vector, returns an identical named vector, but with updated names wherever possible.
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

\link{get.updated.names}
}
\examples{
## gather synonyms for snakes in the genus Thamnophis
thamnophis <- get.species(group="Thamnophis", type="genus")
tham_synonyms <- get.synonyms(species=thamnophis, year=2000, show.progress=TRUE)

## load a phylogeny for Thamnophis (from Zaher et al., 2019)
tham_phy <- read.tree("((Thamnophis_sirtalis:4.706845,(Thamnophis_proximus:1.96124,Thamnophis_sauritus:1.96124):2.745606):1.185519,(((Thamnophis_nigronuchalis:3.462315,Thamnophis_rufipunctatus:3.462315):1.04709,((Adelophis_foxi:2.70963,(Thamnophis_melanogaster:2.535622,Thamnophis_valida:2.535622):0.174008):0.23529,((Thamnophis_errans:1.997499,Thamnophis_exsul:1.997499):0.557615,(Thamnophis_godmani:2.230129,((Thamnophis_sumichrasti:0.837483,Thamnophis_mendax:0.837483):0.564692,(Thamnophis_scaliger:0.869561,Thamnophis_scalaris:0.869561):0.532615):0.827954):0.324985):0.389806):1.564486):0.728537,((Thamnophis_chrysocephalus:3.448062,Thamnophis_fulvus:3.448062):1.534097,((Thamnophis_cyrtopsis:2.419571,Thamnophis_pulchrilatus:2.419571):1.606883,((Thamnophis_eques:1.985027,Thamnophis_marcianus:1.985027):0.828093,((Thamnophis_elegans:1.489543,(Thamnophis_brachystoma:0.39799,(Thamnophis_radix:0.151231,Thamnophis_butleri:0.151231):0.246759):1.091553):0.682727,(Thamnophis_hammondii:2.055541,(Thamnophis_ordinoides:1.839431,(Thamnophis_gigas:1.088956,(Thamnophis_couchii:0.91839,Thamnophis_atratus:0.91839):0.170566):0.750475):0.216111):0.116729):0.640851):1.213333):0.955706):0.255783):0.654421);")

## update the names on the phylogeny
updated_phy <- update.names(dat=tham_phy, synonyms = tham_synonyms, plot.changes = TRUE)
updated_phy
}

