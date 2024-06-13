# reptinames
### An R package for reptile nomenclatural standardization in phylogenies and datasets

## Use:
This packages interfaces with The Reptile Database to update reptile species names in phylogenies and datasets. The intention of this package is to facilitate using data from various studies and phylogneies that may have different names for the same species by automatically changing the species names in these datasets to their currently recongized forms whenever possible. When not possible to automatically update names (e.g., name in dataset is synonymous with multiple valid forms, or is misspelled and cannot be automatically synonymized), the package specifies that these names are in need of manual inspection and for what reason (if synonymous with multiple forms, lists these forms; if unrecognized as any valid form or synonym, states this). 

## Installation:
```
install.packages("devtools")
library(devtools)
install_github("rstanton13/reptinames")
library(reptinames)
```

The package is relatively straightforward and functions are explained in their help pages. I may add more information here later.
