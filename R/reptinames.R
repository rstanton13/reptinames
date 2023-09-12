get.species <- function(group,type){
  {
    if(type=="taxon"){
      taxon.link <- paste("https://reptile-database.reptarium.cz/advanced_search?taxon=",group,"&submit=Search",sep="")
    }else if(type=="genus"){
      taxon.link <- paste("https://reptile-database.reptarium.cz/advanced_search?genus=",group,"&submit=Search",sep="")
    }
    taxon_page <- read_html(taxon.link)
    taxon_list <- taxon_page %>%
      html_elements(css = ".wide")
    txt <- html_text(taxon_list)
    txt <- gsub("\n","GSUBBEDTHIS",txt)
    txt <- str_split(txt,pattern="GSUBBEDTHIS",simplify=TRUE)
    txt <- str_squish(txt)
    txt <- txt[nchar(txt)>1]
    txt <- gsub("[^[:alnum:][:space:]']",'',txt)
    txt <- str_squish(txt)
    species_found <- as.numeric(str_split(txt[grepl("Species found",txt)],pattern=" ",simplify = TRUE)[3])
    ## keeping only the species ones: should have 2nd to last word be name in all caps, then last element be a 4-digit number (a year)
    second.last.is.name <- sapply(str_split(txt,pattern=" "),FUN=function(x){
      if(length(x)<4){
        FALSE
      }else{
        y <- x[length(x)-1]
        y==toupper(y)
      }
    })
    numbers_only <- function(x) !grepl("\\D", x)
    last.is.date <- sapply(str_split(txt,pattern=" "),FUN=function(x){
      if(length(x)<4){
        FALSE
      }else{
        y <- x[length(x)]
        ifelse(numbers_only(y),
               1500<=as.numeric(y)&as.numeric(y)<=2030,
               FALSE)
      }
    })
    txt <- txt[second.last.is.name & last.is.date]
    txt <- sapply(str_split(txt,pattern=" "),FUN=function(x){
      x1 <- x[1]
      x2 <- x[2]
      y <- paste(x1,x2,sep="_")
      y
    })
    species <- txt
    print(paste("Successfully recovered",length(species),"out of",species_found,"recognized species in",group,"from The Reptile Database"))
    species
  }
}

get.synonyms <- function(species,year=2000,show.progress=TRUE){
  synonyms <- as.list(species)
  names(synonyms) <- species
  find.first.name <- function(x){
    which(x==toupper(x))[1]
  }
  has.date <- function(x){
    y <- str_split(x,pattern=" ",simplify=TRUE)
    any(nchar(y) == 4 & removeNumbers(y)!=y)
  }
  find.date <- function(x){
    y <- str_split(x,pattern=" ",simplify=TRUE)
    first(which(nchar(y) == 4 & gsub("[^0-9.-]", "", y)==y))
  }
  has.AUTHOR <- function(x){
    x <- str_squish(removeNumbers(x))
    y <- str_split(x,pattern=" ",simplify=TRUE)
    any(toupper(y)==y)
  }
  last.is.number <- function(x){
    y <- str_split(x,pattern=" ",simplify=TRUE)
    !removeNumbers(y[length(y)]) == y[length(y)]
  }
  print(paste("Collecting all synonyms for the provided species that have been used since",year))
  for(i in 1:length(species)){
    sp <- species[i]
    genus <- str_split(sp,pattern="_",simplify=T)[1]
    epithet <- str_split(sp,pattern="_",simplify=T)[2]
    link <- paste("https://reptile-database.reptarium.cz/species?genus=",genus,"&species=",epithet,sep="")
    page <- read_html(link)
    page2 <- page %>%
      html_elements(css = ".odd")
    txt <- html_text2(page2[[as.numeric(gsub("[^[:alnum:][:space:]']",'',str_split(capture.output(page2)[grepl("Synonym",capture.output(page2))],pattern=" ",simplify=T)[1]))]])
    txt <- gsub("Synonym","",txt)
    txt <- gsub("\t","BREAK_HERE",txt)
    txt <- gsub("\n","BREAK_HERE",txt)
    txt <- str_split(txt,pattern="BREAK_HERE",simplify=T)
    txt <- gsub(r"{\s*\([^\)]+\)}","",txt)
    txt <- gsub('[[:punct:] ]+',' ',txt)
    txt <- txt[nchar(txt)>1]
    txt <- gsub("et al","",txt)
    txt <- gsub("sic","",txt)
    txt <- gsub("cf","",txt)
    txt <- txt[sapply(txt,FUN=has.AUTHOR)]
    txt <- txt[sapply(txt,FUN=has.date)]
    txt <- str_squish(txt)
    txt <- txt[removeNumbers(txt)!=txt]
    txt <- txt[sapply(txt,FUN=last.is.number)]
    txt <- gsub("Taylor ","TAYLOR ",txt)
    date.at <- sapply(txt,FUN=find.date)
    stop.at <- sapply(str_split(txt,pattern=" "),FUN=find.first.name)-1
    dates <- as.numeric(word(txt,start=date.at,end=date.at))
    txt <- word(txt,start=rep(1,length(txt)),end=stop.at)
    txt <- gsub(" ","_",txt)
    if(sum(dates > year)==0){
      txt <- sp
    }else{
      txt <- txt[dates > year]
    }
    txt
    txt <- sort(unique(txt))
    synonyms[[i]][1:length(txt)] <- txt
    if(show.progress){
      print(paste("Synonyms gathered for ", gsub("_"," ",sp),". ",round(100*i/length(species),1),"% complete.",sep=""))
    }
  }
  all.synonyms <- (unlist(synonyms))
  is.not.unique <- unique(unname(all.synonyms[duplicated(all.synonyms)]))
  if(length(is.not.unique>=1)){
    print("Multiple species have the following synonym(s):")
    for(i in 1:length(is.not.unique)){
      print(gsub("_"," ",is.not.unique[i]))
    }
    print("Tips matching these names will not be updated, but will be flagged for manual inspection.")
    print("Possible matches for these names can be viewed using get.updated.names()")
  }else{
    print("All synonyms are unique to their respective species! None have been removed.")
  }
  synonyms
}

update.names <- function(dat,synonyms,plot.changes=TRUE){
  if(class(dat)=="phylo"){
    phy <- dat
    synonyms.v <- unlist(synonyms) # synonyms should be a named list for (at least) all species in the phylogeny. Name = currently recognized name, elements = synonyms and currently recognized name
    tips <- phy$tip.label
    changed <- sapply(tips,FUN=function(x){
      if(length(which(synonyms.v==x))==1){
        removeNumbers(names(synonyms.v[which(synonyms.v==x)]))
      }else if(length(which(synonyms.v==x))>1){
        paste(removeNumbers(names(synonyms.v[which(synonyms.v==x)])),collapse=" or ")
      }else if(length(which(synonyms.v==x))==0){
        x
      }
    })
    cols.vec <- sapply(tips,FUN=function(x){
      if(length(which(synonyms.v==x))==1){
        "black"
      }else if(length(which(synonyms.v==x))>1){
        "orange2"
      }else if(length(which(synonyms.v==x))==0){
        "red2"
      }
    })
    for(i in 1:length(changed)){
      if(tips[i]!=changed[i] & cols.vec[i]=="black"){
        cols.vec[i] <- "blue2"
      }
    }
    changed <- sapply(tips,FUN=function(x){
      if(length(which(synonyms.v==x))==1){
        removeNumbers(names(synonyms.v[which(synonyms.v==x)]))
      }else if(length(which(synonyms.v==x))>1){
        x
      }else if(length(which(synonyms.v==x))==0){
        x
      }
    })
    new.phy <- phy
    new.phy$tip.label <- changed
    if(plot.changes==TRUE){
      par(mfrow=c(1,2),mar=c(0,0,2,0),oma=c(0,0,0,0))
      plot.phylo(phy,tip.color = cols.vec,
                 main = "Old tips",
                 cex=ifelse(130/Ntip(new.phy)>1,
                            1,
                            130/Ntip(new.phy))
      )
      plot.phylo(new.phy,tip.color = gsub("blue2","green3",cols.vec),
                 main = "Updated tips",
                 direction = "leftwards",
                 cex=ifelse(130/Ntip(new.phy)>1,
                            1,
                            130/Ntip(new.phy))
      )
      par(mfrow=c(1,1))
    }
    new.phy # the output is a phylogeny with updated tip labels/names
  }
  else{
    synonyms.v <- unlist(synonyms) # synonyms should be a named list for (at least) all species in the phylogeny. Name = currently recognized name, elements = synonyms and currently recognized name
    if(is.data.frame(dat)){
      tips <- dat$species
      changed <- sapply(tips,FUN=function(x){
        if(length(which(synonyms.v==x))==1){
          removeNumbers(names(synonyms.v[which(synonyms.v==x)]))
        }else if(length(which(synonyms.v==x))>1){
          x
        }else if(length(which(synonyms.v==x))==0){
          x
        }
      })
      dat$species <- changed
      dat
    }
    else if(is.vector(dat)){
      tips <- names(dat)
      changed <- sapply(tips,FUN=function(x){
        if(length(which(synonyms.v==x))==1){
          removeNumbers(names(synonyms.v[which(synonyms.v==x)]))
        }else if(length(which(synonyms.v==x))>1){
          x
        }else if(length(which(synonyms.v==x))==0){
          x
        }
      })
      names(dat) <- changed
      dat
    }
  }
}

get.updated.names <- function(dat,synonyms){
  if(class(dat)=="phylo"){
    phy <- dat
    synonyms.v <- unlist(synonyms) # synonyms should be a named list for (at least) all species in the phylogeny. Name = currently recognized name, elements = synonyms and currently recognized name
    tips <- phy$tip.label
    changed <- sapply(tips,FUN=function(x){
      if(length(which(synonyms.v==x))==1){
        removeNumbers(names(synonyms.v[which(synonyms.v==x)]))
      }else if(length(which(synonyms.v==x))>1){
        paste(removeNumbers(names(synonyms.v[which(synonyms.v==x)])),collapse=" or ")
      }else if(length(which(synonyms.v==x))==0){
        x
      }
    })
    cols.vec <- sapply(tips,FUN=function(x){
      if(length(which(synonyms.v==x))==1){
        "black"
      }else if(length(which(synonyms.v==x))>1){
        "orange2"
      }else if(length(which(synonyms.v==x))==0){
        "red2"
      }
    })
    note <- rep(NA,length(tips))
    for(i in 1:length(note)){
      if(tips[i]==changed[i] & cols.vec[i]=="black"){
        note[i] <- "Valid"
      }
      else if(cols.vec[i]=="orange2"){
        note[i] <- "Multiple"
      }
      else if(tips[i]!=changed[i] & cols.vec[i]=="black"){
        note[i] <- "Synonym"
      }
      else if(cols.vec[i]=="red2"){
        note[i] <- "Unrecognized"
      }
    }
    df <- data.frame(original=tips,
                     updated=changed,
                     note=note
    )
    row.names(df) <- NULL
    df
  }
  else{
    synonyms.v <- unlist(synonyms) # synonyms should be a named list for (at least) all species in the phylogeny. Name = currently recognized name, elements = synonyms and currently recognized name
    if(is.data.frame(dat)){
      tips <- dat$species
      changed <- sapply(tips,FUN=function(x){
        if(length(which(synonyms.v==x))==1){
          removeNumbers(names(synonyms.v[which(synonyms.v==x)]))
        }else if(length(which(synonyms.v==x))>1){
          paste(removeNumbers(names(synonyms.v[which(synonyms.v==x)])),collapse=" or ")
        }else if(length(which(synonyms.v==x))==0){
          x
        }
      })
      cols.vec <- sapply(tips,FUN=function(x){
        if(length(which(synonyms.v==x))==1){
          "black"
        }else if(length(which(synonyms.v==x))>1){
          "orange2"
        }else if(length(which(synonyms.v==x))==0){
          "red2"
        }
      })
      note <- rep(NA,length(tips))
      for(i in 1:length(note)){
        if(tips[i]==changed[i] & cols.vec[i]=="black"){
          note[i] <- "Valid"
        }
        else if(cols.vec[i]=="orange2"){
          note[i] <- "Matched multiple"
        }
        else if(tips[i]!=changed[i] & cols.vec[i]=="black"){
          note[i] <- "Synonym"
        }
        else if(cols.vec[i]=="red2"){
          note[i] <- "Unrecognized"
        }
      }
      df <- data.frame(original=tips,
                       updated=changed,
                       note=note
      )
      row.names(df) <- NULL
      df
    }
    else if(is.vector(dat)){
      tips <- names(dat)
      changed <- sapply(tips,FUN=function(x){
        if(length(which(synonyms.v==x))==1){
          removeNumbers(names(synonyms.v[which(synonyms.v==x)]))
        }else if(length(which(synonyms.v==x))>1){
          paste(removeNumbers(names(synonyms.v[which(synonyms.v==x)])),collapse=" or ")
        }else if(length(which(synonyms.v==x))==0){
          x
        }
      })
      cols.vec <- sapply(tips,FUN=function(x){
        if(length(which(synonyms.v==x))==1){
          "black"
        }else if(length(which(synonyms.v==x))>1){
          "orange2"
        }else if(length(which(synonyms.v==x))==0){
          "red2"
        }
      })
      note <- rep(NA,length(tips))
      for(i in 1:length(note)){
        if(tips[i]==changed[i] & cols.vec[i]=="black"){
          note[i] <- "Valid"
        }
        else if(cols.vec[i]=="orange2"){
          note[i] <- "Matched multiple"
        }
        else if(tips[i]!=changed[i] & cols.vec[i]=="black"){
          note[i] <- "Synonym"
        }
        else if(cols.vec[i]=="red2"){
          note[i] <- "Unrecognized"
        }
      }
      df <- data.frame(original=tips,
                       updated=changed,
                       note=note
      )
      row.names(df) <- NULL
      df
    }
  }
}
