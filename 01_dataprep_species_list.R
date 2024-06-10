#' Save a species list for using for codes and names in dropdowns etc
#' Keep only bird species with either a 2-letter or 5-letter code
#' Make the list have codes and species names for searching
#' Sort on length so codes are listed first, otherwise typing ST will bring oSTrich etc well before ST code
#'  
#' Simon Gillings
#' January 2024

library(BTOTools)

splist <- subset(global_species_lookup, taxa == 'Birds' & taxon_rank_id == 100)
splist <- subset(splist, !(is.na(code2ltr) & is.na(code5ltr)))
splist$code <- ifelse(!is.na(splist$code2ltr), splist$code2ltr, splist$code5ltr)

head(splist)

#make one version of the list from species codes
splist1 <- splist
splist1$select_val <- splist1$code
#make the other version from English names
splist2 <- splist
splist2$select_val <- splist2$english_name

#combine the two lists
splist <- rbind(splist1, splist2)
#calculate the length of each select val so it can be sorted in length for easier selecting
splist$nchar <- nchar(splist$select_val)
splist <- splist[order(splist$nchar, splist$select_val),]

#replace the dots with underscores in the codes to be used for labelling
splist$code <- gsub('.', '_', splist$code, fixed = TRUE)

#drop surplus columns
splist <- splist[,c("select_val", "code")]
head(splist)

save(splist, file = 'data_resources/splist.Rdata')
