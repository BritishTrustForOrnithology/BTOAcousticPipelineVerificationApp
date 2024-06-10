#global startup commands


#make empty history dataset
history_start <- data.frame('path_from' = character(), 
                            'path_to' = character(),
                            'file_from' = character(), 
                            'file_to' = character(), 
                            stringsAsFactors = FALSE)


#populate/update dropdown? #turn off for big datasets as can impact performance
file_dropdown <- TRUE

#load the species list for populating dropdowns
load('data_resources/splist.Rdata')
