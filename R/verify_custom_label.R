#' Preparing a custom label for application
#' @details Custom labels are input as a string of species names or codes. They need 
#' to be standardised to codes and converted to a standard format then applied to 
#' the contents file using verify_apply 
#' 
#' @param this_batch = the name of the batch, for archiving if needed
#' @param this_label = the string of custom labels added by the user
#' @param this_user = the username of the verifier
#' @param global = the global reactive object
#' @param session = the session object
#' 
verify_custom_label <- function(this_batch, this_label, this_user, global, session) {
  if(is.null(this_label)) {
    shinyalert(title = "Error",
               text = "No species name or codes entered in the custom label box",
               type = "error",
               callbackR = message('Callback: No such name or code entered')
    )
    print('No species picked')
  }
  if(!is.null(this_label)) {
    #get the codes for these species
    selected_species <- subset(splist, select_val %in% this_label)
    
    #construct the label string
    label <- paste0('[',paste0("'", selected_species$code, "'", collapse = ','), ']')
    #print(label)
    
    #clear the value from the select
    updateSelectizeInput(inputId = 'select_label', selected = " ")
    
    #apply the verification to the contents
    verify_apply(this_batch, this_status = label, this_user, global, session)
  }
  
}