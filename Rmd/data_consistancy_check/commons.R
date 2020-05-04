# Following function might need double check correctness
check_data <- function(data_dropbox, data_redcap, dropbox_id_col, redcap_id_col, dropbox_field_col, redcap_field_col) {
  data_dropbox <- arrange(data_dropbox, get(dropbox_id_col))
  data_redcap <- arrange(data_redcap, get(redcap_id_col))
  for (val in 1:nrow(data_redcap)){
    progress(100*val/nrow(data_redcap))
    search_id <- select(data_redcap, eval(redcap_id_col))[val,]
    search_val <- select(data_redcap, eval(redcap_field_col))[val,]
    if (nrow(filter(data_dropbox, get(dropbox_id_col)==search_id))==0){
      #      return(TRUE)
      next
    }
    #    tryCatch({
    if (is.na(select(filter(data_dropbox, get(dropbox_id_name)==search_id), eval(dropbox_field_col))) & is.na(search_val)){
      next
    }
    if (xor(is.na(select(filter(data_dropbox, get(dropbox_id_name)==search_id), eval(dropbox_field_col))), is.na(search_val))){
      return(FALSE)
    }
    if (select(filter(data_dropbox, get(dropbox_id_name)==search_id), eval(dropbox_field_col))!=search_val)
      return(FALSE)
    #    }, finally = {
    #      print(search_id)
    #      print(search_val)
    #    })
  }
  return(TRUE)
}