# Function to check that the mandatory variables exist in the dataset (missing or redundant columns)
# param: df, dataframe to check the colnames
# param: colMandatory, list of mandatory headers
# param: showMsg (if T shows the function messages)
# return: 1 or 0, if 1 the check is fine
# rdname: headerControl

headerControl <- function(df = dfToCheck, colMandatory = listMandatoryToCheck,showMsg = F) {
 
  # string of mandatory variables, missing
  errorColMissing = c()

  # string of mandatory variables, duplicate
  errorColMultiple = c()

  for (col in colMandatory){
    ckMandatory = which(colnames(df) == col)
    if (length(ckMandatory) == 0) {
      errorColMissing = c(errorColMissing,col)
      next
    }
    if (length(ckMandatory) > 1) {
      errorColMultiple = c(errorColMultiple,col)
      next
    }
  }
  rm(col)
  if (showMsg) {
    cat(sprintf('missing mandatory variables: %d \n',length(errorColMissing)))   
    if (length(errorColMissing) > 0)
      cat(sprintf(' (%s)\n',paste(errorColMissing,collapse = ',')))

    cat(sprintf('multiple mandatory variables: %d \n',length(errorColMultiple)))
    if (length(errorColMultiple) > 0)
      cat(sprintf(' (%s)\n',paste(errorColMultiple,collapse = ',')))
  }
  if ( (length(errorColMissing) + length(errorColMultiple)) > 0) {
    return(0)
  } else {
    return(1)
  }
}
