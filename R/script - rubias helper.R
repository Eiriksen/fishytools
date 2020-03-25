

convert_rubias <- function(df,indiv,sample_type,repunit,collection) {


  # 1: Prepare initial table
  df_rubias <- data.frame(indiv=indiv,sample_type=sample_type, repunit=repunit, collection=collection)

  # 2: split genotypes
  # split make a lookup-table for converting bases to numbers
  lut <- c("A"=1, "G"=2, "C"=3, "T"=4)
  # create an empty dataframe
  df_new <- data.frame(index=1:nrow(df))

  # fill the new dataframe with columns from the previous one, but split
  for (i in colnames(df)) {
    df_new[[i]]             <- lut[substr(df[[i]],1,1)]
    df_new[[paste(i,"_2")]] <- lut[substr(df[[i]],2,2)]
  }

  df_new[] <- lapply(df_new, as.integer)

  # return the new rubias file
  df_rubias <- cbind(df_rubias, df_new %>% select(-c(index)))

}
