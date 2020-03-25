

#' Converts messy names and ID's to tidy clean ones.
#'
#' For sorting out a vector with long and complicated identifiers or row names, where the true ID of a row is hidden in a string.\cr
#' E.g: Make "dirty" ID's like "A0006_3911_BT-F1_GTCGTCTA_run20190930N" turn into "clean" ID's like 3991_BT
#' @param vector A vector of "dirty" IDs
#' @param identifier ID's need to be formated with a number and following identifier, e.g "34_individuals2019" where "_individuals2019" is the identifier. Any entries not matching this format will be removed.
#' @param identifier_left Wether the identifier is on the left hand (T) or right-hand (R) side of the number
#' @param numLength if you want leading zeroes, use this parameter to specify the length of the number, e.g "8" for 00000342
#' @param prefix if you want a prefix in the new cleaned ID. Ex: "individuals2019_" will give you "individuals2019_0034". If not specified, the old identifier will be used instead. Set to NA if you only want the number.
#' @param na_remove if you want to remove any entries that don't follow your pattern (otherwise, they'll turn to NA)
#' @export
clean_ID = function(vector,identifier="", identifier_left=F, numLength=4, prefix, na_remove=F,numeric=F) {
  require(tidyverse)
  require(stringr)

  # SET THE REGULAR EXPRESSION
  if (!identifier_left) regExpr = paste("[0-9]{1,50}",identifier,sep="")
  else                  regExpr = paste(identifier,"[0-9]{1,50}",sep="")

  # Extract the ID's from the dirty ID's
  ID_dirty = vector
  ID_clean = ID_dirty %>% str_extract(regExpr)

  # Remove the old identifier (for now)
  ID_clean = ID_clean %>% sub(identifier,"",.)

  # Remove NA values
  if (na_remove) ID_clean = ID_clean[!is.na(ID_clean)]

  # Add leading zeroes
  if (numLength!=0) ID_clean[!is.na(ID_clean)] = ID_clean[!is.na(ID_clean)] %>% as.numeric() %>% sprintf(paste("%0",numLength,"d",sep=""),.)

  # Make the ID completely numeric
  if (numeric) ID_clean = as.numeric(ID_clean)

  # Add the new prefix
  if (exists("prefix")){
    if (is.na(prefix))       return(ID_clean)
    else                     ID_clean[!is.na(ID_clean)] = paste(prefix, ID_clean[!is.na(ID_clean)], sep="")
  }
  else if (identifier_left)  ID_clean[!is.na(ID_clean)] = paste(ID_clean[!is.na(ID_clean)], identifier, sep="")
  else if (!identifier_left) ID_clean[!is.na(ID_clean)] = paste(identifier, ID_clean[!is.na(ID_clean)], sep="")

  return(ID_clean)
}


#' In a dataframe, converts messy names and ID's to tidy clean ones.
#'
#' For sorting out column with long and complicated identifiers or row names, where the true ID of a row is hidden in a string.\cr
#' E.g: Make "dirty" ID's like "A0006_3911_BT-F1_GTCGTCTA_run20190930N" turn into "clean" ID's like 3991_BT
#' @param df The data frame
#' @param column The name of a column containing dirty IDs
#' @param identifier ID's need to be formated with a number and following identifier, e.g "34_individuals2019" where "_individuals2019" is the identifier. Any entries not matching this format will be removed.
#' @param identifier_left Wether the identifier is on the left hand (T) or right-hand (R) side of the number
#' @param numLength if you want leading zeroes, use this parameter to specify the length of the number, e.g "8" for 00000342
#' @param prefix if you want a prefix in the new cleaned ID. Ex: "individuals2019_" will give you "individuals2019_0034"
#' @param na_remove if you want to remove any rows that don't follow your pattern (otherwise, they'll turn to NA). Default is True.
#' @export
clean_ID_df = function(df, column_name="ID", identifier="", identifier_left=F, numLength=F, prefix="", na_remove=T, keep_name=F, numeric=F){
  require(tidyverse)
  require(stringr)

  # Ectract the dirty ID's
  ID_dirty = unlist(df[column_name])

  # Clean the ID
  ID_clean = clean_ID(ID_dirty, identifier, identifier_left, numLength, prefix,numeric=numeric)

  # Insert the cleaned ID's into the column
  df[column_name] = ID_clean

  # Remove NA values
  if (na_remove) df = df %>% remoNA(column_name)

  # Rename the old ID column
  # Check what name to use
  if (keep_name == F) column_name_new = "ID"
  else if (keep_name == T) column_name_new = column_name
  else column_name_new = keep_name
  # Rename the column to "ID"
  df = df %>% rename(!! column_name_new := !! column_name)

  return(df)
}



#' Converting sdy to F or M
#'
#' Used on dataframes, for determining sex based on SDY in a given column
#' @export
#'
determineSex = function(dataframe, column, cutoff) {
  dataframe = dataframe %>% group_by(ID, SEQRUN) %>% mutate(
    sex = SDY_to_sex(dataframe %>% select(matches(column)) %>% filter(dataframe$ID==ID) , cutoff)
  )
  # %>% select(-c(column))
  return(dataframe )
}


#' Set sex to NA if many SNPs missing.
#'
#' In a dataframe, sets sex to "NA" when a certain amount of SNP's are missing as NA
#' @export
#'
unSexBad = function(dataframe, column, sensitivity=0.35) {
  sex = unlist(dataframe[column])
  colNum = length(names(dataframe))

  na_prop <- apply(dataframe, 1, function(x) sum(is.na(x))/length(x))

  sex[na_prop > sensitivity] = "?"

  dataframe$sex = sex
  return(dataframe)
}

#' Rename genotypes based on a lookup table
#'
#' In a dataframe, rename genotype columns
#' @export
renameGenotypes = function(dataframe, LUT, not_genotypes=c()) {
  for (i in names(dataframe %>% select(-c(not_genotypes)))) {
    dataframe <- dataframe %>% renameGenotype(i, LUT)
  }
  dataframe
}

#' determinesex2
#' @keywords internal
determineSex2 = function(dataframe, column, cutoff) {
  dataframe = dataframe %>% group_by(ID) %>% mutate(
    sex = SDY_to_sex(dataframe %>% select(matches(column)) %>% filter(dataframe$ID==ID) , cutoff)
  )
  # %>% select(-c(column))
  return(dataframe )
}

#' SDY_to_sex
#' @keywords internal
SDY_to_sex = function(vector, cutoff) {
  sdy = mean(unlist(vector[1]), na.rm=T)

  if (is.na(sdy)) return(NA)
  else if (sdy <= cutoff) return("F")
  else return("M")
}

#' safeMerge
#' @keywords internal
safeMerge = function(vector){
  # Get the datatype of the vector
  type = typeof(vector)

  #1 remove NA values
  vector = vector[!is.na(vector)]
  #check if the remaning entries are equal

  #if they are, return one of them
  #if they're not, return NA

  if (length(unique(vector)) == 1) return(unique(vector))
  else return(convertType(NA,type))
}

#' renameGenotype
#' @keywords internal
renameGenotype = function(dataframe, column, LUT=c("1"="1 1","2"="1 2","3"="2 2")){
  genotype = dataframe[column] %>% unlist()

  col = LUT[genotype]
  col[is.na(col)] = "* *"
  dataframe[column] = col

  return(dataframe)
}


clean_genotypes = function(df) {

  message(glue("dataframe starting at {ncol(df)} columns."))

  nas = df %>% colnames() %>% sapply(function(x){
    sum(is.na(df[x]))
  })

  message("See historgram...")
  hist(nas, breaks=ncol(df))
  filter_at = numextract(readline(prompt="Enter cutoff value: "))
  df = df %>% select_if(~sum(is.na(.)) < filter_at)

  message(paste("Columns cut off at",filter_at,"NA-s.",sep=" "))
  message(glue("Dataframe is now at {ncol(df)} columns."))
  message(glue("Removing mono-something columns"))

  df = Filter(function(x){ length(unique(x))!=1 }, df)
  message(glue("Dataframe is now at {ncol(df)} columns."))
  message("Done")

  return(df)
}
