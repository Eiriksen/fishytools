


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

#' Cecks if certain columns exist in a dataset and returns an error message if not
#' @keywords internal
check_columns = function(dataset,columns,preMessage="Missing columns:"){
  message = c(preMessage)

  for (i in columns) {
    if(!i %in% colnames(dataset))
    {
      message = c(message,paste("Column",i,"is missing."))
    }

    if(length(message)>1) stop(message)
  }
}

#' Converts anything to a number
#' @export
numextract <- function(string){
  require(stringr)
  as.numeric(str_extract(string, "\\-*\\d+\\.*\\d*"))
}


#' Converts anything to a number
#' @export
intextract <- function(string){
  require(stringr)
  as.integer(str_extract(string, "\\-*\\d+\\.*\\d*"))
}



#' Removes rows with NA in a given column
#'
#' Removes NA rows (in a given column) from a dataset
#' @export
remoNA = function(dataset,column){
  return(dataset[which(!is.na(dataset[column])),])
}

# ' Removes rows that have more than a certain proportion of NA values in them
#' @export
remoNA_row = function(dataset, proportion){
  return(dataset[!rowSums(is.na(dataset)) > ncol(dataset)*proportion,])
}

#' Replace NA values with unique new identifier
#'
#' Changes all NA values to an unique identifier
#' @export
uniNA = function(values){
  uniques = cumsum(is.na(values))
  for (i in 1:length(values)){
    if (is.na(values[i])) {
      values[i]=paste("NA-",uniques[i],sep="")
    }
  }
  return(values)
}


#' Changes NA values in a dataframe to a given value
#' @export
  changeNA = function(dataset,value){
  dataset[is.na(dataset)] = value
  return(dataset)
}


#' Changes certain values in a list/vector to NA
#' @export
makeNA = function(values, which){
  for (i in which){
  values[values==i] = NA
  }
  return(values)
}


#' Converts a variabe from one type to another
#' @export
convertType = function(var,type){ #https://stackoverflow.com/questions/47410679/change-type-of-object-based-on-typeof-another-object
  unlist(lapply(var,paste0('as.',type)))
}

unSelect = function(df,...){
  return(df %>% select(-c(...)))
}

#' For looking up variables from one dataset and then add them to another one.
#'
#' Use to add a column (value) to a dataset (samples), from another dataset (lookup), based on an identifier that exists in both (id_lookup)
#' @param df_samples samples to look up
#' @param df_lookup dataframe to look up against
#' @param id_column common column between the two sets containing unique identifiers for rows
#' @param value the value that is looked up and added to df_samples
#' @example fishies <- fishies %>% lookup(df_birthdays, "fish_ID", "date_birth")
#' @export
lookup_olde = function(df_samples, df_lookup, id_column, value_column,default=NA,overwrite=T){
  message("Looking up ",value_column," using ",id_column,"...")
  #check if the df_samples already has a column with /value/
  #if not, create one and fill it with NA
  if(!value_column %in% colnames(df_samples)){
    df_samples[[value_column]] = default
  }

  # for each row,
  # 1. get all matching rows (r_match)
  # 2. remove  all rows containing NA values
  # 3. select the first of these rows, and use the value from that one
  # 4. if the value is NA, use the value already present
  # 5. if there already is a value in the row that's being looked up, only overwrite if overwrite==T

  df_lookup[id_column] = df_lookup[id_column] %>% as.character()
  df_samples[id_column] = df_samples[id_column] %>% as.character()


    values = apply(df_samples,MARGIN=1, FUN=function(s_id){

    r_match = df_lookup %>%
              filter(!!sym(id_column) == s_id) %>%
              remoNA(id_column)



    # check if this item was found (and is not NA) (and the original is empty or overwrite==T)
    if (nrow(r_match)!=0 & !is.na(r_match[[value_column]][1]) & (is.na(x[[value_column]][1]) | overwrite==T)){
      r_match[[value_column]][1] %>% unlist()
    }
    else{

      #if not, use the value already present
      x[[value_column]][1] %>% unlist()
    }
  })
  message(typeof(values))
  df_samples[[value_column]] = values
  message("Done!")
  df_samples
}

lkp = function(df_samples, df_lookup, id_samp, id_lookup, value_samp, value_lookup, default=NA,overwrite=T){

  if (missing(id_lookup)) id_lookup = id_samp
  if (missing(value_lookup)) value_lookup = value_samp

  message("Looking up ",value_lookup," using ",id_samp,"...")

  #check if the df_samples already has a column with /value/
  #if not, create one and fill it with NA
  if(!value_samp %in% colnames(df_samples)){
    df_samples[[value_samp]] = default
  }

  # for each row,
  # 1. get all matching rows (r_match)
  # 2. remove  all rows containing NA values
  # 3. select the first of these rows, and use the value from that one
  # 4. if the value is NA, use the value already present
  # 5. if there already is a value in the row that's being looked up, only overwrite if overwrite==T

  values = apply(df_samples, MARGIN=1, FUN=function(x){
    s_id = x[[id_samp]]
    r_match = df_lookup %>%
      filter(!!sym(id_lookup)==s_id) %>%
      remoNA(id_lookup)

    # check if this item was found (and is not NA) (and the original is empty or overwrite==T)
    if (nrow(r_match)!=0 & !is.na(r_match[[value_lookup]][1]) & (is.na(x[[value_samp]][1]) | overwrite==T)){
      r_match[[value_lookup]][1] %>% unlist()
    }
    else{
      #if not, use the value already present
      x[[value_samp]][1] %>% unlist()
    }
  })
  message(typeof(values))
  df_samples[[value_samp]] = values
  message("Done!")
  df_samples
}

checkup = function()


#' Applies a function on the column of a dataframe and then returns that dataframe
#'
#' @param df A dataframe
#' @param column The name of the column (string) that we want apply the function to
#' @param fun The function we use on the column
#' @example dataframe2 <- dataframe1 %>% manipulate("lengths",convertInches)
#' @export
manipulate = function(df, column, fun){

  if(length(column) > 1) column <- names(df %>% select(column))

  df[[column]] = fun(df[[column]])
  return(df)
}




#' Look up individual-associated data from one dataset and insert into another.
#'
#' Similar to merge(), but more flexible (and slower). Supports renaming of columns and overwrite-controll.
#' Returns the "to" dataframe with new columns from "from" added.
#'
#' @param to A data frame (x) you want to add data from another set into
#' @param from The data (y) frame you want to obtain this data from
#' @param what Which columns from y to look up into x. Single chr value or vector with multiple values. Will take all columns from y if unspecified.
#' @param by Which column to use for identifying individuals. The function only adds data to columns with matching identifiers.
#' @param by.x If the identifier has a different column name in the "to" dataset, specify it with by.x
#' @param by.y If the identifier has a different column name in the "from" dataset, specify it with by.y
#' @param new_name If you want to rename some columns from y before inserting into x. Must be same length as "what". Use an empty "" for columns to stay the same.
#' @param default If inserted columns doesn't exist yet in x, use this to specify the default value in case some rows can't be looked up
#' @param overwrite If there is already a value in x where one is to be inserted from y, use this to specify if it should be overwritten or not
#' @export
#' @examples all_people = lookup(to=all_people, from=phone_book, what=c("phone_number","adress"), new_name=("mobile", ""), by="name", by.x="Name", default="not found")
lookup = function(to, from, what, by, by.x, by.y, new_name, default=NA, overwrite=F){

  df_x    <- to
  df_y    <- from
  if (missing(by.x)) by.x = by
  if (missing(by.y)) by.y = by

  # if "what" is not defined, make it all the columns in Y (from) except the by column
  if(missing(what)) what=names(from)[!names(from) %in% by.y]
  print(what)
  columns <- what


  if (!missing(new_name)) if (length(new_name) != length(columns)) stop("'new_name' and 'what' must be of same length if 'new_name' is used")

  if (missing(new_name)) new_name = columns
  else
  {
    for (i in 1:length(new_name))
    {
      if (new_name[i]=="") new_name[i]=columns[i]
    }
  }

  # columns_n are the columns where to checkup values go
  columns_n <- new_name
  columns_o <- columns

  # first do a column check, create those that doesn't exist
  for (i in 1:length(columns_n))
  {
    if(!columns_n[i] %in% colnames(df_x))
    {
      df_x[[columns_n[i]]] = default
    }
  }

  # iterate over all rows in x
  for (r in 1:nrow(df_x))
  {
    # currently in a row r
    # check if by.x of this rows matches by.y of any row in y

    rows_y = df_y[df_y[[by.y]] == df_x[[by.x]][r],]
    if (nrow(rows_y) != 0)
    {

      # a match was found!
      # Iterate over all columns_o in y and assign them to column_n in x
      for (clm in 1:length(columns_o) )
      {
        # currently looking at column clm
        # put in df_x, at this column, at the current row, the corresponding value in the matching rows in y
        # but only if overwrite is turned on, or the value in x is NA
        if (overwrite == T || is.na(df_x[[columns_n[clm]]][r]))
        {
          df_x[[columns_n[clm]]][r] = rows_y[[columns_o[clm]]][1]
        }
      }

    }
    # if not, leave be and continue to nect

  }
    # check if its by.x matches any by.y
      # if so, take the values from y (depending on setting of overwrite)
      # if not, leave be
return(df_x)

}







#' perform
#' @keywords internal
perform = function(df, column, fun){
  return(fun(df[[column]]))
}

#' Selects random rows from a dataframe
#'
#' Takes a dataframe and a number n
#' Returns return n randomly selected rows from the dataframe (as a dataframe)
#' @export
selRandom = function(df, n) {
  rows = round(runif(n,0,nrow(df)))
  selection = df[rows,]
  return(selection)
}


manipulate <- function(df, column, fun){
  df[[column]] = fun(df[[column]])
  return(df)
}


#version of message() that uses glue() syntax
messaglue <- function(x){
  require(glue)
  message(glue(x))
}


mergeDuplicates = function(vector)
{
  if(length(vector)>1) {
    print(vector)
    print("----")
  }
  return(vector[1])
}


mergeDuplicates_last = function(vector)
{
  if(length(vector)>1) {
    print(vector)
    print("----")
  }
  return(vector[length(vector)])
}

every_nth <- function(x, nth, empty = TRUE, inverse = TRUE) {
  # Source: User Maninal at Stackoverflow
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    }
    else {
      x[1:nth != 1]
    }
  }
  else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    }
    else {
      x[1:nth == 1]
    }
  }
}

seq_minors=function(from, to, step, majors) {
  sequence = seq(from, to, step)
  return( every_nth(sequence,majors))
}

round_df <- function(x, digits) {
  # https://jeromyanglim.tumblr.com/post/50228877196/round-numbers-in-data-frame-that-contains-non
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}


meanSem = function(var,a=2,b=2){ return(paste(  round(mean(var,na.rm=T),a),round(sem(var),b),sep="+-")) }
meanSD = function(var,a=2,b=2){ return(paste(  round(mean(var,na.rm=T),a),round(sd(var),b),sep="+-")) }


# Functions for returning means pluss minus SD or SE
sem <- function(x) sqrt(var(x, na.rm=T)/nanaLength(x))
nanaLength = function(x) sum(!is.na(x))


#' Function for gaining a list of all dates between two dates
#' sollution to this problem by user "yifyan" at stackoverflow.com
#' https://stackoverflow.com/questions/14450384/create-a-vector-of-all-days-between-two-dates
#' date_a and date_b must be lubridate-date-objects
#' @keywords internal
datesBetween = function(date_a,date_b) {
  require(lubridate)

  n_days <- interval(date_a,date_b)/days(1)
  dates = date_a + days(0:n_days)
  return(dates)
}


#' funkyTranspose
#' https://stackoverflow.com/questions/6645524/what-is-the-best-way-to-transpose-a-data-frame-in-r-and-to-set-one-of-the-column
#' Credits: mortonjt and nzcoops
#' @keywords internal
funkyTranspose = function(df){
  # Transpose table YOU WANT
  df_t <- t(df[,2:ncol(df)])
  # Set the column headings from the first column in the original table
  colnames(df_t) <- t(df[,1])
  return(df_t)
}

