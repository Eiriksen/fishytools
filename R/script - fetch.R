# function for simplified reading of files


fetch = function(filename, skip=0, dec="."){
  require(readxl)
  require(glue)
  require(tools)

  filename <- glue(filename)
  type     <- file_ext(filename)

  if (type == "xlsx" | type == "xls")
  {
    df = read_excel(filename, skip=skip)
  }
  else if (grepl(".csv", filename))
  {
    df = read.csv(filename, head=T, sep=",", dec=dec, skip=skip)
  }
  else
  {
    stop("Filetype not specified or compatible")
    break
  }
  return(df)
}


read.data = fetch


# function for simplified saving of files
save.data = function(df, filename){
  # add .csv, in case it's not there
  filename = filename %>% glue %>% sub(".csv","",.) %>% paste(".csv",sep="")

  write.table(df, file=filename, sep=",", row.names=F)
}

save.data.unique = function(df, filename){
  require(tidyverse)
  require(stringi)
  require(lubridate)

  set.seed(as.numeric(Sys.time()))
  filename_unique = filename %>% sub(".csv","",.) %>% paste(" #",stringi::stri_rand_strings(1,5),".csv",sep="")

  #t=now()
  #filename_unique = filename %>% sub(".csv","",.) %>% paste(glue(" {year(t)}{month(t)}{day(t)}{hour(t)}{minute(t)}{round(second(t))}"),".csv",sep="")

  save.data(df,filename_unique)
}
