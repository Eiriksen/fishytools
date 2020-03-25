#' get_advanced_genotypes
#' Deprecated! Use find_familyInfo instead
#' @keywords internal
get_advanced_genotypes <- function(offspring, families) {
  # Function for this specific pipeline, gives the correct heterozygote phenotype
  # apply following function to all offspring:

  apply(offspring,1,FUN=function(x) {
    vgll3geno = as.numeric(x[["c25_1441_SAC"]])
    fam = as.numeric(x[["family"]])
    pop = x[["population"]]
    if(is.na(vgll3geno)) return("NA")

    if (vgll3geno == 1) {
      return("EE")
    }
    if (vgll3geno == 2) {
      # find the genotype of the mother
      if (is.na(fam)) return("EL/LE")
      row_fam <- families %>% filter(ID_family == fam & Population==pop)
      damVGLL3 = row_fam$Dam.VGLL3
      if (damVGLL3 == "LL") return("LE")
      else if (damVGLL3 == "EE") return("EL")
      else return("EL/LE")
    }
    if (vgll3geno == 3) {
      return("LL")
    }
    return("none of the above")
  })

}

#' findFamily DEPRECATED
#' @keywords internal
findFamily = function(ID_mams, ID_paps) {
  family = unlist(data_families %>% filter(ID_ma == ID_mams & ID_pa == ID_paps) %>% select(ID_family))
  if (family %>% length() == 0) return(NA) else return(family)
}

#' find_familyID
#' subfunction
#' @keywords internal
find_familyID = function(ID_mam, ID_pap, df_families){
  # df_families must be formated as
  # rows pr family
  # columns: ID_ma, ID_pa, ID_family

  family = df_families %>%
    filter(ID_ma == ID_mam & ID_pa == ID_pap) %>%
    select(ID_family) %>%
    unlist()
  if ( length(family) == 0) return(NA) else return(family)
}

#' Obtains family IDs based on known mothers and fathers
#'
#' Works on a dataframe of individuals (rows) including two columns ID_ma and ID_pa, which are the ID's of that individual's parents.
#' \cr
#' Then looks up a dataframe of family info, df_families, with rows as families, and finds the ID's of each individual's family
#' @export
find_familyIDs = function(df, df_families){

  df$ID_family = apply(df,MARGIN=1,FUN=function(x){
    ID_pa = x[["ID_pa"]]
    ID_ma = x[["ID_ma"]]
    find_familyID(ID_ma, ID_pa, df_families)
  })

  df
}

#' get_familyInfo DEPRECATEDW
#' @keywords internal
get_familyInfo = function(df,df_families,columns) {
  for (i in columns){
    df[[i]] = apply(df,MARGIN=1,FUN=function(x){
      ID_fam = x[["ID_family"]]
      df_families %>% filter(ID_family == ID_fam) %>% select(i) %>% as.character() %>% unlist()
    })
  }
  df
}

#' Obtains info about a given family (mother x father) and adds that info to their offspring
#'
#' Works on a dataframe of individuals (rows) including two columns ID_ma and ID_pa, which are the ID's of that individual's parents.
#' For each individual, looks up another dataset of family info to attach family info to that individual (based on ID of its mother and father
#' df_families structure: must include columns ID_ma and ID_pa for each row (family).
#' @export
find_familyInfo = function(df,df_families,columns) {
  for (i in columns){
    message("Looking up ",i,"...")
    df[[i]] = apply(df,MARGIN=1,FUN=function(x){
      ID_ma  = x[["ID_ma"]]
      ID_pa  = x[["ID_pa"]]
      ID_fam = find_familyID(ID_ma,ID_pa,df_families)
      r_family = df_families %>% filter(ID_family == ID_fam)
      if (nrow(r_family)!=0) return(r_family %>% select(i) %>% as.character() %>% unlist())
      else return(NA)
    })
  }
  message("Done!")
  df
}

