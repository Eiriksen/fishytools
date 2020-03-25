#' Random, condition based samplig of a set of individuals
#'
#' Takes a dataframe of individuals (rows) with certain attributes (columns), and samples a given number of individuals from all possible combinations of a given set of paramters
#' For example, can sample one individual of ech combination of sex and age
#'
#' @param df_f The dataframe to work on
#' @param ofEach The parameters we want to use, as a vector of column names, e.g: ofEach=c("sex","age","tank")
#' @param nOfEach How many individuals within each combination of the "ofEach" parameters that should be selected
#' @param noShareWithin one or more sets of paremeters, where unique combinations can't be shared. For example, if we want no more than a single individual from any given family, within a tank, a set would be c("family","tank"), meaning that no individuals can have the same of both family and tank. Supplied within a list, so: list(c("family","tank")). Add more sets within the list if needed, for example: list(c("tank","father"),c("tank","mother)))
#' @param priBy If some individuals are to be prioritized over others, specify the name of the column containing prioritization info. This must ba number, and lower numbers are prioritized. (E.g, individuals with "1" are prioritized over individuals iwth "2")
#' @param useDupl If all individuals that are selected within each combination may be be used, or just one of them.
#' @param checkExternal ...
#' @param identifier ...
#' @export
ransamp = function(df_f, ofEach,nOfEach,noShareWithin,priBy,usedupl=F,checkExternal=F,identifier=""){
  message("Finding individuals_cur...")
  # INTIALIZING:
  df_f$ID_num    = 1:nrow(df_f)
  pri_max        = df_f[[priBy]] %>% max(na.rm=T)
  cases_internal = 0
  cases_external = 0
  cur_comb_ID    = 0
  df_found       = data.frame()
  l_combinations = do.call(crossing,as.list(df_f %>% select(ofEach) %>% na.omit()))


  for (i in 1:nrow(l_combinations)){
  # PER COMBINATION
    df_temp     = data.frame()
    count_found = 0
    pri_cur     = 1
    cur_comb_ID = cur_comb_ID + 1

    combination_cur = l_combinations[i,] %>% as.list()
    individuals_cur = listSelect(df_f %>% filter(pri == pri_cur), combination_cur)



    # CHECK FOR INDIVIDUALS UNTIL WE FOUND THE NUMBER WE'RE LOOKING FOR:
    while(count_found != nOfEach) {
    # PER CHECKED INDIVIDUAL

      if (nrow(individuals_cur)==0) {
      # WE'VE RAN OUT OF INDIVIDUALS IN THIS SELECTION
        if (pri_cur > pri_max){
          # WE'VE RAN OUT OF INVIDIUALS ALTOGETHER FOR THIS COMBINATION
          # JUST STORE THIS ONE AS NA
          df_found    = rbind(df_found,NA)
          count_found = count_found+1
          next()
        }
        #INCREASE THE CURRENT PRIORITY WE'RE SEARCHING FOR
        pri_cur         = pri_cur+1
        individuals_cur = listSelect(df_f %>% filter(pri == pri_cur), combination_cur)

        next()
        }


      # OBTAIN AN INVIDUAL (RANDOMLY) AND REMOVE IT FROM OUR CURRENT SELECTION OF INDIVIDUALS
      # ...BUT FIRST, CHECK IF THIS IS THE FIRST INDIVIDUAL OF THIS COMBINATION OR ONT
      individual_cur_narrowed = individuals_cur
      if (count_found!=0 & usedupl==T){
        # IF NOT, ONE INDIVIDUAL HAS ALREADY BEEN FOUND FOR THIS COMBINATION, AND WE'RE LOOKING FOR A BACKUP
        # TO REDUCE THE NUMBER OF INDIVIDUALS USED FOR THIS SET OF NOSHARE FACTORS, TRY FINDING AN INDIVIDUAL WITH THE SAME SET OF NOSHARE FACTORS FIRST
        for (u in noShareWithin){
          cur_conflictComb = df_temp[1,] %>% select(unlist(u)) %>% as.list()
          individual_cur_narrowed = individual_cur_narrowed %>% listSelect(cur_conflictComb)
        }
      }

      # PICK AN INDIVIDUAL FROM THE (POTENTIALLY NARROWED) SELECTION OF INDIVIDUALS
      ranRow = round(runif(1,1,nrow(individual_cur_narrowed)))
      individual      = individual_cur_narrowed[ranRow,]
      individuals_cur = individuals_cur %>% filter(ID_num != individual$ID_num)


      # CHECK FOR INTERNAL CONFLICT
      conflicts_exist=F
      for (j in noShareWithin){
        cur_conflictComb = individual %>% select(unlist(j)) %>% as.list()
        l_conflicts      = listSelect(df_found, cur_conflictComb)

        if (nrow(l_conflicts) != 0) {
          conflicts_exist = T
          cases_internal=cases_internal+1

        }
      }
      if(conflicts_exist) next()


      # CHECK FOR EXTERNAL CONFLICTS
      if (checkExternal){
        remaining_combinations = l_combinations[c((i+1):nrow(l_combinations)),]
        for (z in noShareWithin){
          cur_conflictComb       = individual %>% select(unlist(z)) %>% as.list()
          remaining_combinations = listSelect(remaining_combinations, cur_conflictComb)
        }

        if (nrow(remaining_combinations) != 0){
          if(get_least_n_options2(df_f %>% filter(pri>=(pri_cur+2)),df_found,remaining_combinations,noShareWithin) <= nOfEach) {
            cases_external = cases_external+1
            message("oi")
            next()
          }
        }
      }

      # NO CONFLICTS! STORE THIS INDIVIDUAL
      # ALSO REMOVE IT FROM THE MASTER LIST OF INDIVIDUALS SO IT CAN'T BE SELECTED AGAIN
      count_found=count_found+1
      individual$duplicate = count_found
      individual$ID_type   = paste("type",cur_comb_ID,identifier,sep="")
      df_f = df_f %>% filter(ID_num != individual$ID_num)


      if(!usedupl) df_temp  = rbind(df_temp, individual)
      else         df_found = rbind(df_found_individual)
    }
    if(!usedupl)df_found=rbind(df_found,df_temp)
  }

  message("All done!")
  message("Internal breaks: ",cases_internal,"\nExternal breaks: ",cases_external)
  message("Use of priorities:")
  print(table(df_found$pri))
  message("Not found: ", sum(is.na(df_found$pri)))

  return(df_found)
}

#get_least_n_options = function(df,ofeach){
#  df %>% group_by(.dots=ofeach) %>%
#    summarize(n=n())%>%
#    na.omit() %>%
#    (function(df){
#      return(min(df$n))
#    })
#}

get_least_n_options2 = function(df_all,df_chosen,combolist,conflictLists)
{
  options = apply(combolist, MARGIN=1, FUN=function(combo){
    combo=combo %>% as.list()
    get_available_options(df_all,df_chosen,combo,conflictLists)
  })
  least = min(options)

  return(least)
}


get_available_options = function(df_all,df_chosen,conditionList,conflictLists){
  # Available individuals
  available_individuals = df_all %>% listSelect(conditionList)
  if(nrow(available_individuals)==0) return(0)
  # Excluding noshares, that is, how many of these are not sharing any conflict l_combinations

  # Iterate over all individual in this individuals_cur and check how many conflicts with the already chosen individuals

  availables = apply(available_individuals, MARGIN=1, FUN=function(indivdl){
    for (conflictList in conflictLists){
        available=T

        indivdl = indivdl %>% as.list() %>% data.frame()
        conflictcombination = indivdl %>% select(unlist(conflictList)) %>% as.list()
        conflicts = df_chosen %>% listSelect(conflictcombination)
        if (nrow(conflicts) != 0) available=F
     }

    if(available) return(T) else return(F)
  })

  return(sum(availables))

}



listSelect = function(df,l,eq=T){
  for (s in 1:length(l))
  {
    col = names(l)[s]
    val = l[s]
    if(col %in% colnames(df)){
      if(eq)  df = df %>% filter(!!sym(col)==val)
      else    df = df %>% filter(!!sym(col)!=val)
    }
  }
  return(df)
}

