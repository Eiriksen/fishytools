#' Calculate the genetic similarity between two individuals (based on SNPs)
#'
#' takes two vectors of SNPs,
#' Each element in the vector should be the genotypes of one SNP, written as bases, e.g "CT" "CC" "AT" etc. Order, e.g. "CT" or "TC" is not taken into account.
#' It's important that the order of SNPs in the two vectors are the same.
#' Returns an integer between 1 and 0 (0 = 0% match, 1=100% match)
#' @param SNPs1 the first vector of SNPs
#' @param SNPs2 the second vector of SNPs
#' @keywords internal
calc_genetic_similarity = function(sample1, sample2,cutoff_NA_collective=50){
  require(tidyverse)
  require(stringr)

  #Error checking: similar length

  # For easier processing using sapply
  df = data.frame(sample1=unlist(sample1),sample2=unlist(sample2))



  # Return NA if number of missing SNPS is large
  if (sum(is.na(df)) > cutoff_NA_collective) return(0)

  #iterate over each element in the vector
  #check how many alleles they have in common, (CT TC = 4, two matches,  CT CC = two matches, CC TT = nomatches), then store that number
  matches = apply(df,1,FUN=function(x){
    snp1 = x[["sample1"]]
    snp2 = x[["sample2"]]

    # Heart of this function
    m = count_matches(snp1,snp2)
    m
  })
  # Return the proportion of matches vs total potential matches
  sum(matches)/(length(df$sample1)*2)
}

#' subfunction of calc_genetic_similarity
#' @keywords internal
count_matches = function(snp1,snp2) {
  m=0
  if (is.na(snp1) | is.na(snp2)){
    gnt=NA
    if(!is.na(snp1)) gnt = snp1
    if(!is.na(snp2)) gnt = snp2
    if(is.na(gnt)) m=1.25
    else if(substr(gnt,1,1)==substr(gnt,2,2)) m=1 else m=1.5
    #logic:
    # Both snps NA: avg matches is 1.25
    # Only one snip NA, the other heterozygous: avg matches = 1.5
    # ONly one snip NA, the other homozygous: avg matches = 1
    m
  }
  else {
    if( substr(snp1,1,1)==substr(snp2,1,1)) m=m+1
    if( substr(snp1,2,2)==substr(snp2,2,2)) m=m+1
    m
  }
}


#' calculate_similarities_to_sample
#'
#' Using the calc_genetic_similarity function, takes a table of genetic samples (rows: samples, columns: snp genotypes) and calculates
#' @param sample the sample to find closest match to
#' @param lookup a dataframe of samples with SNP data (Each row one sample, each column a snp loki). First row must be ID info.
#' @keywords internal
calculate_similarities_to_sample = function(sample, lookup, cutoff_NA_collective){ #for one vs one
  require(tidyverse)

  ID_sample = sample[["ID"]]
  sample = unlist(sample[2:length(sample)])

  result =apply(lookup,1,FUN=function(x){

    ID = x[1]
    SNPs = unlist(x[2:length(x)])
    similarity <- calc_genetic_similarity(sample,SNPs,cutoff_NA_collective=cutoff_NA_collective)
    return(list(ID_sample=ID_sample, ID_match=ID, similarity=similarity))
  })

  result = result %>%
    map(as.data.frame) %>%
    bind_rows() %>%
    select(ID_sample, ID_match, similarity)

  return(result)
}

#' Creates a genetic similarity matrix
#'
#' Compares two dataframes with SNP data (rows=individuals, columns=snp genotypes) and  finds the genetics similarity
#' between all individuals in set a and b (total checkups is nrow(samples) x nrow(lookup)). Returns a dataframe that contains
#' all possible combinations between individuals of the sample set and the lookup set together with the similarity between them.
#' Output is then: ID_sample (A given individual from the sample set), ID_match (which individual from lookup it was check against) and similarity (a number from 1 to 0 where 1 is a perfect match)
#'
#' @param df_samples A dataframe with individuals (rows) and snp genotypces (columns). Must contain a column "ID" that gives each individual a unique ID
#' @param df_lookup The dataframe the df_sample is looked up against. Must be formated like df_samples
#' @export
create_similarityMatrix = function(df_samples, df_lookup, cutoff_NA_collective){
  require(pbapply)
  require(tidyverse)
  results = data.frame(ID_sample=c(),ID_match=c(),similarity=c())

  message("Calculating similarity matrix...")
  res = pbapply(df_samples, MARGIN=1, FUN=function(x){
    calculate_similarities_to_sample(x, df_lookup, cutoff_NA_collective)

  })
  message("Done!")
  return(bind_rows(res))
}

#parellelized version
create_similarityMatrix_par = function(df_samples, df_lookup,cutoff_NA_collective){
  require(pbapply)
  require(tidyverse)
  require(paralell)
  require(glue)

  results = data.frame(ID_sample=c(),ID_match=c(),similarity=c())

  n_cores = detectCores()
  message(glue("Using {n_cores} cores."))

  clust = makeCluster(n_cores)
  clusterExport(clust, varlist=c("df_samples","df_lookup","calculate_similarities_to_sample","calc_genetic_similarity","count_matches"), envir=environment())

  message("Calculating similarity matrix...")
  res = pbapply(df_samples, MARGIN=1, cl=clust, FUN=function(x){
    calculate_similarities_to_sample(x, df_lookup, cutoff_NA_collective)

  })
  message("Done!")
  return(bind_rows(res))
}


#' Finds the closest genetic matches to a set of individuals, checked up against another set of individuals. SNP based.
#'
#' Finds and assigns matches between two dataframe of genetic samples (SNPs and their genotypes)
#' Both dataframes must be formated as:\cr Rows=individuals/samples \cr Column=SNP genotypes. Spelled as bases, e.g. "AB", "CT", "CC" etc. \cr Must contain a column "ID" with a unique identifier for each row.
#' \cr\cr
#' Returns df_samples with an additional column, "ID_match", which is the closest match of that individual
#' \cr\cr
#' Needs to create a similarity matrix to operate. This is time consuming, and if you have already created one you can refer to it using the parameter similarity_matrix.
#' After creating the similarity matrix, the function saves the matrix as a csv file for use later.
#'
#' @param df_samples Dataframe with all samples to be lookup up
#' @param df_lookup Dataframe with samples to be looked up against (can be larger than df_samples)
#' @param similarity_matrix A similarity matrix is created automatically, but if you already have made a similarity matrix (see function create_similarityMatrix()) you can refer to it here and save some time.
#' @param cutoff_similarity Any match with a similarity (0 to 1) lower than this is set as NA
#' @param resolve_conflicts If conflicts (samples with the same match) should be resolved
#' @export
assign_closest_matches = function(df_samples, df_lookup, project="-",similarity_matrix,cutoff_similarity=0.9,cutoff_NA_collective=50, conflicts_resolve=T){

  matrix_name = glue("data - {project} - similarity matrix.csv")

  if(missing(similarity_matrix)){
    message("No similarity matrix supplied, checking if one exists for this project...")
    if(file.exists(matrix_name)){
      message(glue("Similarity matrix found (\"{matrix_name}\"). Using this one."))
      similarity_matrix = read.table(matrix_name, sep=",",head=T)
    }
    else {
    message("None found, starting create_similarityMatrix()...")
    similarity_matrix = create_similarityMatrix(df_samples, df_lookup,cutoff_NA_collective)
    write.table(similarity_matrix, matrix_name, sep=",", row.names=F)
    message(glue("Done. Matrix saved in working directory as '{matrix_name}"))
    }
  }

  message("Finding closest matches...")
  df_samples$ID_match = apply(df_samples, MARGIN=1, FUN=function(x){
    ID=x[["ID"]]
    matches = similarity_matrix %>% filter(ID_sample == ID)
    closest = matches[which.max(matches$similarity),]
    if (closest$similarity < cutoff_similarity)
      return(NA)
      else return(closest$ID_match)
  })
  message("Done!")

  if (conflicts_resolve) df_samples = df_samples %>% resolve_conflicts(similarity_matrix)

  df_samples
}

#' resolve_conflicts
#'
#' subfunction of assign_closest_matches
#' @keywords internal
resolve_conflicts = function(df_samples, df_similarities){

  shave_conflicts = function(df_samples, df_similarities, choice){

    conflicts    = get_conflicts(df_samples)
    totalChoices = length(unique(df_similarities$ID_match))

    for(i in conflicts$.){
      conflict_samples = df_samples %>% filter(ID_match == i)

      conflict_samples$similarity = apply(conflict_samples, MARGIN=1, FUN=function(x){
        ID=x[["ID"]]
        match = df_similarities %>% filter(ID_sample == ID)
        closest = match[which.max(match$similarity),]
        closest$similarity

      })

      conflict_samples = conflict_samples %>% arrange(desc(similarity)) %>% slice(2:n())

      for(ID in conflict_samples$ID){
        if(choice > totalChoices){
          df_samples$ID_match[which(df_samples$ID == ID)] = NA
        }
        else{
          nextbest = df_similarities %>%
                     filter(ID_sample==ID) %>%
                     arrange(desc(similarity)) %>%
                     slice(choice:choice)
          df_samples$ID_match[which(df_samples$ID == ID)] = nextbest$ID_match
        }

      }
    }

    return(df_samples)
  }

  get_conflicts = function(df_samples){
    conflicts = df_samples$ID_match %>%
      table() %>%
      as.data.frame %>%
      filter(Freq > 1)

    conflicts
  }

  totalChoices = length(unique(df_similarities$ID_match))
  choice=1
  while (nrow(get_conflicts(df_samples)) > 0){
    df_samples = df_samples %>% shave_conflicts(df_similarities,choice)
    choice = choice + 1
    if(choice > totalChoices) break()
  }

  message("Conflicts resolved  (down to choice nr.", (choice-1),")")

  return(df_samples)
}



assign_closest_matches_grouped = function(df_samples, df_lookup, name_group, cutoff_similarity=0.9, cutoff_NA_collective=50, conflicts_resolve=T){

  df_results = data.frame()

  #basically, divide the two dataset into x groups based on unique categories in the grouping variable
  #then, for each grouping variable
    #match the individuals, and rbind the results to a new datafrae

  groups = df_samples[[name_group]] %>% unique() %>% na.omit()

  for(group in groups){
    message(glue("working on group {group}"))
    df_samples_g = df_samples[df_samples[[name_group]]==group,]
    df_lookup_g  = df_lookup[df_lookup[[name_group]]==group,]

    matches <- assign_closest_matches(df_samples_g, df_lookup_g, project=glue("submatrix {group}"), cutoff_similarity=cutoff_similarity, cutoff_NA_collective=cutoff_NA_collective, conflicts_resolve=conflicts_resolve)

    df_results <- rbind(df_results, matches)

  }

  return(df_results)


}
