#Function for getting the mean temperature of a time period
#Dates must be supplied as a lubridate time object
tempData_mean = function(df_temp, startDate, endDate, column) {
  if(is.na(startDate) | is.na(endDate) | is.na(column)) return(NA)
  return(
    unlist(df_temp[column])[df_temp$date < endDate & df_temp$date > startDate] %>% mean(na.rm=T)
  )
}


#extends a temperature dataset using temperature data from previous year
#df_temp has columnds "date", as well as those specified by the parameter "columns"  that contains temperatures, for-
#example from different channels
#parameter "date" is the date of etension
tempData_extend = function(df_temp, date, columns){
  date=dmy(date)
  # get the last date of the temperature dataset
  date_last <- df_temp$date[nrow(df_temp)]
  message("Last recorded date is: ", date_last)

  # get the days between that date and the extension date and make this into a dataframe (rows being dates)
  betweenDates <- datesBetween(date, date_last)
  df_new <- data.frame(date=betweenDates)
  df_new2<- data.frame(date=betweenDates)
  # for each column we want to extend:
  for (i in columns){
    # apply over the list of days, return the temperature of that date one year ago
    df_new2[[i]] = apply(df_new,MARGIN=1,FUN=function(j){
      day = ymd(j[["date"]])
      day_prev = df_temp %>% filter(date == (day-years(1)))
      temp_prev = day_prev[[i]]
      return(temp_prev)
    })
  }

  for (i in columns){
    df_new[[i]] = as.numeric(df_new2[[i]])
  }

  message("Data extended to ",date)
  # rbind the new dataset to the old one
  return(bind_rows(df_temp,df_new))
}


# calculates thermal growth coefficient (from Jobling: The thermal growth coefficient (TGC) model of fish growth: a cautionary note)
calc_TGC = function(W0,W1,temp,time) {
  time = as.numeric(time)
  TGC = (((W1^(1/3))-(W0^(1/3)))/(temp*time)*1000)
}


#' Calculates Thermal Growth Coefficient TGC
#' @keywords internal
calc_TGC_df <- function(df_fish,df_temp,period)
{
  time <- ymd(df_fish[[glue("date.{period}")]]) - ymd(df_fish[[glue("date.{period-1}")]])
  W0   <- df_fish[[glue("weight.{period-1}")]]
  W1   <- df_fish[[glue("weight.{period}")]]
  temp <- mapply(tempData_mean,
                startDate= ymd(df_fish[[glue("date.{period-1}")]]),
                endDate  = ymd(df_fish[[glue("date.{period}")]]),
                column   = df_fish$temp,
                MoreArgs = list(df_temp=df_temp))

  TGC <- calc_TGC(W0, W1, temp, time)
}


# predicts growth based on TGC, same model as above
calc_growth_TGC <- function(W0, TGC, temp, time)
{
  time <- as.numeric(time)
  growth <- as.numeric((W0^(1/3)+((TGC/1000)*(temp*time)) )^3)
  return(growth)
}


# Based on the input of a list of weights (the weights of all fishes in a group)
# Returns table with the different feed sizes and their respective distributions in the feed
calc_feedSizeDist = function(weights)
{
  tabl = case_when(weights <= 15 ~ 1.2,
                   weights > 15  & weights <= 30  ~ 1.7,
                   weights > 30  & weights <= 70  ~ 2.5,
                   weights > 70  & weights <= 125 ~ 3.5,
                   weights > 110 & weights <= 500 ~ 5.0,
                   weights > 500 & weights <=2300 ~ 7.0,
                   weights > 2300 ~ 100,
                   TRUE ~ NaN)
  return(tabl)
}


# uses the same function as above, but returns results as percentage for each feed size
calc_feedSizePercentage = function(weights) {
  weightClasses = calc_feedSizeDist(weights)
  dat = data.frame(weightClasses)
  totalLength = length(dat$weightClasses[!is.na(dat$weightClasses)])

  return(
    dat %>% group_by(weightClasses) %>% summarise( perc = length(weightClasses)/totalLength*100 )
  )
}


calc_feedSizePercentage_special = function(weights,prefix) {
  weightClasses = paste(prefix,calc_feedSizeDist(weights),sep="")
  dat = data.frame(weightClasses)
  totalLength = length(dat$weightClasses[!is.na(dat$weightClasses)])

  return(
    dat %>% group_by(weightClasses) %>% summarise( perc = length(weightClasses)/totalLength*100 )
  )
}


# calculates the total amount of feed given between two dates
# internal function
feedData_amount_fed = function(dates, feed, date1, date2) {
  return(
    sum( feed[ dates > date1 & dates < date2], na.rm=T )
  )
}


# calculates the amount of feed given a specific tank between two dates
feedData_feedGiven = function(df_feed,tankID, date1, date2) {
  data_feed_tank = df_feed[which(df_feed$tank == tankID),]
  amount = feedData_amount_fed(data_feed_tank$date, data_feed_tank$feed, date1, date2)
  return(amount)
}





# function for converting the feed datasat to a "pr-day" type dataset
feedData_convert_feedPrDate = function(df_feed,tank,date) {
  #BE AWARE: this function does not work well for the very last period of feeding

  #find the last date where this tank was "fed"
  prevDates = df_feed$date[df_feed$date <= date & df_feed[tank] != 0]
  prevDate  = max(prevDates,na.rm=T)
  #get the next date where this tank is being "fed"
  comingDates = df_feed$date[df_feed$date > date]
  if (length(na.omit(comingDates))==0) return(NA)
  nextDate = min(comingDates,na.rm=T)

  #get the amount of feed at the intial date
  amount = as.numeric(df_feed[tank][df_feed$date == prevDate,1])

  #get the number of days between last and next date
  numDays = as.numeric(nextDate-prevDate)

  #divide amount fed by number of dates
  dailyAmount = amount/numDays
  return(dailyAmount[1])
}

#' predict_weights_df
#'
#' @keywords internal
predict_weights_df <- function(df_fish, df_temp, dates, startPeriod=999, tgc_p_modifier=1, temp_abs_modifier=0){
  message("Predicting weights...")
  # obtain all the periods so far as a vector (of numbers)
  periods <- ft_get_periods(df_fish)
  # remove periods that are after the specified startPeriod
  periods <- periods[periods <= startPeriod]


  # calculate TGC for all used periods
  tgc <- as.data.frame(sapply(periods[2:length(periods)], FUN=function(i){
    calc_TGC_df(df_fish,df_temp,i)
  }))
  message("TGC calculated from periods: ",paste(periods[2:length(periods)],sep=", "))


  # create one new tgc that is the mean of these TGC values, but weighting the second last and last one 2 and three timess
  tgc_weighted <- apply(tgc,1,FUN=function(x){
    x=na.omit(x)
    if(length(x)==0) return(NA)
    weighted.mean(x,c(rep(1,length(x)-1),5),)
  })

  tgc_weighted = tgc_weighted*tgc_p_modifier
  tgc_weighted[tgc_weighted<0]=0

  # the next period:
  period_cur = max(periods+1)

  # for each date, record the predicted weight
  for ( date in as.list(dates)){
    #get the last recorded date (for the last recorded date)
    date_cur   <- glue("date.{max(periods)}")
    #calculate the mean temp from the last real period until the new date
    temp = mapply(
      tempData_mean,
      startDate = ymd(df_fish[[date_cur]]),
      endDate   = date,
      column    = df_fish$temp,
      MoreArgs  = list(df_temp=df_temp)) + temp_abs_modifier

    #find the last recorded weight of the fish
    W0 <- df_fish[[glue("weight.{max(periods)}")]]
    #create new column for the weight and date of the fish
    weight_new <- glue("weight.{period_cur}")
    date_new   <- glue("date.{period_cur}")
    #predict the growth for the newdate
    df_fish[[date_new]]   <- date
    df_fish[[weight_new]] <- calc_growth_TGC(
      W0,
      TGC  = tgc_weighted,
      time = date-ymd(df_fish[[date_cur]]),
      temp = temp)

    message(glue("Weights predicted for {weight_new}"))

    period_cur = period_cur+1
  }


  return(df_fish)
}



# write_size and feed table
write_size_and_feed_table = function(df_fish,df_temp,period_start)
{
  df_results <- data.frame()
  periods <- ft_get_periods(df_fish)

  for (period in periods[period_start:length(periods)])
  {

    df_fish_c = df_fish %>% filter(temp=="cold")
    df_fish_w = df_fish %>% filter(temp=="hot")
    proportions_feed_w <- df_fish_w[[glue("weight.{period}")]] %>% na.omit() %>% calc_feedSizePercentage_special("W.") %>% funkyTranspose()
    proportions_feed_c <- df_fish_c[[glue("weight.{period}")]] %>% na.omit() %>% calc_feedSizePercentage_special("C.") %>%funkyTranspose()

    tank_biomass     <- data.frame(tank=sort(unique(df_fish$tank.2))) %>% na.omit()
    tank_biomass$biomass <- apply(tank_biomass,1,FUN=function(x){
                                   tank_name <- x[["tank"]]
                                   tank_this <- df_fish[df_fish$tank.2==tank_name,]
                                   mean_mass <- tank_this[[glue("weight.{period}")]] %>% na.omit() %>% mean(na.rm=T)
                                   return(mean_mass)
                                  })
    tank_biomass <- tank_biomass %>% funkyTranspose()

    week             <- data.frame(week=week(min(df_fish[[glue("date.{period}")]])))

    row              <- cbind(week,proportions_feed_c,proportions_feed_w,tank_biomass)
    df_results <- df_results %>% bind_rows(row) %>% round_df(2)
  }

  filename = glue("table - predicted biomass and feed size.csv")
  write.table(df_results, filename, sep=",", row.names=F)
  message("Table saved to ",filename)
  return(df_results)

}


# Takes a wide-format dataset of individuals, with period-associated measurements denoted by dot-number format, e.g:
# Weight.1, weight.2, weight.3 or length.1 length.2 or date.1, date2. etc
# Returns a numeric vector of all the periods in that dataset eg: c(1,2,3,4,5) for a dataset with 5 periods
ft_get_periods <- function(df_fish){
  require(tidyverse)

  periods <- colnames(df_fish) %>%
    str_extract("weight[.][0-9]*") %>%
    sub("weight.","",.) %>%
    numextract() %>%
    na.omit() %>%
    unique()

  return(periods)
}



# Calculated the FCR for all specified tanks summed for a all periods in the given dataset
# returns a dataframe with columns "period" and "FCR"
# NOTE NOTE NOTE: The amount of feed eaten is likely overestimated (because the fish don't eat all the food given to them)
#                 And so the FCR is likely underestimated
#                 Using an underestimated FCR when predicting growth means you think the fish will need more feed than they really do,
#                 worst case scenario is then just that you order a bit too much feed.
#                 Anyways, to correct for this, the feed given is multiplied with a correcting constant
calc_FCR_df <- function(df_fish, df_feed, tanks, consumption_correction=1){
  # obtain all the periods so far as a vector (of numbers)
  df_FCR = data.frame(period=periods)

  #reduce the df_fish set
  df_fish <- df_fish %>% filter(tank.2 %in% tanks)

  # for each period:
  df_FCR$FCR<- lapply(periods, FUN=function(period){

    # skip the first period
    if(period==1) return(NA)

    # obtain the dates for this period
    date_start <- min(df_fish[[glue("date.{period-1}")]],na.rm=T)
    date_end   <- min(df_fish[[glue("date.{period}")]],na.rm=T)

    # To calculate FCR:
    # for all fish, the FCR is:
    # the total biomass gained
    # divided by the total feed fed during that period

    biomass_gained <- sum(df_fish[[glue("weight.{period}")]],na.rm=T) - sum(df_fish[[glue("weight.{period-1}")]],na.rm=T)
    feed_fed       <- sum(sapply(
                                 tanks,
                                 FUN=function(tank){return(feedData_feedGiven(df_feed,tank,date_start,date_end))}
                                 )
                          ,na.rm=t)
    feed_fed       <- feed_fed * consumption_correction

    # Return the FCR for this period so its added to df_FCR
    FCR <- biomass_gained/feed_fed
    return(FCR)
  })

  return(df_FCR)
}


calc_feedNeeds = function(df_fish, period_start, period_end){
  # Create binnies to dump food into
  binnies = data.frame("period"=0, "size_1p2"=0, "size_1p7"=0, "size_2p5"=0, "size_3p5"=0, "size_5"=0, "size_7"=0)
  # Fill bins function
  fillBins = function(bins, bin, amount, identifier){
    # (Neccessary function for compiling feed data to binnies
    #)
    # Function that uses dataframe with columns (bins) corresponding to each feed size pr week
    # - this function then reads data about feed consumption for a list of fishsh (feed)
    # - as well as data about which size each fish uses (bin)
    # - and creates a row for that week with the appropriate amonut of food in each bin, t
    # - and then adds that row to the dataframe that was supplied to it
    size_1p2 = sum(amount[bin==1.2], na.rm=T)
    size_1p7 = sum(amount[bin==1.7], na.rm=T)
    size_2p5 = sum(amount[bin==2.5], na.rm=T)
    size_3p5 = sum(amount[bin==3.5], na.rm=T)
    size_5   = sum(amount[bin==5], na.rm=T)
    size_7   = sum(amount[bin==7], na.rm=T)

    bins = rbind(bins, c(identifier, size_1p2, size_1p7, size_2p5, size_3p5, size_5, size_7))
    return(bins)
  }

  for (i in period_start:period_end){
    #for each period:
    #figure out how much the fishes grew since last period
    growth = df_fish[paste("weight.",i,sep="")]-df_fish[paste("weight.",i-1,sep="")]
    print(min(growth,na.rm=T))
    #Using their FCR, figure out how much feed they needed for that period
    feed_amount = growth/df_fish$FCR
    #Using their size, find the size of feed they needed for that period
    feed_size <- calc_feedSizeDist(df_fish[paste("weight.",i-1,sep="")] )
    #add the feed for all the fish for that week to binnies
    binnies <- fillBins(binnies, unlist(feed_size), unlist(feed_amount), i-1)
  }

  # Return the resulting dataset for fishes, feed, and the summary of feed needs.
  return(list(
    "feed"      = binnies,
    "summary"   = list(
      "Size_1p2"=sum(binnies$size_1p2)/1000,
      "Size_1p7"=sum(binnies$size_1p7)/1000,
      "Size_2p5"=sum(binnies$size_2p5)/1000,
      "Size_3p5"=sum(binnies$size_3p5)/1000,
      "Size_5"  =sum(binnies$size_5)/1000,
      "Size_7"  =sum(binnies$size_7)/1000)
  ))
}


