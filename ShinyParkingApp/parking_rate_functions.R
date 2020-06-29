library(janitor)
library(tidyverse)
library(stringr)
library(dplyr)
library(lubridate)


#Number of Weeks Function
number_of_weeks <- function (mins, num_days) {
  
  week_count <- 0
  
  while (mins - (num_days*1440) >= 0) {
    mins <- mins - (7*1440)
    week_count <- week_count + 1
  }
  
  return(week_count)
}

num_of_weeks <- Vectorize(number_of_weeks)

#test1 <- num_of_weeks(19, 6)
#test1

#Number of Full Days Function
number_of_full_days <- function (mins, num_days, weekly_rate = 0) {
  
  day_count <- 0
  
  if (weekly_rate != 0) {
    
    week_count <- number_of_weeks(mins, num_days)
    
    mins_minus_weeks <- mins - (week_count*7*1440)
      
    day_count <- max(floor(mins_minus_weeks/1440),0)
      
    
  } else {
    
   day_count <- max(floor(mins/1440),0)
    
  }
  
  return(day_count)
}

num_of_full_days <- Vectorize(number_of_full_days)

#test2 <- number_of_full_days(mins = 1440, num_days = 6, weekly_rate = 200)
#test2

#Calculate Revenue Function
pot_revenue_calc <- function(mins, 
                    gp, 
                    fh_rate, 
                    inc, 
                    inc_rate, 
                    weekly_rate, 
                    num_days, 
                    dmax, 
                    rollover = TRUE) 
  {
  
mins_remain <- mins

pot_revenue <- 0

week_count <- num_of_weeks(mins, num_days)

day_count <- number_of_full_days(mins = mins, num_days = num_days, weekly_rate = weekly_rate)


if (mins <= gp) {
  
  pot_revenue <- 0
  
} else {
  
  if (rollover == TRUE) {
    
    if (weekly_rate != 0) {
      pot_revenue <- weekly_rate*week_count
      mins_remain <- mins_remain - (day_count*7*1440)
      
      pot_revenue <- pot_revenue + (day_count*dmax)
      mins_remain <- mins_remain - (day_count*1440)
      
      if (fh_rate != 0) {
        pot_revenue <- pot_revenue + min((fh_rate + max((ceiling((mins_remain-60)/inc)),0)*inc_rate),dmax)
      } else {
        pot_revenue <- pot_revenue + min(max((mins_remain/inc),0)*inc_rate,dmax)
      }
      
    } else {
      
      pot_revenue <- pot_revenue + (day_count*dmax)
      mins_remain <- mins_remain - (day_count*1440)
      
      if (fh_rate != 0) {
        pot_revenue <- pot_revenue + min((fh_rate + max((ceiling((mins_remain-60)/inc)),0)*inc_rate),dmax)
      } else {
        pot_revenue <- pot_revenue + min((max((mins_remain/inc),0)*inc_rate),dmax)
      }
    }
    
  } else {
    
    if (day_count == 0 & week_count == 0) {
      
      if (fh_rate != 0) {
        
        pot_revenue <- pot_revenue + min((fh_rate + max((ceiling((mins_remain-60)/inc)),0)*inc_rate),dmax)
        
      } else {
        
        pot_revenue <- pot_revenue + min((max((mins_remain/inc),0)*inc_rate),dmax)
        
      }
      
    } else {
      
      if (weekly_rate != 0) {
        
        pot_revenue <- weekly_rate*week_count
        mins_remain <- mins_remain - (week_count*7*1440)
        
        pot_revenue <- pot_revenue + (day_count*dmax)
        mins_remain <- mins_remain - (day_count*1440)
        
        
      } else {
        
        pot_revenue <- pot_revenue + (day_count*dmax)
        mins_remain <- mins_remain - (day_count*1440)
        
      }
      
    }
    
  }
  
}
  
  return(pot_revenue)
}

#test3 <- pot_revenue_calc(mins = 121, gp = 15, fh_rate = 5, inc = 60, inc_rate = 3, weekly_rate = 200, num_days = 5, dmax = 25, rollover = FALSE)
#test3

calc_revenue <- Vectorize(pot_revenue_calc)

#Logical Function for Rate_Type
rate_type_logic <- function(rollover) {
  
  value <- rollover == "rollover"
  
  return(value)
}

right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

