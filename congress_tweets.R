# members of congress on twitter
library(rtweet)
library(tidyverse)

# function to check rate limits
check.limits <- function(){
  rl <- rtweet::rate_limits()
  
  # return limits that are less than full allocation
  limits <- rl[rl$limit > rl$remaining,]
  return(limits)
}

# function to return how many requests are left on a given twitter API call
# takes q, the query, and default equal to the limit for that query (for calls with limit > 15, needs to be reset)
remaining <- function(q, default = 15){
  cl <- check.limits()
  
  # if query is returned (i.e. has less than full complement left), return number remaining
  if(q %in% cl$query){
    r <- cl$remaining[cl$query == q]
  }else{
    
    # otherwise return default
    r <- default
  }
  return(r)
}

# get follows
# set next cursor
nc <- "-1"
congress.follows <- bind_rows(lapply(members$screen_name, function(x){
  
  #print progress to give you an idea of how much is left
  print(which(members$screen_name == x))
  
  # get how many pages of follows need to be scraped (only a handful will be more than one)
  rounds <- ceiling(members$friends_count[members$screen_name == x] / 5000)
  
  # set next cursor
  nc <<- "-1"
  
  # get friends
  friends <- lapply(1:rounds, function(y){
    # get number of get_friends() calls left before rate limit
    limit <- remaining(q = "friends/ids")
    
    # if at the limit, wait 15 minutes to refresh (this is the slow part)
    if(limit == 0){
      Sys.sleep(60 * 15 + 10)
    }
    
    # get friends on that page
    page <- rtweet::get_friends(x, page = nc)
    
    nc <<- rtweet::next_cursor(page)
    
    return(page)
  })
  
  # stack pages on top of each other if necessary
  fdat <- unique(bind_rows(friends))
  
  return(fdat)
}))


# get members from cspan list
members <- rtweet::lists_members(slug = "members-of-congress", owner = "cspan")

# make sure you have a full complement of get_timeline() calls
check.limits()

# get last 3200 tweets from each member
tweets <- bind_rows(lapply(members$user_id, function(x){
  
  # keeps track of progress to give you an idea of how long this will take
  print(which(members$user_id == x))
  
  # get tweets
  t <- rtweet::get_timeline(x, n = 3200)
  return(t)
}))



