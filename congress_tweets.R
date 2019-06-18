# members of congress on twitter
library(rtweet)

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

# get members from cspan list
members <- rtweet::lists_members(slug = "members-of-congress", owner = "cspan")

# get last 3200 tweets from each member
tweets <- bind_rows(lapply(members$user_id, function(x){
  print(which(members$user_id == x))
  
  limit <- remaining(q = "statuses/user_timeline")
  
  if(limit == 0){
    Sys.sleep(60 * 15 + 10)
  }
  
  t <- rtweet::get_timeline(x, n = 3200)
  return(t)
}))
  
  
  
