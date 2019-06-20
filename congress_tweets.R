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

### get tweets ###
# get members from cspan list
members <- rtweet::lists_members(slug = "members-of-congress", owner = "cspan")


## bonus handles for reps who have significant personal accounts alongside official accounts
## bonus handles included if they have some combination of more followers than official accounts, 
#       more tweets than official acount, or aren't clearly run from the campaign side
bonus.handles <- data.frame(usefulhandle = c("RogerMarshallMD","RobBishopUT","AOC",
                                             "votetimmons","MichaelJCloud","gregsteube",
                                             "DevinNunes","DanCrenshawTX","Malinowski","RussFulcher",
                                             "lucymcbath","gregstantonaz","timburchett",
                                             "anthonygonzalez","DrKimSchrier","ConorLambPA",
                                             "ElissaSlotkin","PeteStauber","MittRomney",
                                             "Abby4Iowa","DustyJohnson","JimHagedornMN",
                                             "Troy_Balderson","LaurenUnderwood","GregPenceIN",
                                             "RashidaTlaib","Armstrong_ND","davidjtrone",
                                             "Liz_Cheney","AndyKimNJ","ColinAllredTX",
                                             "JoshHarder","HarleyRouda","SenGillibrand",
                                             "ChuyForCongress","MikeLevinCA","HawleyMO","deanbphillips",
                                             "AngieCraigMN","MaxRose4NY","JoeCunninghamSC",
                                             "sharicedavids","Lizzie4Congress","katieporteroc",
                                             "GilCisnerosCA","ABrindisiNY","DelgadoforNY19",
                                             "SeanCasten","michaelgwaltz","DrMarkGreen4TN",
                                             "Ann_Kirkpatrick","LaCongresista","HaleyLive",
                                             "ChrisPappasNH","JasonCrowCO6","wildforcongress",
                                             "cindyhydesmith","IlhanMN","JenniferWexton",
                                             "DougJones","KatieHill4CA","AyannaPressley",
                                             "DonnaShalala","chiproytx","BenSasse","SpanbergerVA07",
                                             "LisaBRochester","SusieLeeNV","DebbieLesko","JoeNeguse",
                                             "Deb4CongressNM","BillCassidy","marygayscanlon",
                                             "DebbieforFL","vgescobar","StevenHorsford","DarrenSoto",
                                             "Donald_McEachin","Schneider4IL10","DonJBacon",
                                             "PeteOlson","CharlieCrist","leezeldin",
                                             "standwithbarry","TinaSmithMN","JaredHuffman",
                                             "CedricRichmond","MarkMeadows","benraylujan",
                                             "JimmyGomezCA","joniernst","stevestivers","JasonSmithMO",
                                             "StevenPalazzo","mattgaetz","Nanette4CA","MarkDeSaulnier",
                                             "kevincramer","Perduesenate",
                                             "Douglas_Collins","MarcVeasey","BenMcAdams",
                                             "EliseStefanik","TomGravesGA14",
                                             "ThomTillis","BobCorker","TulsiGabbard","stabenow",
                                             "Marcy_Kaptur","Grace4NY","RubenGallego",
                                             "CheriBustos","CoryGardner","alanlowenthal","JuliaBrownley",
                                             "PatToomey","tedlieu","KarenBassTweets","Jenniffer2012",
                                             "KatherineClark","AdamSchiff","SuzanDelBene","bradwenstrup",
                                             "Sewell4Congress","DrLarryBucshon","andybiggs4az",
                                             "RepSpeier","spmaloney","tammyduckworth","MarkPocan",
                                             "JerryNadler","JudgeJohnCarter","dankildee","billhuizenga",
                                             "AlmaforCongress","Duffy4Wisconsin",
                                             "TimRyan","auctnr1","TedDeutch","TomCottonAR",
                                             "larsenrick","McGovernMA","DrPaulGosar","JeffMerkley",
                                             "ScottPetersSD","BenCardinforMD","CarolynBMaloney",
                                             "tedcruz","ericswalwell","kyrstensinema","DickDurbin",
                                             "BradleyByrne","PramilaJayapal"),
                            officialhandle = c("RepMarshall","RepRobBishop","RepAOC","reptimmons",
                                               "RepCloudTX","RepGregSteube","RepDevinNunes",
                                               "RepDanCrenshaw","RepMalinowski","RepRussFulcher",
                                               "RepLucyMcBath","RepGregStanton","RepTimBurchett",
                                               "RepAGonzalez","RepKimSchrier","RepConorLamb",
                                               "RepSlotkin","RepPeteStauber","SenatorRomney",
                                               "RepFinkenauer","RepDustyJohnson","RepHagedorn",
                                               "RepBalderson","RepUnderwood","RepGregPence",
                                               "RepRashida","RepArmstrongND","RepDavidTrone",
                                               "RepLizCheney","RepAndyKimNJ","RepColinAllred",
                                               "RepJoshHarder","RepHarley",
                                               "gillibrandny","RepChuyGarcia","RepMikeLevin",
                                               "SenHawleyPress","RepDeanPhillips","RepAngieCraig",
                                               "RepMaxRose","RepCunningham","RepDavids",
                                               "RepFletcher","RepKatiePorter","RepGilCisneros",
                                               "RepBrindisi","repdelgado","RepCasten",
                                               "RepMichaelWaltz","RepMarkGreen","RepKirkpatrick",
                                               "RepSylviaGarcia","RepHaleyStevens","RepChrisPappas",
                                               "RepJasonCrow","RepSusanWild","SenHydeSmith",
                                               "Ilhan","RepWexton","SenDougJones",
                                               "RepKatieHill","RepPressley","RepShalala",
                                               "RepChipRoy","SenSasse","RepSpanberger",
                                               "RepLBR","RepSusieLee","RepDLesko","RepJoeNeguse",
                                               "RepDebHaaland","SenBillCassidy","RepMGS",
                                               "RepDMP","RepEscobar","RepHorsford","RepDarrenSoto",
                                               "RepMcEachin","RepSchneider","RepDonBacon",
                                               "RepPeteOlson","RepCharlieCrist","RepLeeZeldin",
                                               "RepLoudermilk","SenTinaSmith","RepHuffman",
                                               "RepRichmond","RepMarkMeadows","repbenraylujan",
                                               "RepJimmyGomez","SenJoniErnst","RepSteveStivers",
                                               "RepJasonSmith","CongPalazzo","RepMattGaetz",
                                               "RepBarragan","RepDeSaulnier","SenKevinCramer",
                                               "sendavidperdue","RepDougCollins",
                                               "RepVeasey","RepBenMcAdams","RepStefanik",
                                               "RepTomGraves",
                                               "SenThomTillis","SenBobCorker","TulsiPress",
                                               "SenStabenow","RepMarcyKaptur",
                                               "RepGraceMeng","RepRubenGallego","RepCheri",
                                               "SenCoryGardner","RepLowenthal","RepBrownley",
                                               "SenToomey","RepTedLieu","RepKarenBass","RepJenniffer",
                                               "RepKClark","RepAdamSchiff","RepDelBene",
                                               "RepBradWenstrup","RepTerriSewell",
                                               "RepLarryBucshon","RepAndyBiggsAZ",
                                               "RepSpeier","RepSeanMaloney","SenDuckworth",
                                               "repmarkpocan","RepJerryNadler","JudgeCarter",
                                               "RepDanKildee","RepHuizenga",
                                               "RepAdams","RepSeanDuffy","RepTimRyan",
                                               "USRepLong","RepTedDeutch","SenTomCotton",
                                               "RepRickLarsen","RepMcGovern","RepGosar",
                                               "SenJeffMerkley","RepScottPeters",
                                               "SenatorCardin","RepMaloney","SenTedCruz",
                                               "RepSwalwell","SenatorSinema","SenatorDurbin",
                                               "RepByrne","RepJayapal")
)
bonus.handles <- bonus.handles %>% left_join(members[,c("screen_name","name")], by = c("officialhandle" = "screen_name"))

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

extratweets <-  bind_rows(lapply(as.character(bonus.handles$usefulhandle), function(x){
  
  # keeps track of progress to give you an idea of how long this will take
  print(which(bonus.handles$usefulhandle == x))
  
  # get tweets
  t <- rtweet::get_timeline(x, n = 3200)
  return(t)
}))


### get follows ###
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

extra.users <- rtweet::lookup_users(as.character(bonus.handles$usefulhandle))

# repeat for bonus handles
nc <- "-1"
extrafollows <-  bind_rows(lapply(extra.users$screen_name, function(x){
  
  #print progress to give you an idea of how much is left
  print(which(extra.users$screen_name == x))
  
  # get how many pages of follows need to be scraped (only a handful will be more than one)
  rounds <- ceiling(extra.users$friends_count[extra.users$screen_name == x] / 5000)
  
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

# send to character
bonus.handles$usefulhandle <- as.character(bonus.handles$usefulhandle)
bonus.handles$officialhandle <- as.character(bonus.handles$officialhandle)

bonus.handles$usefulhandle[bonus.handles$officialhandle == "RepLowenthal"] <- "alanlowenthal"

ntweets.official <- 
  tweets %>% filter(created_at > "2019-01-01 00:00:00 UTC") %>%
  group_by(name) %>% summarise(n = n())
ntweets.bonus <- 
  extratweets %>% filter(created_at > "2019-01-01 00:00:00 UTC") %>%
  group_by(screen_name) %>% summarise(n = n())

bonus.handles$ntweets.official <- as.numeric(sapply(bonus.handles$name, function(x){
  ntweets.official$n[ntweets.official$name == x]
}))
bonus.handles$ntweets.official[is.na(bonus.handles$ntweets.official)] <- 0

bonus.handles$ntweets.extra <- as.numeric(sapply(bonus.handles$usefulhandle, function(x){
  ntweets.bonus$n[ntweets.bonus$screen_name == x]
}))
bonus.handles$ntweets.extra[is.na(bonus.handles$ntweets.extra)] <- 0

# get days since january 1 2019 at which tweets were scraped (to get average tweets per day)
days.since <- as.numeric(lubridate::as_date("2019-06-19") - lubridate::as_date("2019-01-01"))

# add flag for whether unofficial account is verified
bonus.handles <- bonus.handles %>% left_join(extra.users[,c("screen_name","verified")], 
                                             by = c("usefulhandle" = "screen_name"))

# flag to drop if unofficial account is not verified or if it is not active (1 tweet per day)
bonus.handles$drop.unofficial <- with(bonus.handles, ifelse(verified == FALSE | ntweets.extra < days.since, 1, 0))

# keep the rest
bonus.handles.keeps <- bonus.handles %>% filter(drop.unofficial == 0)

# combine all follows and tweets (will parse later)
combined.follows <- bind_rows(congress.follows, extrafollows)
combined.tweets <- bind_rows(tweets, extratweets)

# members with two official accounts: lindsey graham, debbie wasserman schultz, brendan boyle, dina titus
double.handles <- data.frame(usefulhandle = c("GrahamBlog","DWStweets","dinatitus","CongBoyle"),
                             officialhandle = c("LindseyGrahamSC","RepDWStweets","repdinatitus","RepBrendanBoyle"))
double.handles <- double.handles %>% 
  left_join(members[,c("screen_name","name")], 
            by = c("officialhandle" = "screen_name"))

# combine tweets/follows for members with two official accounts listed
doublehandle.tweets <- combined.tweets %>% 
  filter(screen_name %in% c(as.character(double.handles$usefulhandle), as.character(double.handles$officialhandle))) %>%
  left_join(double.handles %>% reshape2::melt(id.vars = c("name")) %>% dplyr::select(name, value),
            by = c("screen_name" = "value"))

doublehandle.follows <-combined.follows %>%
  filter(user %in% c(as.character(double.handles$usefulhandle), as.character(double.handles$officialhandle))) %>%
  left_join(double.handles %>% reshape2::melt(id.vars = c("name")) %>% dplyr::select(name, value),
            by = c("user" = "value"))


# combine tweets/follows for members with useful unofficial accounts
cutweets <- combined.tweets %>% 
  filter(screen_name %in% c(as.character(bonus.handles.keeps$usefulhandle), 
                            as.character(bonus.handles.keeps$officialhandle))) %>%
  left_join(bonus.handles.keeps %>% dplyr::select(usefulhandle, officialhandle, name) %>%
              reshape2::melt(id.vars = c("name")) %>% dplyr::select(name, value),
            by = c("screen_name" = "name"))

cufollows <- combined.follows %>%
  filter(user %in% c(as.character(bonus.handles.keeps$usefulhandle), as.character(bonus.handles.keeps$officialhandle))) %>%
  left_join(bonus.handles.keeps %>% dplyr::select(usefulhandle, officialhandle, name) %>%
              reshape2::melt(id.vars = c("name")) %>% dplyr::select(name, value),
            by = c("user" = "name"))


# separate out members who did not need any combining
solo.tweets <- combined.tweets %>% filter(!screen_name %in% c(as.character(bonus.handles.keeps$usefulhandle), 
                                                              as.character(bonus.handles.keeps$officialhandle),
                                                              as.character(double.handles$usefulhandle), 
                                                              as.character(double.handles$officialhandle)))
solo.tweets$name.y <- solo.tweets$name

solo.follows <- combined.follows %>% filter(!user %in% c(as.character(bonus.handles.keeps$usefulhandle), 
                                                              as.character(bonus.handles.keeps$officialhandle),
                                                              as.character(double.handles$usefulhandle), 
                                                              as.character(double.handles$officialhandle)))

# stack
names(doublehandle.tweets) <- names(solo.tweets)
names(cutweets) <- names(solo.tweets)

solo.follows <- solo.follows %>% left_join(members[,c("screen_name","name")],
                                           by = c("user" = "screen_name"))

all.tweets <- bind_rows(solo.tweets, cutweets, doublehandle.tweets)

all.follows <- bind_rows(solo.follows, cufollows, doublehandle.follows)


# save two separate files: a more expansive one that you can use to update, and a smaller one that you can load up for analyses
save(bonus.handles, congress.follows, extra.users, extrafollows, extratweets, 
     members, ntweets.bonus, ntweets.official, tweets,
     all.tweets, all.follows,
     file= "~/Desktop/congress/congresstwitter_progress.RData")
save(all.tweets, all.follows, file  = "~/Desktop/congress/congress116twitter_through61919.RData")