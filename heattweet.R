# HeatTweet.R
# version 2.0
# Original by Alyssa Goldberg 2020
# For hackathon team by Alessandro G. Magnasco, who continued it
# NYC Open Data Week, March 2020
# R version 4.2.2

# changes from 1.0: changed scraper, updated regex, general cleanup and tweaks

##### SETUP #####

setwd("/Users/Ale/Documents/CUNY/DataAnal/FireData")

# Twitter API connection
  #require(twitteR) # original code, now deprecated
  #require(rtweet) # doesn't use the v2 endpoint
#install.packages("academictwitteR")
require(academictwitteR)

# data processing
require(tidyverse)
require(remotes)
require(sf)
require(dplyr)
require(dbplyr)

# geocoding
# install https://github.com/austensen/geoclient
#remotes::install_github("austensen/geoclient")
require(geoclient)

# for connecting to postgresql instance
# install.packages("RPostgres")
require(RPostgres)
require(DBI)
# install.packages(c("RODBC", "odbc"))
require(RODBC)
require(odbc)

###### EXTRACT ######

# scrape tweets 
# get api credentials by signing up at https://developer.twitter.com/
# Good tutorial at http://utstat.toronto.edu/~nathan/teaching/sta4002/Class1/scrapingtwitterinR-NT.html

# API setup

# initial setup steps, save keys to env
#vignette("academictwitteR-intro") # setup tutorial
#readLines("./twitteR credentials.txt") #I've saved my credentials into a txt file
#set_bearer()

# test OAUTH
get_bearer()

# scrape tweets from fdnyalerts account

#first scrapes into a list.  Need to convert to a data frame with the second function
#timeline search, limited to last 3200
#raw_fdny<-userTimeline("fdnyalerts", n = 3200) %>% 
#  twListToDF(.)

# full archive search, academic access to v2 endpoint
raw_fdny_2019 <-
  get_all_tweets(
    query = "from:fdnyalerts",
    start_tweets = "2019-01-01T00:00:00Z",
    end_tweets = "2020-01-01T00:00:00Z",
    data_path = "tweets/", # saves to directory
    bind_tweets = FALSE, # resolves issues with large transfers
    n = Inf
  )

raw_fdny <-
  get_all_tweets(
    query = "from:fdnyalerts",
    start_tweets = "2008-01-01T00:00:00Z",
    end_tweets = "2023-04-09T00:00:00Z",
    data_path = "tweets/", # saves to directory
    bind_tweets = FALSE, # resolves issues with large transfers
    n = Inf
  )

# bind data to dataframe using academicTwitteR
raw_fdny <- bind_tweets(data_path = "tweets/")

# take a peek
glimpse(raw_fdny)
summary.data.frame(raw_fdny)
summary(raw_fdny$created_at)
min(raw_fdny$created_at)
max(raw_fdny$created_at)

###### PARSE ######

# in order to use with the pluto database and geoclient we need to extract the numeric and 2-alpha boro codes. After that, set the alarm type to levels 0-9 to facilitate calculating fire duration.

# early tweets had the occasional typo and might get missed by this process
# starting on 2020-12-18T23:54:15.000Z, the structure was changed.
# the new structure changed the variable names and placement, and adds alarm box numbers, among others

library(dplyr)

fdny_parsed<-raw_fdny %>% 
  mutate(
    boro_code=case_when(
      str_detect(text, '(?i)QNS|Queens(?-i)') ~ 4,
      str_detect(text, '(?i)BX|Bronx(?-i)') ~ 2,
      str_detect(text, '(?i)BKLYN|BK|Brooklyn(?-i)') ~ 3,
      str_detect(text, '(?i)MAN|Manhattan(?-i)') ~ 1,
      str_detect(text, '(?i)SI|Staten(?-i)') ~ 5,
      TRUE ~ 0)
  ) %>% 
  mutate(
    boro = case_when(
      str_detect(text, '(?i)QNS|Queens(?-i)') ~ 'QN',
      str_detect(text, '(?i)BX|Bronx(?-i)') ~ 'BX',
      str_detect(text, '(?i)BKLYN|BK|Brooklyn(?-i)') ~ 'BK',
      str_detect(text, '(?i)MAN|Manhattan(?-i)') ~ 'MH',
      str_detect(text, '(?i)SI|Staten(?-i)') ~ 'SI',
      TRUE ~ 'none'
    )) %>% 
  # FDNY Box number (street intersections)
  mutate(
    box = trimws(str_extract(text,'(?<=(?i)Box (?-i)).+?(?=,)'))
  ) %>%
  # check for creation date and parse structure accordingly
  mutate(street = case_when(
    # extract everything between either the words HANDS or ALARM and before the first comma after HAND or ALARM to pull the street address
    created_at<"2020-12-18T23:54:15.000Z" ~ trimws(str_extract(text, '(?<=(?i)HANDS|ALARM|MAJOR EMERGENCY (?-i)).+?(?=,)')),
    # extract everything between the first comma after the box number and the following comma
    created_at>="2020-12-18T23:54:15.000Z" ~ trimws(str_extract(text, '(?<=(?i)Box [0-9]{4}, (?-i)).+?(?=,)')),
    TRUE ~ 'none'
    )) %>% 
  mutate(alarm = case_when(
    str_detect(text, '(?i)CLEAR|UNDER CONTROL(?-i)') ~ 9,
    str_detect(text, '(?i)ALL HANDS(?-i)') ~ 1,
    str_detect(text, '(?i)2-ALARM(?-i)') ~ 2,
    str_detect(text, '(?i)3-ALARM(?-i)') ~3,
    str_detect(text, '(?i)4-ALARM(?-i)') ~ 4,
    str_detect(text, '(?i)5-ALARM(?-i)') ~ 5,
    TRUE ~ 0
  )) %>%
  # mutate(street = trimws(map(str_split(street, ","),`[`,1))) %>% #might have to extract again if there is more than one comma
  select(created_at,text,boro,boro_code,street,alarm,box)

#fires that have alarm level 0 don't have the words ALARM or HAND so we need another strategy for extracting the address.
working_fires<-fdny_parsed %>% filter(is.na(street)) %>% 
  mutate(code=map(str_split(text, ' '),`[`,2) %>% unlist()) %>% 
  group_by(code) %>% 
  tally() %>% 
  filter(n>1) 

working_fires_codes<-working_fires %>% 
  pull(code) %>% 
  paste0(., collapse = "|")

#replace the codes identified in working_fires_codes with XXX to facilitate extracting the address from these items as well
working_fires_streets<-fdny_parsed %>% 
  filter(is.na(street)) %>% 
  mutate(text = str_replace(text, working_fires_codes, "XXX")) %>% 
  mutate(street = str_extract(text, '(?<=XXX ).+?(?=,)')) %>% 
  select(created_at, boro, boro_code, street, alarm)

#bind the rows to the fdny_parsed dataframe, filter out nas
fdny_parsed<-fdny_parsed %>% 
  bind_rows(working_fires_streets) %>% 
  filter(!is.na(street)) %>% 
  mutate(search_string=case_when(#add boro names to address to facilitate search
    boro == "BK" ~ paste(street, ", Brooklyn, NY", sep=""),
    boro == "MH" ~ paste(street, ", New York, NY", sep = ""),
    boro == "SI" ~ paste(street, ", Staten Island, NY", sep = ""),
    boro == "BX" ~ paste(street, ", Bronx, NY", sep = ""), 
    boro == "QN" ~ paste(street, ", Queens, NY", sep = ""),
    TRUE ~ "")
  )

# save parsed dataset
write_csv(fdny_parsed, "fdny_parsed.csv")

#### GEOCODING ####

# The dataframe is now ready for geocoding with geoclient

# thank you to Maxwell Austensen for this great client
# tutorial https://github.com/austensen/geoclient
# follow instructions to create api id and key
# because the street field doesn't conform to geoclient's geo_address format, we'll use geo_search

# we are doing this to find the BBL number (Boro-Block-Lot)
# BBLs are the best unique identifier for NYC housing data (BINs are recycled)
# using this, we can join it to other datasets

# key saved in external text file
# geoclient_api_key(key = geoclient_key, install = TRUE)

# Run the geo_search, this can take 2 or 3 minutes
# as of 2023/4/17, limits are 100/second, 2500/min, and 500k/day
geo_code<-geo_search_data(fdny_parsed, location = search_string) 

# export unfiltered geocoded set before we remove nulls
write_csv(geo_code, "fdny_geo_code.csv")
# geo_code = read_csv("fdny_geo_code.csv")

# pull out only addresses that didn't return a geocoded BBL
# about 6% error rate, many are highways or otherwise unique locations
# many nulls have codes in front of search string, invalidating geocoding
# structure is one or two numbers, dash, one or two numbers, then address
# e.g. 10-75, 7-5, 3-3
# some are radio "10 codes", can't figure out the rest
# difficult to remove because many real addresses are formatted that way
geo_null <-geo_code %>% filter(is.na(bbl))

# remove the null BBLs from the geo_code file
geo_code<-geo_code %>% filter(!is.na(bbl))

# create a smaller data set with lat long data
geo_code_lat_long <- geo_code %>% 
  select(input_location,latitude, longitude, bbl, fireBattalion,censusTract2010, communityDistrict)

# Add the fire data to the geo_code data, remove the nulls.
geo_code_created <- fdny_parsed %>% 
  left_join(geo_code_lat_long, by=c("search_string"="input_location")) %>% 
  distinct() 

# add date field as yyyy-mm-dd
geo_code_created$date = as.Date(geo_code_created$created_at, format="%Y-%m-%d")
geo_code_created$month = format(as.Date(geo_code_created$created_at), format="%Y-%m")

# export CSV
write_csv(geo_code_created, "fdny_geo_code_created.csv")
geo_code_created = read_csv("fdny_geo_code_created.csv")

###### LOAD ######

# set up postgresql connection to NYCDB
# this is the database

# I am using the instance hosted by the Housing Data Coalition
# but you can set up your own at https://github.com/nycdb/nycdb

library(DBI)
library(RODBC)
library(odbc)
library(dbplyr)
library(RPostgres)

# check validity of the connection
dbCheck <- dbCanConnect(RPostgres::Postgres(),
                      dbname = hdc_db,
                      user = hdc_user,
                      password = hdc_pass,
                      host = hdc_server
                    )
dbCheck
# create the connection
con <- dbConnect(RPostgres::Postgres(),
                      dbname = hdc_db,
                      user = hdc_user,
                      password = hdc_pass,
                      host = hdc_server
                      )

# test the connection
con
dbListTables(con)

### pull data-- these will take a while ###
# run as background job for now, will update later to chunk processing

# PLUTO is a collection of several city datasets on every lot in NYC
# each fire needs to be matched to the version of pluto in effect at that time
# approx (860k,91)
pluto_21v3 <- dbGetQuery(con, "SELECT * FROM pluto_21v3")
write_csv(pluto_21v3, "pluto_21v3.csv")
pluto_20v8 <- dbGetQuery(con, "SELECT * FROM pluto_20v8")
write_csv(pluto_20v8, "pluto_20v8.csv")
pluto_19v2 <- dbGetQuery(con, "SELECT * FROM pluto_19v2")
write_csv(pluto_19v2, "pluto_19v2.csv")
pluto_19v1 <- dbGetQuery(con, "SELECT * FROM pluto_19v1")
write_csv(pluto_19v1, "pluto_19v1.csv")
pluto_18v2 <- dbGetQuery(con, "SELECT * FROM pluto_18v2")
write_csv(pluto_18v2, "pluto_18v2.csv")
pluto_18v1 <- dbGetQuery(con, "SELECT * FROM pluto_18v1")
write_csv(pluto_18v1, "pluto_18v1.csv")
pluto_17v1 <- dbGetQuery(con, "SELECT * FROM pluto_17v1")
write_csv(pluto_17v1, "pluto_17v1.csv")
pluto_16v2 <- dbGetQuery(con, "SELECT * FROM pluto_16v2")
write_csv(pluto_16v2, "pluto_16v2.csv")
#pluto_15v1 <- dbGetQuery(con, "SELECT * FROM pluto_15v1")
#write_csv(pluto_21v3, "pluto_21v3.csv")
#pluto_10v1 <- dbGetQuery(con, "SELECT * FROM pluto_10v1")
#write_csv(pluto_21v3, "pluto_21v3.csv")

# debugging and fixing readr automatic column type assignments
# the columns changed over the many years of pluto revisions
spec(pluto_21v3)
pluto_col_types = cols(
  overlay1 = col_character(),
  overlay2 = col_character(),
  zonedist3 = col_character(),
  zonedist4 = col_character(),
  spdist2 = col_character(),
  spdist3 = col_character(),
  splitzone = col_character(),
  ownername = col_character(),
  landmark = col_character()
)
problems(read_csv("pluto_16v2.csv", col_types = pluto_col_types))

pluto_21v3 = read_csv("pluto_21v3.csv", col_types = pluto_col_types) # 2023-03
pluto_20v8 = read_csv("pluto_20v8.csv", col_types = pluto_col_types)
pluto_19v2 = read_csv("pluto_19v2.csv", col_types = pluto_col_types)
pluto_19v1 = read_csv("pluto_19v1.csv", col_types = pluto_col_types)
pluto_18v2 = read_csv("pluto_18v2.csv", col_types = pluto_col_types)
pluto_18v1 = read_csv("pluto_18v1.csv", col_types = pluto_col_types)
pluto_17v1 = read_csv("pluto_17v1.csv", col_types = pluto_col_types)
pluto_16v2 = read_csv("pluto_16v2.csv", col_types = pluto_col_types)

# datasets on violations
ecb_violations <- dbReadTable(con, "ecb_violations")
write_csv(ecb_violations, "ecb_violations.csv")   
dob_violations <- dbReadTable(con, "dob_violations")
write_csv(dob_violations, "dob_violations.csv")   # 2.3M, 19
hpd_violations <- dbReadTable(con, "hpd_violations")
write_csv(hpd_violations, "hpd_violations.csv")   
# datasets on complaints
hpd_complaints <- dbReadTable(con, "hpd_complaints")
write_csv(hpd_complaints, "hpd_complaints.csv")   
dob_complaints <- dbReadTable(con, "dob_complaints")
write_csv(dob_complaints, "dob_complaints.csv")

# datasets on Marshal evictions and HPD vacate orders
marshal_evictions_all <- dbReadTable(con, "marshal_evictions_all")
write_csv(marshal_evictions_all, "marshal_evictions_all.csv")
hpd_vacateorders <- dbReadTable(con, "hpd_vacateorders")
write_csv(hpd_vacateorders, "hpd_vacateorders.csv")

# datasets on financial and legal factors affecting buildings
hpd_litigations <- dbReadTable(con, "hpd_litigations")
write_csv(hpd_litigations, "hpd_litigations.csv")   
dof_exemptions <- dbReadTable(con, "dof_exemptions")
write_csv(dof_exemptions, "dof_exemptions.csv")   
hpd_conh <- dbReadTable(con, "hpd_conh")
write_csv(hpd_conh, "hpd_conh.csv")  
dof_sales <- dbReadTable(con, "dof_sales")
write_csv(dof_sales, "dof_sales.csv") 
dobjobs <- dbReadTable(con, "dobjobs")
write_csv(dobjobs, "dobjobs.csv") 
speculation_watch_list <- dbReadTable(con, "speculation_watch_list") ## missing from here
write_csv(speculation_watch_list, "speculation_watch_list.csv") 

rentstab_summary <- dbReadTable(con, "rentstab_summary")
write_csv(rentstab_summary, "rentstab_summary.csv")  
# contained in rentstab_summary
dof_421a <- dbReadTable(con, "dof_421a")
write_csv(dof_421a, "dof_421a.csv") 
j51_exemptions <- dbReadTable(con, "j51_exemptions")
write_csv(j51_exemptions, "j51_exemptions.csv")   

# Who Owns What building dataset
wow_bldgs <- dbReadTable(con, "wow_bldgs")
write_csv(wow_bldgs, "wow_bldgs.csv") 

# disconnect
dbDisconnect(con)

### process datasets to join with fires

# merge different versions of pluto?
# find version of pluto in place at time of fire
# match up each fire with corresponding pluto

##### MERGE WITH 311 DATA ######

# from New York Open Data download 311 data and save it in the working directory. 
# Use read_csv_chunked to filter that data for the bbl codes in the geo_code_created file
# create the DataFrameCallback filter
f1<-function(x,pos)subset(x, BBL %in% geo_code_created$bbl)

# read the data in chunked the use DataFrameCallback$new() with the filter function
# to read in only those items that match your criteria
# in this case it reduces a 2 million row data frame to just under 50k
all_311<-read_csv('311_requests.csv') %>% 
  janitor::clean_names() %>% 
  group_by(bbl, complaint_type) %>% 
  tally() %>% 
  mutate(fire=ifelse(
    test = bbl %in% geo_code$bbl,
    yes=1,
    no=0
  ))

#filter to include only fire data set bbls
all_filtered <- all_311 %>% 
  filter(bbl %in% geo_code$bbl)

#save for later
write_csv(all_311, "all_311_requests.csv")

##### JOIN WITH DATASETS DOWNLOADED FROM OPEN DATA PORTAL #####

### year of construction > building code in force at time of construction

# correlation of building code w fires

### DISPLACEMENT ###

# requires matching fires to version of pluto in effect


### DOB violations ###
displacement <- pluto21v3 %>%
  janitor::clean_names() %>%
  #group_by(bbl_date = paste(bbl,date,sep="-")) %>%
  #summarize(final_status=max(alarm), updates=n()-1, date=date, bbl=bbl, created_at=min(created_at), latitude=latitude, longitude=longitude)
  group_by(bbl,unitsres) %>%
  tally() %>%
  mutate(fire=ifelse(
    test = bbl %in% geo_code$bbl,
    yes=1,
    no=0
  ))

library(psych)
describeBy(displacement$unitsres,displacement$fire, mat=TRUE, na.rm=TRUE)

# HPD VACATE ORDERS (minimum floor for displacement)
hpd_vacates_fires <- hpd_vacateorders %>%
  janitor::clean_names() %>%
  filter(primaryvacatereason=="Fire Damage") %>%
  group_by(bbl,numberofvacatedunits) %>%
  tally()
  
hpd_viols_fires <- hpd_violations %>%
  janitor::clean_names() %>%
  group_by(bbl,class) %>%
  tally() %>%
  mutate(fire=ifelse(
    test = bbl %in% geo_code$bbl,
    yes=1,
    no=0
  )) %>%
  left_join(hpd_vacates_fires, by=c("bbl"="bbl")) %>%
  distinct()

hpd_viols_fires_classC <-hpd_viols_fires %>%
  filter(class=="C",bbl>1)

describeBy(hpd_viols_fires$n.x, hpd_viols_fires$fire, mat=TRUE, na.rm=TRUE)
boxplot(hpd_viols_fires$n.x, hpd_viols_fires$fire,main="HPD Violations vs fires",xlab="HPD Violations",ylab="Had Fire")

cor(hpd_viols_fires_classC$n.x, hpd_viols_fires_classC$numberofvacatedunits, method = "pearson", use = "complete.obs")

# log
cor(log(hpd_viols_fires_classC$n.x), hpd_viols_fires_classC$numberofvacatedunits, method = "pearson", use = "complete.obs")
plot(hpd_viols_fires_classC$n.x, log(hpd_viols_fires_classC$numberofvacatedunits), pch=19, col="lightblue")
cor(hpd_viols_fires_classC$n.x, log(hpd_viols_fires_classC$numberofvacatedunits), method = "pearson", use = "complete.obs")

lm(numberofvacatedunits~log(n.x), data=hpd_viols_fires_classC)
# interpret coefficient:
# interpret significance: 
# R^2 

########################################
########################################

# #the following is part of an attempt to apply some machine learning to the data set.  Feel free to ignore.
# 
# #create a summary of 311 data to see how many complaints per bbl
# bbl_fire_summary<-all_311 %>% 
#   spread(key=complaint_type, value=n,fill=0)
# 
# 
# fire_cor<-bbl_fire_summary %>% 
#   ungroup() %>% 
#   select(-bbl) %>% 
#   cor(.) 
# 
# fit<-glm(fire~., fire_cor, family = binomial)
# fit$coefficients
# summary(fit
#         )

##### ANALYSIS ######


##### SURVIVAL ANALYSIS ######

##### FACTOR ANALYSIS ######

# likelihood of fires by building code in force at time of construction

# poisson distribution
# have table of every BBL, count by number of fires that happened
# problematic b/c doesn't account for changes over time in structural issues

# AMI by area as predictor of 
# tidycensus

# build an additive index of violations as predictors for fires

