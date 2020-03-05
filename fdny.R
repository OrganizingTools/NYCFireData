require(twitteR)
require(tidyverse)
require(remotes)
require(sf)
#install https://github.com/austensen/geoclient
#remotes::install_github("austensen/geoclient")
require(geoclient)

#scrape tweets 
#get api credentials by signing up at https://developer.twitter.com/
#Good tutorial at http://utstat.toronto.edu/~nathan/teaching/sta4002/Class1/scrapingtwitterinR-NT.html

#save credentials into environment
# read_lines("twitteR credentials.txt") #I've saved my credentials into a txt file
consumer_key<-"xxxxxxx"
consumer_secret<-"xxxxxxx"
access_token<-"xxxxxx"
access_secret<-"xxxxxx"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#scrape tweets based on fdnyalerts account
#first scrapes into a list.  Need to convert to a data frame with the second function
raw_fdny<-userTimeline("fdnyalerts", n = 3200) %>% 
  twListToDF(.)

#in order to use with the pluto database and geoclient we need to extract the numeric and 2-alpha boro codes. After that, set the alarm type to levels 0-9 to facilitate calculating fire duration.
fdny_parsed<-raw_fdny %>% 
  mutate(
    boro_code=case_when(
      str_detect(text, '^QNS')~4,
      str_detect(text, '^BX')~2,
      str_detect(text, 'BKLYN|BK') ~ 3,
      str_detect(text, 'MAN') ~ 1,
      str_detect(text, 'SI') ~ 5,
      TRUE ~ 0)
  ) %>% 
  mutate(
    boro = case_when(
      str_detect(text, '^QNS')~'QN',
      str_detect(text, '^BX')~'BX',
      str_detect(text, 'BKLYN|BK') ~ 'BK',
      str_detect(text, 'MAN') ~ 'MH',
      str_detect(text, 'SI') ~ 'SI',
      TRUE ~ 'none'
    )) %>% 
  mutate(street = trimws(str_extract(text, '(?<=HANDS|ALARM|MAJOR EMERGENCY ).+?(?=,)'))) %>% #extract everything between either the words HANDS or ALARM and before the first comma after HAND or ALARM to pull the  street address
  mutate(alarm = case_when(
    str_detect(text, 'CLEAR|UNDER CONTROL') ~ 9,
    str_detect(text, 'ALL HANDS')~ 1,
    str_detect(text, '2-ALARM')~ 2,
    str_detect(text, '3-ALARM')~3,
    str_detect(text, '4-ALARM') ~ 4,
    str_detect(text, '5-ALARM') ~ 5,
    TRUE ~ 0
  ))%>%
  # mutate(street = trimws(map(str_split(street, ","),`[`,1))) %>% #might have to extract again if there is more than one comma
  select(created,text,boro,boro_code,street,alarm)

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
  select(created, boro, boro_code, street, alarm)

#bind the rows to the fdny_parsed dataframe, filter out nas
fdny_parsed<-fdny_parsed %>% 
  bind_rows(working_fires_streets) %>% 
  filter(!is.na(street)) %>% 
  mutate(search_string=case_when(#add boro names to address to facilitate search
    boro == "BK" ~ paste(street, ", Brooklyn, NY", sep=""),
    boro == "MH"~ paste(street, ", New York, NY", sep = ""),
    boro == "SI" ~ paste(street, ", Staten Island, NY", sep = ""),
    boro == "BX" ~ paste(street, ", Bronx, NY", sep = ""), 
    boro == "QN"~paste(street, ", Queens, NY", sep = ""),
    TRUE ~ "")
  )
  

#The dataframe is now ready for geocoding with geoclient
#I think it is OK to share the api info
#tutorial https://github.com/austensen/geoclient follow instructions to create api id and key
#because the street field doesn't conform to geoclients geo_address format, we'll use geo_search
# geoclient_api_keys(id = '3856bf63',
#                    key ='a71b639934de4a0a5aa20255e7d285c2',
#                    install = TRUE)

#Run the geo_search this can take 2 or 3 minutes
geo_code<-geo_search_data(fdny_parsed, location = search_string) 

write_csv(geo_code, "fdny_geo_code.csv")
#check addresses that didnt' return a bbl
geo_null <-geo_code %>% filter(is.na(bbl))

#remove the nulls from the geo_code file
geo_code<-geo_code %>% filter(!is.na(bbl))

#create a smaller data set with lat long data
geo_code_lat_long<-geo_code%>% 
  select(input_location,latitude, longitude, bbl, fireBattalion,censusTract2010, communityDistrict)

#Add the fire data to the geo_code data remove the nulls.
geo_code_created<-fdny_parsed %>% 
  left_join(geo_code_lat_long, by=c("street"="input_location")) %>% 
  distinct() 

#from New York Open Data download 311 data and save it in the working directory. Use read_csv_chunked to filter that data for the bbl codes in the geo_code_created file
#create the DataFrameCallback filter
f1<-function(x,pos)subset(x, BBL %in% geo_code_created$bbl)

#read the data in chunked the use DataFrameCallback$new() with the filter function to read in only those items that match your criteria in this case it reduces a 2 million row data frame to just under 50k
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
