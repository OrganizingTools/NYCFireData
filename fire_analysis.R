# Analysis of NYC Fires
# data generated from heattweet.R
# CC BY-SA-NC 4.0 Alessandro G. Magnasco 2023

setwd("/Users/Ale/Documents/CUNY/DataAnal/FireData")

# create a not-in operator
`%notin%` <- Negate(`%in%`)

#### import pre-processed data ####

library(readr)

# tweets parsed for geolocation
# n=28115
fdny_parsed = read_csv("fdny_parsed.csv")

# geocoded data
# n=26115
geo_code = read_csv("fdny_geo_code.csv")

# parsed with clean date field
# n=24058
geo_code_created = read_csv("fdny_geo_code_created.csv")

#### import PLUTO data ####
# approx (860k,91) 
# a general NYC building dataset, updated monthly
# Primary Land Use Tax Lot Output dataset by NYC Dept. of City Planning
# https://nycplanning.github.io/db-pluto/#/
# https://www.nyc.gov/site/planning/data-maps/open-data.page
# generated from data by DCP,DOF,DCAS,LPC
# one record for tax lot except for condominiums, which have one record per condo complex
# lots are designated by legal county ownership, not administrative service

# define data types that were giving issues for the imported PLUTO datasets
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

# load multiple revisions of PLUTO, in order to create a consistent set
pluto_21v3 = read_csv("pluto_21v3.csv", col_types = pluto_col_types) # 2023-03
pluto_20v8 = read_csv("pluto_20v8.csv", col_types = pluto_col_types)
pluto_19v2 = read_csv("pluto_19v2.csv", col_types = pluto_col_types)
pluto_19v1 = read_csv("pluto_19v1.csv", col_types = pluto_col_types)
pluto_18v2 = read_csv("pluto_18v2.csv", col_types = pluto_col_types)
pluto_18v1 = read_csv("pluto_18v1.csv", col_types = pluto_col_types)
pluto_17v1 = read_csv("pluto_17v1.csv", col_types = pluto_col_types)
pluto_16v2 = read_csv("pluto_16v2.csv", col_types = pluto_col_types)

#### import data on housing and building violations ####

# Dept. of Buildings
# n=2.3M
dob_violations = read_csv("dob_violations.csv")

# DOB Environmental Control Board
# n=1.6M
ecb_violations = read_csv("ecb_violations.csv")

# Dept. of Housing Preservation
# Violations issued, (2.7M,41)
hpd_violations = read_csv("hpd_violations.csv")
# Vacate orders issued, (6480,20)
hpd_vacateorders = read_csv("hpd_vacateorders.csv")

# marshal evictions
marshal_evictions_all <- read_csv("marshal_evictions_all.csv")

# complaints
hpd_complaints <- read_csv("hpd_complaints.csv")   
dob_complaints <- read_csv("dob_complaints.csv")


# Dept. of Sanitation

# Dept. of Health

# FDNY

# FDNY Bureau of Fire Prevention

# rent stabilized units summary from JustFix.NYC
rentstab_summary <- read_csv("rentstab_summary.csv")

#### PRELIMINARY DESCRIPTIVE ANALYSIS ####
library(dplyr)
library(psych)
library(lubridate)

range(dob_violations$issuedate, na.rm=TRUE)

# HPD Vacate Orders
range(hpd_vacateorders$vacateeffectivedate, na.rm=TRUE)
# remove one typo'd observation with date in future
hpd_vacateorders <- hpd_vacateorders %>% filter(year(hpd_vacateorders$vacateeffectivedate) < 2024)
range(hpd_vacateorders$vacateeffectivedate)
hpd_vacateorders$month = format(as.Date(hpd_vacateorders$vacateeffectivedate), format="%Y-%m")
vacates_by_month <- table(hpd_vacateorders$month)
length(unique(hpd_vacateorders$month)) # 137 months tracked

library(psych)
library(ggplot2)

# by type of vacate
describeBy(hpd_vacateorders$numberofvacatedunits,hpd_vacateorders$primaryvacatereason, mat=TRUE)
ggplot(hpd_vacateorders,aes(y=numberofvacatedunits,x=primaryvacatereason))+geom_bar(stat="identity",position="dodge")

# vacates only due to fire
hpd_vacates_fire_all <- hpd_vacateorders %>% filter(hpd_vacateorders$primaryvacatereason == "Fire Damage")
ggplot(hpd_vacates_fire_all,aes(y=numberofvacatedunits,x=year(vacateeffectivedate)))+geom_bar(stat="identity",position="dodge")

# number of fires over time
fires_by_year <- table(year(geo_code_created$created_at))
ggplot(fires_by_year,aes(x=year(created_at)))+geom_bar(stat="identity",position="dodge")

# correlation between fires and unit size


##### analyzing fires in 2021 against previous 5 years' violations

# create a smaller PLUTO dataset for 2021 (fires)
# 39 columns, from 91 originally
pluto_21v3_mini <- pluto_21v3 %>% 
  select(borough,block,lot,cd,ct2010,cb2010,council,zipcode,firecomp,address,zonedist1,zonedist2,zonedist3,zonedist4,bldgclass,landuse,easements,ownertype,lotarea,resarea,numbldgs,numfloors,unitsres,unitstotal,assessland,assesstot,exempttot,yearbuilt,yearalter1,yearalter2,residfar,bbl,tract2010,xcoord,ycoord,geom,latitude,longitude,notes)

# export
# write_csv(pluto_21v3_mini, "pluto_21v3_mini.csv")
# import
# pluto_21v3_mini = read_csv("pluto_21v3_mini.csv")

# PLUTO buildings that were consistent 2016 vs 2021 (violations)
# this only keeps the buildings that did not undergo major renovations between 2016-2020

# using merge
pluto_2016_2021 <- merge(x=pluto_21v3,y=pluto_16v2,by=c("bbl","unitsres","yearalter1","yearalter2"))
# then, it only keeps a smaller subset of columns. separated out for debugging
pluto_2016_2021 <- pluto_2016_2021 %>%
 select(borough.x,block.x,lot.x,cd.x,ct2010.x,cb2010.x,council.x,zipcode.x,firecomp.x,address.x,zonedist1.x,zonedist2.x,zonedist3.x,zonedist4.x,bldgclass.x,landuse.x,easements.x,ownertype.x,lotarea.x,resarea.x,numbldgs.x,numfloors.x,unitsres,unitstotal.x,assessland.x,assesstot.x,exempttot.x,yearbuilt.x,yearalter1,yearalter2,residfar.x,bbl,tract2010.x,xcoord.x,ycoord.x)

# a faster but less accurate approach using dplyr
# pluto_2016_2020 <- semi_join(pluto_20v8, pluto_16v2)

# export
# write_csv(pluto_2016_2021, "pluto_2016_2021.csv")
# import
# pluto_2016_2021 = read_csv("pluto_2016_2021.csv")

# filter fire data for only 2021
fires_2021 <- geo_code_created %>%
  filter(
    geo_code_created$month >= "2021-01-01", geo_code_created$month <= "2021-12-31",
    alarm == 9 # avoid duplication by only taking resolved incidents
    ) %>%
  group_by(bbl) %>%
  tally(name = "fires") %>%
  replace(is.na(.),0)

# export
# write_csv(fires_2021, "fires_2021.csv")
# import
# fires_2021 = read_csv("fires_2021.csv")

table(month(geo_code_created$date))

# separate out winter fires, which have different characteristics
fires_2021_winter <- geo_code_created %>%
  filter(
    year(geo_code_created$date) == 2021,
    month(geo_code_created$date) %in% c(1,2,3,11,12),
    alarm == 9 # avoid duplication by only taking resolved incidents
  ) %>%
  group_by(bbl) %>%
  tally(name = "winterfire") %>%
  replace(is.na(.),0)
# idem for summer fires
fires_2021_summer <- geo_code_created %>%
  filter(
    year(geo_code_created$date) == 2021,
    month(geo_code_created$date) %notin% c(1,2,3,11,12),
    alarm == 9 # avoid duplication by only taking resolved incidents
  ) %>%
  group_by(bbl) %>%
  tally(name = "summerfire") %>%
  replace(is.na(.),0)

# export
# write_csv(fires_2021_winter, "fires_2021_winter.csv")
# write_csv(fires_2021_summer, "fires_2021_summer.csv")
# import
# fires_2021_winter = read_csv("fires_2021_winter.csv")
# fires_2021_summer = read_csv("fires_2021_summer.csv")

### violation data from 2021, 2020, 2019, 2018, 2017, 2016
# Only hazardous violations

table(ecb_violations$severity)

hpd_viols_2016_2020 <- hpd_violations %>%
  filter(
    lubridate::year(hpd_violations$novissueddate) %in% c(2016,2017,2018,2019,2020),
    class %in% c("B","C") # Hazardous, Immediately Hazardous
      ) %>%
  group_by(bbl) %>%
  tally(name = "hpd_viols") %>%
  replace(is.na(.),0)
ecb_viols_2016_2020 <- ecb_violations %>%
  filter(
    lubridate::year(ecb_violations$issuedate) %in% c(2016,2017,2018,2019,2020),
    severity %in% c("Hazardous","Unknown") # Most recent are unclassified, assuming unknown violations are hazardous due to nature of ECB
      ) %>%
  group_by(bbl) %>%
  tally(name = "ecb_viols") %>%
  replace(is.na(.),0)
dob_viols_2016_2020 <- dob_violations %>%
  filter(
    lubridate::year(dob_violations$issuedate) %in% c(2016,2017,2018,2019,2020),
    violationtypecode %in% c("EGNCY","IMEGNCY","UB"), # Emergency, Immediate Emergency, Unsafe Building
    is.na(ecbnumber) # Make sure ECB violations aren't double-counted
    ) %>%
  group_by(bbl) %>%
  tally(name = "dob_viols") %>%
  replace(is.na(.),0)

# NON-CRITICAL violations

hpd_nc_2016_2020 <- hpd_violations %>%
  filter(
    lubridate::year(hpd_violations$novissueddate) %in% c(2016,2017,2018,2019,2020),
    class %notin% c("B","C") # Hazardous, Immediately Hazardous
  ) %>%
  group_by(bbl) %>%
  tally(name = "hpd_nc") %>%
  replace(is.na(.),0)
ecb_nc_2016_2020 <- ecb_violations %>%
  filter(
    lubridate::year(ecb_violations$issuedate) %in% c(2016,2017,2018,2019,2020),
    severity %notin% c("Hazardous","Unknown") # they stopped classifying in 2008, so this returns 0
  ) %>%
  group_by(bbl) %>%
  tally(name = "ecb_nc") %>%
  replace(is.na(.),0)
dob_nc_2016_2020 <- dob_violations %>%
  filter(
    lubridate::year(dob_violations$issuedate) %in% c(2016,2017,2018,2019,2020),
    violationtypecode %notin% c("EGNCY","IMEGNCY","UB"), # Emergency, Immediate Emergency, Unsafe Building
    is.na(ecbnumber) # Make sure ECB violations aren't double-counted
  ) %>%
  group_by(bbl) %>%
  tally(name = "dob_nc") %>%
  replace(is.na(.),0)

# vacate orders
hpd_vacates_fires <- hpd_vacateorders %>%
  janitor::clean_names() %>%
  filter(
    lubridate::year(vacateeffectivedate) %in% c(2016,2017,2018,2019,2020),
    primaryvacatereason=="Fire Damage"
    ) %>%
  group_by(bbl) %>%
  summarize(vacated_units = sum(numberofvacatedunits)) %>%
  replace(is.na(.),0)

# complaints to HPD and DOB
hpd_compl_2016_2020 <- hpd_complaints %>%
  janitor::clean_names() %>%
  filter(
    lubridate::year(receiveddate) %in% c(2016,2017,2018,2019,2020)
  ) %>%
  group_by(bbl) %>%
  tally(name = "hpd_cp") %>%
  replace(is.na(.),0)

# can't be merged in without geocoding, so skipping for now
dob_compl_2016_2020 <- dob_complaints %>%
  janitor::clean_names() %>%
  filter(
    lubridate::year(dateentered) %in% c(2016,2017,2018,2019,2020)
  ) %>%
  group_by(bin) %>% # not ideal, since BINs are recycled as opposed to BBLs, but we are filtering out any issues later
  tally(name = "dob_cp") %>%
  replace(is.na(.),0)


# summary of rent stabilization and building subsidies
rentstab_mini <- rentstab_summary %>%
  select(ucbbl,unitsstab2007,unitsstab2017,diff,percentchange,j51,a421,scrie,drie,c420)

# merge fires from 2021 and violations from 2016-2020 into matching PLUTO set
pluto_fires_viols <- pluto_2016_2021 %>%
  janitor::clean_names() %>%
  left_join(hpd_vacates_fires, by=c("bbl"="bbl")) %>% distinct() %>%
  left_join(fires_2021, by=c("bbl"="bbl")) %>% distinct() %>%
  left_join(fires_2021_winter, by=c("bbl"="bbl")) %>% distinct() %>%
  left_join(fires_2021_summer, by=c("bbl"="bbl")) %>% distinct() %>%
  left_join(hpd_viols_2016_2020, by=c("bbl"="bbl")) %>% distinct() %>%
  left_join(ecb_viols_2016_2020, by=c("bbl"="bbl")) %>% distinct() %>%
  left_join(dob_viols_2016_2020, by=c("bbl"="bbl")) %>% distinct() %>%
  left_join(hpd_nc_2016_2020, by=c("bbl"="bbl")) %>% distinct() %>%
  left_join(ecb_nc_2016_2020, by=c("bbl"="bbl")) %>% distinct() %>%
  left_join(dob_nc_2016_2020, by=c("bbl"="bbl")) %>% distinct() %>%
  left_join(hpd_compl_2016_2020, by=c("bbl"="bbl")) %>% distinct() %>%
  #left_join(dob_compl_2016_2020, by=c("bin"="bin")) %>% distinct() %>%
  left_join(rentstab_mini, by=c("bbl"="ucbbl")) %>% distinct() %>%
  # remove buildings without residential units
  filter(unitsres > 0)

# clean up NA values only on specific columns
pluto_fires_viols$vacated_units[is.na(pluto_fires_viols$vacated_units)] <- 0
pluto_fires_viols$fires[is.na(pluto_fires_viols$fires)] <- 0
pluto_fires_viols$winterfire[is.na(pluto_fires_viols$winterfire)] <- 0
pluto_fires_viols$summerfire[is.na(pluto_fires_viols$summerfire)] <- 0
pluto_fires_viols$hpd_viols[is.na(pluto_fires_viols$hpd_viols)] <- 0
pluto_fires_viols$ecb_viols[is.na(pluto_fires_viols$ecb_viols)] <- 0
pluto_fires_viols$dob_viols[is.na(pluto_fires_viols$dob_viols)] <- 0
pluto_fires_viols$hpd_nc[is.na(pluto_fires_viols$hpd_nc)] <- 0
pluto_fires_viols$ecb_nc[is.na(pluto_fires_viols$ecb_nc)] <- 0
pluto_fires_viols$dob_nc[is.na(pluto_fires_viols$dob_nc)] <- 0
pluto_fires_viols$hpd_cp[is.na(pluto_fires_viols$hpd_cp)] <- 0

# recode all rent stabilization and subsidy data into numeric binary
pluto_fires_viols$j51[!is.na(pluto_fires_viols$j51)] <- 1
pluto_fires_viols$j51[is.na(pluto_fires_viols$j51)] <- 0
pluto_fires_viols$j51 <- as.numeric(pluto_fires_viols$j51)
pluto_fires_viols$a421[!is.na(pluto_fires_viols$a421)] <- 1
pluto_fires_viols$a421[is.na(pluto_fires_viols$a421)] <- 0
pluto_fires_viols$a421 <- as.numeric(pluto_fires_viols$a421)
pluto_fires_viols$scrie[!is.na(pluto_fires_viols$scrie)] <- 1
pluto_fires_viols$scrie[is.na(pluto_fires_viols$scrie)] <- 0
pluto_fires_viols$scrie <- as.numeric(pluto_fires_viols$scrie)
pluto_fires_viols$drie[!is.na(pluto_fires_viols$drie)] <- 1
pluto_fires_viols$drie[is.na(pluto_fires_viols$drie)] <- 0
pluto_fires_viols$drie <- as.numeric(pluto_fires_viols$drie)
pluto_fires_viols$c420[!is.na(pluto_fires_viols$c420)] <- 1
pluto_fires_viols$c420[is.na(pluto_fires_viols$c420)] <- 0
pluto_fires_viols$c420 <- as.numeric(pluto_fires_viols$c420)

##### Some summary statistics #####

# numeric binary for "had fire" or not, since some buildings had 4 fires in just 2021
pluto_fires_viols$firesbin <- recode(pluto_fires_viols$fires, `2`=1,`3`=1,`4`=1)
table(pluto_fires_viols$fires)
table(pluto_fires_viols$firesbin)
# add a new column that adds all critical violations
pluto_fires_viols$all_crit_viols <- pluto_fires_viols$hpd_viols+pluto_fires_viols$ecb_viols+pluto_fires_viols$dob_viols
# adds all non-critical violations
pluto_fires_viols$all_nc_viols <- pluto_fires_viols$hpd_nc+pluto_fires_viols$ecb_nc+pluto_fires_viols$dob_nc
# calculate percentage of vacated units
pluto_fires_viols$pct_vac_fire <- pluto_fires_viols$vacated_units/pluto_fires_viols$unitsres
# average number of hazardous violations per unit during those 5 years
pluto_fires_viols$avg_viol_unit <- pluto_fires_viols$all_crit_viols/pluto_fires_viols$unitstotal_x
# and for non-critical violations
pluto_fires_viols$avg_nc_unit <- pluto_fires_viols$all_nc_viols/pluto_fires_viols$unitstotal_x
# avg assessed building value per residential square foot
pluto_fires_viols$avg_sqft_value <- pluto_fires_viols$assesstot_x/pluto_fires_viols$lotarea_x
# this generated some infinites, so dropping them now
pluto_fires_viols <- pluto_fires_viols[!is.infinite(pluto_fires_viols$avg_sqft_value),]

### FINAL EXPORT/IMPORT BEFORE ANALYSIS ###
# export
# write_csv(pluto_fires_viols, "pluto_fires_viols.csv")
# import
# pluto_fires_viols = read_csv("pluto_fires_viols.csv")

# continued in fire_models.R