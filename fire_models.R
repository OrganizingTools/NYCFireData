# Analysis of NYC Fires - Models
# data generated from heattweet.R and organized in fire_analysis.R
# CC BY-SA-NC 4.0 Alessandro G. Magnasco 2023

setwd("/Users/Ale/Documents/CUNY/DataAnal/FireData")

# create a not-in operator
`%notin%` <- Negate(`%in%`)

pluto_fires_viols = read_csv("pluto_fires_viols.csv")

##### DESCRIPTIVES #####

# fires in 2021
hist(pluto_fires_viols$fires, main="Hist. of Fires") # Bernoulli dist.
summary(pluto_fires_viols$fires)
sum(pluto_fires_viols$fires,na.rm=TRUE) # 1291 with fires, 731841 without, 732112 total
describe(pluto_fires_viols$fires)

# winter fires
hist(pluto_fires_viols$winterfire, main="Hist. Winter Fires") # Bernoulli dist.
summary(pluto_fires_viols$winterfire) # 
# summer fires
hist(pluto_fires_viols$summerfire, main="Hist. Summer Fires") # Bernoulli dist.
summary(pluto_fires_viols$summerfire) # 

# vacated units 2016-2020
hist(pluto_fires_viols$vacated_units, main="Hist. of Vacated Units") # Bernoulli dist.
summary(pluto_fires_viols$vacated_units)
sum(pluto_fires_viols$vacated_units,na.rm=TRUE) # 2918 units vacated, 731925 without

# avg num vacated units 2016-2020
hist(pluto_fires_viols$pct_vac_fire, main="Hist. of Avg Pct Vacated") # Bernoulli dist.
summary(pluto_fires_viols$pct_vac_fire)
count(pluto_fires_viols$pct_vac_fire,na.rm=TRUE)

# total hazardous violations 2016-2020
hist(pluto_fires_viols$all_crit_viols, main="Hist. of Hazardous Viols.") # Bernoulli dist.
summary(pluto_fires_viols$all_crit_viols) # mean is 0.9763 hazardous viols per unit, median is 0, max is 1264

# total non-hazardous violations 2016-2020
hist(pluto_fires_viols$all_nc_viols, main="Hist. of Non-Critical Viols.") # Bernoulli dist.
summary(pluto_fires_viols$all_nc_viols) # mean is 0.37 non-hazardous viols per unit, median is 0, max is 633

# average assessed dollar value per lot square foot in 2021
hist(pluto_fires_viols$avg_sqft_value, main="Hist. avg. $/res sqft (2021)")
summary(pluto_fires_viols$avg_sqft_value,na.rm=TRUE) # median is 19.44, max is 27236

# percent change in rent stabilized units 2007-2017
hist(pluto_fires_viols$percentchange, main="% chg, rent stabilized units 2007-2017")
summary(pluto_fires_viols$percentchange,na.rm=TRUE) # 691375 buildings are NA, mean is 9.6% drop

##### CORRELATIONS #####

# Spearman correlations, due to non-normal dist, ranked variables
cor(pluto_fires_viols$fires, pluto_fires_viols$avg_viol_unit, method="spearman", use="complete.obs") # pearson 0.016, spearman 0.047
cor(pluto_fires_viols$winterfire, pluto_fires_viols$avg_viol_unit, method="spearman", use="complete.obs") # spearman 0.032
cor(pluto_fires_viols$summerfire, pluto_fires_viols$avg_viol_unit, method="spearman", use="complete.obs") # spearman 0.039

ggplot(pluto_fires_viols, aes(summerfire, log(all_nc_viols))) + geom_point() + geom_smooth(method='lm',se=TRUE)

cor(pluto_fires_viols$firesbin, pluto_fires_viols$avg_sqft_value, method="spearman", use="complete.obs") # pearson 0.021, spearman 0.024
cor(pluto_fires_viols$firesbin, pluto_fires_viols$all_crit_viols, method="spearman", use="complete.obs") # pearson 0.084, spearman 0.051
cor(pluto_fires_viols$firesbin, pluto_fires_viols$all_nc_viols, method="spearman", use="complete.obs") # pearson 0.114, spearman 0.060
cor(pluto_fires_viols$firesbin, pluto_fires_viols$avg_nc_unit, method="spearman", use="complete.obs") # pearson 0.019, spearman 0.056

cor(pluto_fires_viols$fires, pluto_fires_viols$hpd_cp, method="spearman", use="complete.obs") # spearman 0.053
cor(pluto_fires_viols$winterfire, pluto_fires_viols$hpd_cp, method="spearman", use="complete.obs") # spearman 0.034
cor(pluto_fires_viols$summerfire, pluto_fires_viols$hpd_cp, method="spearman", use="complete.obs") # spearman 0.042

cor(pluto_fires_viols$hpd_viols, pluto_fires_viols$hpd_cp, method="spearman", use="complete.obs") # spearman 0.514 -- about half of complaints become violations

lm(data=pluto_fires_viols, formula= fires~avg_viol_unit+avg_sqft_value)

# factor analysis
#install.packages("corrplot")
require(corrplot)

# minify dataset for factor analysis
# removed ecb_nc as they stopped classifying in 2008, classing all as hazardous due to nature of ECB
pfv_mini <- pluto_fires_viols %>% 
  select(zipcode_x,zonedist1_x,bldgclass_x,ownertype_x,lotarea_x,resarea_x,numbldgs_x,numfloors_x,unitsres,unitstotal_x,assessland_x,assesstot_x,exempttot_x,yearbuilt_x,yearalter1,yearalter2,residfar_x,fires,hpd_viols,ecb_viols,dob_viols,all_crit_viols,avg_viol_unit,avg_sqft_value,hpd_nc,dob_nc,all_nc_viols,avg_nc_unit,diff,percentchange,j51,a421,scrie,drie,c420,hpd_cp,winterfire,summerfire)

# transform certain columns to factor
pfv_mini$zipcode_x <- as.factor(pfv_mini$zipcode_x)
pfv_mini$zonedist1_x <- as.factor(pfv_mini$zonedist1_x)
pfv_mini$bldgclass_x <- as.factor(pfv_mini$bldgclass_x)
pfv_mini$ownertype_x <- as.factor(pfv_mini$ownertype_x)

# generate a Spearman correlation matrix of complete numerical variables, ignoring first 4 columns which are factors
datamatrix <- cor(pfv_mini[,(ncol(pfv_mini)-32+1):ncol(pfv_mini)], method="spearman", use="complete.obs")
corrplot(datamatrix, method="color")
summary(datamatrix)

# some notes:
# size of building matters, as expected.
# Unexpected: Non-critical viols affect fires more than critical ones, but at an aggregate, not avg per unit.
# year of construction does not relate to fires, nor does assessed value of the lot
# overall no really strong relationships to fires in this dataset

summary(pfv_mini$dob_viols)

# subtract winter fires from summer fires in correlation
view(datamatrix[32,]-datamatrix[31,])

# Predicting fires from average hazardous violations and average assessed value of residential square footage
model1 <- glm(data=pluto_fires_viols, formula= fires~avg_viol_unit+avg_sqft_value, family="poisson")
summary(model1) # sqft value isn't significant, so trying different model

# adding year of construction
model2 <- glm(data=pluto_fires_viols, formula= fires~avg_viol_unit+avg_sqft_value+yearbuilt_x, family="poisson")
summary(model2)

# adding lot area
model3 <- glm(data=pluto_fires_viols, formula= fires~avg_viol_unit+avg_sqft_value+yearbuilt_x+lotarea_x, family="poisson")
summary(model3)
exp(coefficients(model3))

# removing assessed sqft value
model4 <- glm(data=pluto_fires_viols, formula= fires~avg_viol_unit+yearbuilt_x+lotarea_x, family="poisson")
summary(model4)
exp(coefficients(model4))

# adding a factor of the building class
model5 <- glm(data=pluto_fires_viols, formula= fires~avg_viol_unit+yearbuilt_x+lotarea_x+factor(bldgclass_x), family="poisson")
summary(model5)
exp(coefficients(model5))
plot(model5)

# using those identified from exploratory factor analysis
model6 <- glm(data=pfv_mini, formula= fires~hpd_nc+all_crit_viols+unitstotal_x+avg_sqft_value, family="poisson")
summary(model6)
exp(coefficients(model6))

# summer 
model7s <- glm(data=pfv_mini, formula= summerfire~all_nc_viols+all_crit_viols+drie, family="poisson")
summary(model7s)
exp(coefficients(model7s))

# winter
model8w <- glm(data=pfv_mini, formula= winterfire~hpd_nc+all_crit_viols+unitstotal_x+avg_sqft_value, family="poisson")
summary(model8w)
exp(coefficients(model8w))

plot(model7s)
plot(model8w)

# testing whether model 6 is appropriate
#install.packages("AER")
require(AER)
dispersiontest(model7s,trafo=1) # 

# LRT 
step(model7s, test="LRT")

# model 7s diagnostic
model7s_diag <- data.frame(pfv_mini,
                           link = predict(model7s, type = "link"),
                           fit = predict(model7s, type = "response"),
                           pearson = residuals(model7s, type = "pearson"),
                           resid = residuals(model7s, type = "response"),
                           residSqr = residuals(model7s, type = "response")^2
)

# plotted diagnostic
ggplot(model7s_diag, aes(x = fit, y = residSqr)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, size = 1) +
  geom_abline(intercept = 0, slope = summary(model8w)$dispersion,
              color = "darkgreen", linetype = 2, size = 1) +
  geom_smooth(se = F, size = 1) +
  theme_bw() 