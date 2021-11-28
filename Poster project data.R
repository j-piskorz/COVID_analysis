## POSTER PROJECT

library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)

setwd("/Users/juliannapiskorz/OneDrive - Imperial College London/Mathematics/M1R/Poster project/Data")

## 1. TOTALS DATA
totals <- read_csv("Total.csv")
totals <- totals[1:7454, ]
totals <- spread(totals, Year, Value)
totals <- totals[, c(1, 6:9, 73:78)]
totals <- totals %>% rename(
  R_2012 = '2012',
  R_2013 = '2013',
  R_2014 = '2014',
  R_2015 = '2015',
  R_2016 = '2016',
  R_2017 = '2017'
)
totals <- totals %>% filter_at(vars(R_2012,R_2013, R_2014, R_2015, R_2016, R_2017), any_vars(!is.na(.)))

## 2. RESTAURANTS AND HOTELS DATA

restaurants <- read_csv("Restaurants_and_hotels.csv")

#deleting the unrelevant footnotes
restaurants <- restaurants [1:5327, ]
restaurants <- spread(restaurants, Year, Value)

#only taking into consideration the relevant columns (2012-2017)
restaurants <- restaurants[, c(1, 6:9, 77:82)]

#renaming the columns
restaurants <- restaurants %>% rename(
  R_2012 = '2012',
  R_2013 = '2013',
  R_2014 = '2014',
  R_2015 = '2015',
  R_2016 = '2016',
  R_2017 = '2017'
)

#deleting rows with "NA" only
restaurants <- restaurants %>% filter_at(vars(R_2012,R_2013, R_2014, R_2015, R_2016, R_2017), any_vars(!is.na(.)))

#cleaning the specific data
restaurants$R_2012[2] <- restaurants$R_2012[3]

#MERGE
merge <- full_join(restaurants, totals, by = c("Country or Area", "Series", "Currency", "SNA System", "Fiscal Year Type"), suffix=c(".rest", ".tot"))

#calculating the % of restaurant spending
merge <- merge %>% filter(!is.na('R_2012.rest')) %>% mutate(R_2012.percent = R_2012.rest/R_2012.tot*100)
merge <- merge %>% filter(!is.na('R_2013.rest')) %>% mutate(R_2013.percent = R_2013.rest/R_2013.tot*100)
merge <- merge %>% filter(!is.na('R_2014.rest')) %>% mutate(R_2014.percent = R_2014.rest/R_2014.tot*100)
merge <- merge %>% filter(!is.na('R_2015.rest')) %>% mutate(R_2015.percent = R_2015.rest/R_2015.tot*100)
merge <- merge %>% filter(!is.na('R_2016.rest')) %>% mutate(R_2016.percent = R_2016.rest/R_2016.tot*100)
merge <- merge %>% filter(!is.na('R_2017.rest')) %>% mutate(R_2017.percent = R_2017.rest/R_2017.tot*100)


#creating a tibble with the percentages only
restaurants.percent <- merge[, c(1:5, 18:23)]

#adjusting for small differences in values
restaurants.percent[9, 10:11] <- restaurants.percent[10, 10:11]
restaurants.percent[39, 10:11] <- restaurants.percent[38, 10:11]
restaurants.percent[68, 10:11] <- restaurants.percent[69, 10:11]
restaurants.percent[82, 10:11] <- restaurants.percent[83, 10:11]
restaurants.percent[94, 8:9] <- restaurants.percent[95, 8:9]

#counting NAs in each row
restaurants.percent <- restaurants.percent %>%
  mutate(no_NA = rowSums(is.na(.)))

#taking row with maximal number of entries (minimal number of NAs)
restaurants.percent <- restaurants.percent %>% 
  group_by(`Country or Area`) %>%
  slice_min(no_NA, 1)

#checking if there is only one row for each country
table(restaurants.percent$`Country or Area`)

#adjusting for doubled records (taking into consideration the more recent data)
restaurants.percent <- restaurants.percent[-c(2, 10, 62, 63, 74), ]

library(matrixStats)

#calculating the mean of each row
restaurants.percent <- restaurants.percent %>%
  ungroup() %>%
  mutate(mean = rowMeans(select(., c('R_2012.percent':'R_2017.percent')), na.rm=TRUE),
         sd = rowSds(as.matrix(select(., c('R_2012.percent':'R_2017.percent'))), na.rm = TRUE))

#checking how many mean values couldn't be calculated
sum(!is.na(restaurants.percent$mean))

#plotting the value of mean
ggplot(restaurants.percent, aes(mean))+
  geom_histogram()

## 3. RECREATION AND CULTURE DATA

culture <- read_csv("Recreation_and_culture.csv")

#deleting the unrelevant footnotes
culture <- culture [1:5462, ]
culture <- spread(culture, Year, Value)

#only taking into consideration the relevant columns (2012-2017)
culture <- culture[, c(1, 6:9, 77:82)]

#renaming the columns
culture <- culture %>% rename(
  R_2012 = '2012',
  R_2013 = '2013',
  R_2014 = '2014',
  R_2015 = '2015',
  R_2016 = '2016',
  R_2017 = '2017'
)

#deleting rows with "NA" only
culture <- culture %>% filter_at(vars(R_2012,R_2013, R_2014, R_2015, R_2016, R_2017), any_vars(!is.na(.)))

#MERGE
merge1 <- full_join(culture, totals, by = c("Country or Area", "Series", "Currency", "SNA System", "Fiscal Year Type"), suffix=c(".cult", ".tot"))

#calculating the % of restaurant spending
merge1 <- merge1 %>% filter(!is.na('R_2012.cult')) %>% mutate(R_2012.percent = R_2012.cult/R_2012.tot*100)
merge1 <- merge1 %>% filter(!is.na('R_2013.cult')) %>% mutate(R_2013.percent = R_2013.cult/R_2013.tot*100)
merge1 <- merge1 %>% filter(!is.na('R_2014.cult')) %>% mutate(R_2014.percent = R_2014.cult/R_2014.tot*100)
merge1 <- merge1 %>% filter(!is.na('R_2015.cult')) %>% mutate(R_2015.percent = R_2015.cult/R_2015.tot*100)
merge1 <- merge1 %>% filter(!is.na('R_2016.cult')) %>% mutate(R_2016.percent = R_2016.cult/R_2016.tot*100)
merge1 <- merge1 %>% filter(!is.na('R_2017.cult')) %>% mutate(R_2017.percent = R_2017.cult/R_2017.tot*100)


#creating a tibble with the percentages only
culture.percent <- merge1[, c(1:5, 18:23)]

#adjusting for small differences in values
culture.percent[2, 6] <- culture.percent[3, 6]
culture.percent[9, 10:11] <- culture.percent[10, 10:11]
culture.percent[37, 10:11] <- culture.percent[36, 10:11]
culture.percent[77, 10:11] <- culture.percent[78, 10:11]
culture.percent[89, 8:9] <- culture.percent[90, 8:9]


#counting NAs in each row
culture.percent <- culture.percent %>%
  mutate(no_NA = rowSums(is.na(.)))

#taking row with maximal number of entries (minimal number of NAs)
culture.percent <- culture.percent %>% 
  group_by(`Country or Area`) %>%
  slice_min(no_NA, 1)

#checking if there is only one row for each country
table(culture.percent$`Country or Area`)

#adjusting for doubled records (taking into consideration the more recent data)
culture.percent <- culture.percent[-c(2, 10, 52, 53, 64, 65, 76), ]

#calculating the mean of each row
culture.percent <- culture.percent %>%
  ungroup() %>%
  mutate(mean = rowMeans(select(., c('R_2012.percent':'R_2017.percent')), na.rm=TRUE),
         sd = rowSds(as.matrix(select(., c('R_2012.percent':'R_2017.percent'))), na.rm = TRUE))

#checking how many mean values couldn't be calculated
sum(!is.na(culture.percent$mean))

#plotting the value of mean
ggplot(culture.percent, aes(mean))+
  geom_histogram()

## 4. TRANSPORT DATA

transport <- read_csv("Transport.csv")

#deleting the irrelevant footnotes
transport <- transport [1:4824, ]
transport <- spread(transport, Year, Value)

#only taking into consideration the relevant columns (2012-2017)
transport <- transport[, c(1, 6:9, 77:82)]

#renaming the columns
transport <- transport %>% rename(
  R_2012 = '2012',
  R_2013 = '2013',
  R_2014 = '2014',
  R_2015 = '2015',
  R_2016 = '2016',
  R_2017 = '2017'
)

#deleting rows with "NA" only
transport <- transport %>% filter_at(vars(R_2012,R_2013, R_2014, R_2015, R_2016, R_2017), any_vars(!is.na(.)))

#MERGE
merge2 <- full_join(transport, totals, by = c("Country or Area", "Series", "Currency", "SNA System", "Fiscal Year Type"), suffix=c(".trans", ".tot"))

#calculating the % of restaurant spending
merge2 <- merge2 %>% filter(!is.na('R_2012.trans')) %>% mutate(R_2012.percent = R_2012.trans/R_2012.tot*100)
merge2 <- merge2 %>% filter(!is.na('R_2013.trans')) %>% mutate(R_2013.percent = R_2013.trans/R_2013.tot*100)
merge2 <- merge2 %>% filter(!is.na('R_2014.trans')) %>% mutate(R_2014.percent = R_2014.trans/R_2014.tot*100)
merge2 <- merge2 %>% filter(!is.na('R_2015.trans')) %>% mutate(R_2015.percent = R_2015.trans/R_2015.tot*100)
merge2 <- merge2 %>% filter(!is.na('R_2016.trans')) %>% mutate(R_2016.percent = R_2016.trans/R_2016.tot*100)
merge2 <- merge2 %>% filter(!is.na('R_2017.trans')) %>% mutate(R_2017.percent = R_2017.trans/R_2017.tot*100)


#creating a tibble with the percentages only
transport.percent <- merge2[, c(1:5, 18:23)]

#adjusting for small differences in values
transport.percent[2, 6] <- transport.percent[3, 6]
transport.percent[40, 10:11] <- transport.percent[39, 10:11]
transport.percent[70, 10:11] <- transport.percent[71, 10:11]
transport.percent[84, 10:11] <- transport.percent[85, 10:11]
transport.percent[92, 8:9] <- transport.percent[91, 8:9]
transport.percent[92, 7] <- transport.percent[93, 7]
transport.percent[99, 8:9] <- transport.percent[100, 8:9]

#counting NAs in each row
transport.percent <- transport.percent %>%
  mutate(no_NA = rowSums(is.na(.)))

#taking row with maximal number of entries (minimal number of NAs)
transport.percent <- transport.percent %>% 
  group_by(`Country or Area`) %>%
  slice_min(no_NA, 1)

#checking if there is only one row for each country
table(transport.percent$`Country or Area`)

#adjusting for doubled records (taking into consideration the more recent data)
transport.percent <- transport.percent[-c(2, 10, 72), ]

#calculating the mean of each row
transport.percent <- transport.percent %>%
  ungroup() %>%
  mutate(mean = rowMeans(select(., c('R_2012.percent':'R_2017.percent')), na.rm=TRUE),
         sd = rowSds(as.matrix(select(., c('R_2012.percent':'R_2017.percent'))), na.rm = TRUE))

#checking how many mean values were calculated
sum(!is.na(transport.percent$mean))

#plotting the value of mean
ggplot(transport.percent, aes(mean))+
  geom_histogram()

# 5. ALCOHOL, TOBACCO AND NARCOTICS DATA 

alcohol <- read_csv("Alcohol_etc.csv")

#deleting the irrelevant footnotes
alcohol <- alcohol [1:5443, ]
alcohol <- spread(alcohol, Year, Value)

#only taking into consideration the relevant columns (2012-2017)
alcohol <- alcohol[, c(1, 6:9, 64:69)]

#renaming the columns
alcohol <- alcohol %>% rename(
  R_2012 = '2012',
  R_2013 = '2013',
  R_2014 = '2014',
  R_2015 = '2015',
  R_2016 = '2016',
  R_2017 = '2017'
)

#deleting rows with "NA" only
alcohol <- alcohol %>% filter_at(vars(R_2012,R_2013, R_2014, R_2015, R_2016, R_2017), any_vars(!is.na(.)))

#MERGE
merge3 <- full_join(alcohol, totals, by = c("Country or Area", "Series", "Currency", "SNA System", "Fiscal Year Type"), suffix=c(".alco", ".tot"))

#calculating the % of restaurant spending
merge3 <- merge3 %>% filter(!is.na('R_2012.alco')) %>% mutate(R_2012.percent = R_2012.alco/R_2012.tot*100)
merge3 <- merge3 %>% filter(!is.na('R_2013.alco')) %>% mutate(R_2013.percent = R_2013.alco/R_2013.tot*100)
merge3 <- merge3 %>% filter(!is.na('R_2014.alco')) %>% mutate(R_2014.percent = R_2014.alco/R_2014.tot*100)
merge3 <- merge3 %>% filter(!is.na('R_2015.alco')) %>% mutate(R_2015.percent = R_2015.alco/R_2015.tot*100)
merge3 <- merge3 %>% filter(!is.na('R_2016.alco')) %>% mutate(R_2016.percent = R_2016.alco/R_2016.tot*100)
merge3 <- merge3 %>% filter(!is.na('R_2017.alco')) %>% mutate(R_2017.percent = R_2017.alco/R_2017.tot*100)


#creating a tibble with the percentages only
alcohol.percent <- merge3[, c(1:5, 18:23)]

#adjusting for small differences in values
alcohol.percent[2, 6] <- alcohol.percent[3, 6]
alcohol.percent[9, 10:11] <- alcohol.percent[10, 10:11]
alcohol.percent[41, 10:11] <- alcohol.percent[40, 10:11]
alcohol.percent[69, 10:11] <- alcohol.percent[70, 10:11]
alcohol.percent[83, 10:11] <- alcohol.percent[84, 10:11]
alcohol.percent[91, 8:9] <- alcohol.percent[90, 8:9]
alcohol.percent[91, 7] <- alcohol.percent[92, 7]
alcohol.percent[98, 8:9] <- alcohol.percent[99, 8:9]

#counting NAs in each row
alcohol.percent <- alcohol.percent %>%
  mutate(no_NA = rowSums(is.na(.)))

#taking row with maximal number of entries (minimal number of NAs)
alcohol.percent <- alcohol.percent %>% 
  group_by(`Country or Area`) %>%
  slice_min(no_NA, 1)

#checking if there is only one row for each country
table(alcohol.percent$`Country or Area`)

#adjusting for doubled records (taking into consideration the more recent data)
alcohol.percent <- alcohol.percent[-c(2, 10, 72), ]

#calculating the mean of each row
alcohol.percent <- alcohol.percent %>%
  ungroup() %>%
  mutate(mean = rowMeans(select(., c('R_2012.percent':'R_2017.percent')), na.rm=TRUE),
         sd = rowSds(as.matrix(select(., c('R_2012.percent':'R_2017.percent'))), na.rm = TRUE))

#checking how many mean values were calculated
sum(!is.na(alcohol.percent$mean))

#plotting the value of mean
ggplot(alcohol.percent, aes(mean))+
  geom_histogram()


## 7. MERGING ALL THE DATA
restaurants.percent1 <- restaurants.percent[,c(1, 13)]
culture.percent1 <- culture.percent[, c(1,13)]
transport.percent1 <- transport.percent[, c(1,13)]
alcohol.percent1 <-alcohol.percent [, c(1,13)]

data.all <- restaurants.percent1 %>%
  full_join(culture.percent1, by='Country or Area', suffix = c(".rest", ".cult")) %>%
  full_join(transport.percent1, by='Country or Area', suffix = c("", ".trans")) %>%
  full_join(alcohol.percent1, by='Country or Area', suffix = c("", ".alco"))

data.all <- data.all %>% rename("mean.trans" = mean)

#deleting the rows with NAs only
data.all <- data.all %>% filter_at(vars(mean.alco, mean.cult, mean.rest, mean.trans), any_vars(!is.na(.)))

#deleting the rows with any NAs
#data.all <- data.all %>% filter_at(vars(mean.alco, mean.cult, mean.rest, mean.trans), all_vars(!is.na(.)))

## 6. ANALYSING COVID DATA

library(COVID19)
library(lubridate)

covid <- covid19(verbose = F)
covid <- covid[, c(28, 2, 4, 10)]
covid <- rename(covid, Country = "administrative_area_level_1")
covid <- drop_na(covid, population)
covid <- covid %>%
  filter(date >= "2020-01-27") %>%
  filter(date <= "2020-06-08")

#determining the week of the epidemic (where we count the 22/01 as the first week)
covid <- covid %>% 
  mutate(week = isoweek(date)-4)

#calculating the incidence of covid cases per 100,000 people in each week
weekly.incidence <- covid %>%
  group_by(Country, week) %>%
  mutate(incidence = (max(confirmed) - min(confirmed))/population*100000) %>%
  slice(1)

weekly.incidence <- ungroup(weekly.incidence)

weekly.incidence <- filter(weekly.incidence, incidence>0)

#calculating the mean incidence
weekly.incidence <- weekly.incidence %>%
  group_by(Country) %>%
  mutate(incidence.mean = mean(incidence)) %>%
  slice(1)

table(weekly.incidence$Country)

weekly.incidence <- weekly.incidence[, c(1,7)]


## MERGING WITH THE EXPENDITURE DATA

#adjusting names of the countries
data.all <- rename(data.all, Country = "Country or Area")
data.all$Country[data.all$Country == "Czechia"] <- "Czech Republic"
data.all$Country[data.all$Country == "Ethiopia [from 1993]"] <- "Ethiopia"
data.all$Country[data.all$Country == "Iran (Islamic Republic of)"] <- "Iran"
data.all$Country[data.all$Country == "Republic of Korea"] <- "Korea, South"
data.all$Country[data.all$Country == "Russian Federation"] <- "Russia"
data.all$Country[data.all$Country == "Eswatini"] <- "Swaziland"

#merging the two tibbles
data.all <- left_join(data.all, weekly.incidence, by="Country")
data.all <- drop_na(data.all, incidence.mean)

#descriptive analysis of the data - BOXPLOTS
data.all.melt <- data.all %>% gather(mean.alco, mean.cult, mean.rest, mean.trans, key = "sector", value = "percentage")
data.all.melt$sector[data.all.melt$sector == "mean.rest"] <- 1
data.all.melt$sector[data.all.melt$sector == "mean.cult"] <- 2
data.all.melt$sector[data.all.melt$sector == "mean.alco"] <- 4
data.all.melt$sector[data.all.melt$sector == "mean.trans"] <- 3

ggplot(data.all.melt, aes(x = sector, y = sqrt(percentage), colour = sector))+
  geom_boxplot()+
  theme_bw()+
  xlab("")+
  ylab("mean expenditure (sqrt)")+
  scale_color_discrete(name = "Expenditure categories", labels = c("1: Restaurants and hotels",
                                                                   "2: Culture and recreation",
                                                                   "3: Transport",
                                                                   "4: Alcohol, tobacco and narcotics" )) +
  ggtitle("Boxplots of mean expenditure in each of the categories")

#analysing outliers of the incidence.mean data
ggplot(data.all, aes(y = (incidence.mean), x = factor(0)))+
  geom_boxplot(colour = "orange")+
  theme(axis.text.x = element_blank()) +
  theme_bw()+
  xlab("") +
  ylab("mean incidence rate") + 
  ggtitle("Boxplot of mean incidence rate")

ggplot(data.all, aes(x=sqrt(incidence.mean))) +
  geom_histogram(colour = "orange", fill = "orange")+
  theme_bw()+
  xlab("mean incidence rate (sqrt)")+
  ylab("frequency")+
  ggtitle(("Histogram of the mean incidence rate"))
  
#excluding outliers
data.all <- filter(data.all, Country != "Belize")
data.all <- filter(data.all, Country != "South Sudan")
data.all <- filter(data.all, Country != "Andorra")

#fitting the individual LINEAR incidence.mean models
model.rest <- lm(incidence.mean ~ mean.rest, data.all)
model.cult <- lm(incidence.mean ~ mean.cult, data.all)
model.alco <- lm(incidence.mean ~ mean.alco, data.all)
model.trans <- lm(incidence.mean ~ mean.trans, data.all)

#fitting the individual transformed incidence.mean models
model.rest2 <- lm(incidence.mean ~ log(log(mean.rest)), data.all)
model.rest3 <- lm(incidence.mean ~ log(mean.rest), data.all)
model.rest4 <- lm(incidence.mean ~ sqrt(log(mean.rest)), data.all)
model.cult2 <- lm(incidence.mean ~ mean.cult + I(mean.cult^2), data.all)
model.cult3 <- lm(incidence.mean ~ exp(mean.cult), data.all)
model.cult4 <- lm(incidence.mean ~ log(mean.cult), data.all)
model.trans2 <- lm(incidence.mean ~ mean.trans + I(mean.trans^2) + I(mean.trans^3) + I(mean.trans^4), data.all)

#descriptive analysis of the incidence.mean data
rest <- ggplot(data.all, aes(x = mean.rest, y = incidence.mean)) +
  geom_point(colour = "#F8766D")+
  geom_abline(slope = model.rest$coefficients[2], intercept = model.rest$coefficients[1])+
  theme_bw()+
  xlab("mean expenditure")+
  ylab("mean incidence rate")+
  ggtitle("Restaurants and hotels")

trans <- ggplot(data.all, aes(x = mean.trans, y = incidence.mean)) +
  geom_point(colour = "#00BFC4")+
  geom_abline(slope = model.trans$coefficients[2], intercept = model.trans$coefficients[1])+
  theme_bw()+
  xlab("mean expenditure")+
  ylab("mean incidence rate")+
  ggtitle("Transport")

cult <- ggplot(data.all, aes(x = mean.cult, y = incidence.mean)) +
  geom_point(colour = "#7CAE00")+
  geom_abline(slope = model.cult$coefficients[2], intercept = model.cult$coefficients[1])+
  theme_bw()+
  xlab("mean expenditure")+
  ylab("mean incidence rate")+
  ggtitle("Culture and recreation")

alco <- ggplot(data.all[], aes(x = mean.alco, y = incidence.mean)) +
  geom_point(colour = "#C77CFF")+
  geom_abline(slope = model.alco$coefficients[2], intercept = model.alco$coefficients[1])+
  theme_bw()+
  xlab("mean expenditure")+
  ylab("mean incidence rate")+
  ggtitle("Alcohol, tobacco and narcotics")

ggarrange(rest, cult, trans, alco,
          ncol = 2, nrow = 2)

#fitting model for those which seem to be correlated
model.fit2 <- lm(incidence.mean ~ mean.cult + log(mean.rest), data.all)
model.fit4 <- lm(incidence.mean ~ mean.cult + mean.rest + mean.alco + mean.trans, data.all)
model.fit3 <- lm(incidence.mean ~ mean.cult + mean.rest + mean.alco, data.all)

#evaluating the fit of the model
AIC(model.rest)
AIC(model.rest2) ##
AIC(model.rest3)
AIC(model.rest4)
AIC(model.trans)
AIC(model.trans2)
AIC(model.cult) ##
AIC(model.cult2)
AIC(model.cult3)
AIC(model.cult4)
AIC(model.alco)
AIC(model.fit3)
AIC(model.fit2) #best fit
AIC(model.fit4)
AIC(model.cat.fit1)
AIC(model.cat.fit2)
AIC(model.cat.fit3)

confint(model.fit2, level = 0.95)
summary(model.fit2)

coef(model.fit2)

ggplot(data.all, aes(x = mean.cult, y = incidence.mean, colour = log(mean.rest)))+
  geom_point()+
  geom_abline(slope = model.fit2$coefficients[2], 
              intercept = model.fit2$coefficients[1]+model.fit2$coefficients[3]*log(min(data.all$mean.rest, na.rm = TRUE)))+
  geom_abline(slope = model.fit2$coefficients[2], 
              intercept = model.fit2$coefficients[1]+model.fit2$coefficients[3]*log(quantile(data.all$mean.rest, 0.25, na.rm = TRUE)))+
  geom_abline(slope = model.fit2$coefficients[2], 
              intercept = model.fit2$coefficients[1]+model.fit2$coefficients[3]*log(quantile(data.all$mean.rest, 0.5, na.rm = TRUE)))+
  geom_abline(slope = model.fit2$coefficients[2], 
              intercept = model.fit2$coefficients[1]+model.fit2$coefficients[3]*log(quantile(data.all$mean.rest, 0.75, na.rm = TRUE)))+
  geom_abline(slope = model.fit2$coefficients[2], 
              intercept = model.fit2$coefficients[1]+model.fit2$coefficients[3]*log(max(data.all$mean.rest, na.rm = TRUE)))

#'xgrid <- seq(min(data.all$mean.cult), max(data.all$mean.cult), 0.1)
#'ygrid <- seq(min(data.all$mean.rest), max(data.all$mean.rest), 0.01)
#'data <- expand.grid(mean.cult = xgrid, mean.rest = ygrid)
#'data$incidence.mean <- predict(model.fit2, data)
#'
#'ggplot(data, aes(mean.cult, mean.rest, z = incidence.mean, colour = incidence.mean)) + 
#'geom_contour_filled()+
#'geom_point(data = data.all, aes())

#attempt at categorizing the variables:
# ggplot(data.all, aes(x = mean.alco)) +
#   geom_histogram()
# 
# mean(data.all$mean.alco, na.rm = TRUE)
# 
# data.all <- data.all %>%
#   mutate(cat.alco = ifelse(mean.alco <= median(data.all$mean.alco, na.rm = TRUE), 1, 0 ))
# data.all <- data.all %>%
#   mutate(cat.trans = ifelse(mean.trans <= median(data.all$mean.trans, na.rm = TRUE), 1, 0 ))
# data.all <- data.all %>%
#   mutate(cat.rest = ifelse(mean.rest <= median(data.all$mean.rest, na.rm = TRUE), 1, 0 ))
# 
# model.cat.fit1 <- lm(incidence.mean ~ mean.cult + cat.alco, data.all)
# summary(model.cat.fit1)
# 
# model.cat.fit2 <- lm(incidence.mean ~ mean.cult +log(mean.rest) + cat.alco, data.all)
# model.cat.fit3 <- lm(incidence.mean ~ mean.cult + log(mean.rest) + as.factor(cat.alco) + as.factor(cat.trans), data.all)
# 
