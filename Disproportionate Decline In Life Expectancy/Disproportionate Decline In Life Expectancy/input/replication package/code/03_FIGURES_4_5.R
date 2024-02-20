## EMPTY THE ENVIRONMENT
rm(list = ls())

## LOAD PACKAGES
library(ggpubr)
library(ggplot2)
library(readxl)
library(lubridate)
library(data.table)

#############################################################################################
###                               PULL ALL DATA FROM GITHUB                               ###
#############################################################################################

## SET DIRECTORY
mydir <- 'C:/Users/Russell/Desktop/Disproportionate Decline In Life Expectancy'

## READ COVID-19 CASE/MORTLAITY DATA
deaths <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
excess <- fread(paste0(mydir, '/data/IHME_excess_deaths.csv'))
cmr    <- fread(paste0(mydir, '/data/smr.csv'))
locs   <- fread(paste0(mydir, '/data/locs.csv'))
ghs    <- data.table(readxl::read_excel(paste0(mydir, '/data/GHS_index.xlsx')))
pops   <- fread(paste0(mydir, '/data/UN_pops.csv'))

#############################################################################################
###                               PROCESSES HOPKINS COVID DATA                            ###
#############################################################################################

## PREP FOR RESHAPING
setnames(deaths, old=c('Province/State', 'Country/Region'), new=c('province', 'location_name'))

## DO THE SAME FOR DEATHS
deaths <- melt(deaths, id.vars=c('province', 'location_name'), measure.vars=names(deaths)[names(deaths) %like% "/"], variable.name="date", value.name="deaths")
deaths <- deaths[date=='12/31/21']
deaths <- deaths[, .(total_deaths=sum(deaths)), by=.(location_name, date)]

## GET UN POPULATION DATA
pops <- pops[(Variant == 'Medium') & (Time == 2021)]
pops <- pops[, .(ISO3_code, Time, PopTotal)]
pops <- pops[ISO3_code != '']
setnames(pops, old=names(pops), new=c('iso3', 'year_id', 'population'))
pops[, population := population*1000]

## PREP CMR DATA
setnames(cmr, 
         old=c('Country', 'ISO3', 'SMR (United States of America)', 'OVERALL SCORE'), 
         new=c('location_name', 'iso3', 'cmr', 'ghs_score'))
cmr <- cmr[, .(location_name, iso3, ghs_score, cmr)]
cmr[, age_std_rate := (cmr*(253500/164384742))*100000]

#############################################################################################
###                                MERGE IN LOCATION DATA                                 ###
#############################################################################################

## RENAME LOCATIONS FOR MERGE
deaths[location_name == 'Bolivia', location_name := 'Bolivia (Plurinational State of)']
deaths[location_name == 'Brunei', location_name := 'Brunei Darussalam']
deaths[location_name == 'Burma', location_name := 'Myanmar']
deaths[location_name == "Congo (Kinshasa)", location_name := "Democratic Republic of the Congo"]
deaths[location_name == "Congo (Brazzaville)", location_name := "Congo"]
deaths[location_name == "West Bank and Gaza", location_name := "Palestine"]
deaths[location_name == "Taiwan*", location_name := "Taiwan (Province of China)"]
deaths[location_name == "US", location_name := "United States of America"]
deaths[location_name == "Cote d'Ivoire", location_name := "Côte d'Ivoire"]
deaths[location_name == "Iran", location_name := "Iran (Islamic Republic of)"]
deaths[location_name == "Korea, South", location_name := "Republic of Korea"]
deaths[location_name == "Korea, North", location_name := "Democratic People's Republic of Korea"]
deaths[location_name == "Laos", location_name := "Lao People's Democratic Republic"]
deaths[location_name == "Micronesia", location_name := "Micronesia (Federated States of)"]
deaths[location_name == "Moldova", location_name := "Republic of Moldova"]
deaths[location_name == "Russia", location_name := "Russian Federation"]
deaths[location_name == "Syria", location_name := "Syrian Arab Republic"]
deaths[location_name == "Tanzania", location_name := "United Republic of Tanzania"]
deaths[location_name == "Venezuela", location_name := "Venezuela (Bolivarian Republic of)"]
deaths[location_name == "Vietnam", location_name := "Viet Nam"]

## PROCESS LOCATION DATA
locs <- locs[!(location_id %in% c(533, 25344))]
locs[location_id == 155, location_name := 'Turkey']

## MERGE DATA
input <- merge(locs, deaths, by='location_name')
input <- merge(input, pops, by='iso3')

#############################################################################################
###                                  PROCESS REMAINING DATA                               ###
#############################################################################################

## PROCESS GHS DATA
setnames(ghs, old=c('Country', 'Year', 'OVERALL SCORE'), new=c('location_name', 'year_id', 'ghs_score'))

## PROCESS GHS DATA
ghs <- ghs[, .(location_name, year_id, ghs_score)]
ghs <- ghs[year_id == 2021]

## MERGE ON GHS_INDEX
input <- merge(input, ghs, by=c('location_name', 'year_id'))
input[, death_rate := (total_deaths/population)*100000]

## GET EXCESS DEATH DATA
excess <- excess[measure_name == 'excess_death_rate']
excess <- excess[, .(location_id, mean_value)]
setnames(excess, old='mean_value', new='excess_dth_rate')

## MERGE DATA
input <- merge(input, excess, by='location_id', all.x=T)
input <- input[population > 1000000] #REMOVE OUTLIER SMALL ISLAND COUNTRIES FOR CLARITY

## GET CORRELATIONS
cor.test(x=input$ghs_score, y=input$death_rate)
cor.test(x=input$ghs_score, y=input$excess_dth_rate)
cor.test(x=cmr$ghs_score, y=cmr$age_std_rate)

#############################################################################################
###                                    CREATE FIGURE 4                                    ###
#############################################################################################

## REPORTED DEATH RATE
A <-ggplot(data=input, aes(x=ghs_score, y=death_rate)) +
  geom_point(size=2.1, alpha=0.6) +
  geom_smooth(method='lm', size=1.01, color='darkgreen', fill='limegreen', alpha=0.15) +
  theme_bw() + annotate('text', x=76, y=510, label='r=0.51, p<0.0001', size=13/.pt) +
  scale_x_continuous(name='', breaks=seq(10, 80, by=10), limits=c(10, 80)) +
  scale_y_continuous(name='Cumulative reported COVID-19 death rate', breaks=seq(0, 600, by=100), limits=c(-40, 625)) +
  theme(axis.title=element_text(size=14.5), axis.text=element_text(size=13))

## EXCESS DEATH RATE
B <-ggplot(data=input, aes(x=ghs_score, y=excess_dth_rate)) +
  geom_point(size=2.1, alpha=0.6) +
  geom_smooth(method='lm', size=1.01, color='darkgreen', fill='limegreen', alpha=0.15) +
  theme_bw() + annotate('text', x=76, y=490, label='r=0.04, p=0.637', size=13/.pt) +
  scale_x_continuous(name='', breaks=seq(10, 80, by=10), limits=c(10, 80)) +
  scale_y_continuous(name='Excess death rate', breaks=seq(0, 700, by=100), limits=c(-60, 700)) +
  theme(axis.title=element_text(size=14.5), axis.text=element_text(size=13))

## AGE-STD EXCESS DEATHS; REMOVE OUTLIERS FOR CLARITY
C <- ggplot(data=cmr[age_std_rate < 1000], aes(x=ghs_score, y=age_std_rate)) +
  geom_point(size=2.1, alpha=0.6) +
  geom_smooth(method='lm', size=1.01, color='darkgreen', fill='limegreen', alpha=0.15) +
  theme_bw() + annotate('text', x=75, y=760, label='r=-0.36, p<0.0001', size=13/.pt) +
  scale_x_continuous(name='Global Health Security (GHS) Index', breaks=seq(10, 80, by=10), limits=c(10, 80)) +
  scale_y_continuous(name='Indirectly age-standardized excess death rate', breaks=seq(0, 1000, by=100), limits=c(-60, 1040)) +
  theme(axis.title=element_text(size=14.5), axis.text=element_text(size=13))

## SAVE PANEL FIGURE
pdf(paste0(mydir, '/results/FIGURE_4.pdf'), width=9, height=16)
  ggarrange(A, B, C, ncol=1, labels=c('A)', 'B)', 'C)'))
dev.off()

#############################################################################################
###                                    CREATE FIGURE 5                                    ###
#############################################################################################

## PULL PREDICTIONS FOR PLOTTING
reg  <- lm(data=cmr, formula=age_std_rate~ghs_score)
preds<- data.table(ghs_score=seq(0, 100, 0.01))
preds[, pred := predict(reg, newdata=.SD)]
cmr[iso3 == 'USA', location_name := 'United States']

pdf(paste0(mydir, '/results/FIGURE_5.pdf'), width=12)
ggplot(data=cmr[ghs_score >= 60], aes(x=ghs_score, y=age_std_rate)) +
  geom_point(data=cmr[(ghs_score >= 60) & (iso3 != 'USA')], alpha=0.7, size=1.72) + 
  geom_point(data=cmr[(iso3 == 'USA')], alpha=0.7, color='red', size=1.72) +
  geom_text(data=cmr[(ghs_score >= 60) & (iso3 != 'USA')], aes(label=location_name),hjust=0.5, vjust=-0.7, alpha=0.75, size=2.63) +
  geom_text(data=cmr[(iso3 == 'USA')],aes(label=location_name), hjust=0.5, vjust=-0.7,  alpha=0.5, color='red', size=2.63) +
  geom_line(data=preds[(ghs_score > 60) & (ghs_score < 76)], aes(y=pred), color='darkgreen', alpha=0.4, size=0.941) +
  scale_y_continuous(name='Indirectly age-standardized excess death rate',
                     limits=c(-60, 520), breaks=seq(-50, 500, by=50)) +
  scale_x_continuous(name='Global Health Security (GHS) Index', breaks=seq(60, 76, by=2), limits=c(59, 77)) +
  theme_minimal() +
  theme(axis.title=element_text(size=14.5, face='bold'), axis.text=element_text(size=12.5))
dev.off()
  

