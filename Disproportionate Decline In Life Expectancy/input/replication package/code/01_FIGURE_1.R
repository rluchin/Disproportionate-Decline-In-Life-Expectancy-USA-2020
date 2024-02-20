## EMPTY THE ENVIRONMENT
rm(list = ls())

## LOAD PACKAGES
library(data.table)
library(ggplot2)
library(sf)
library(RColorBrewer)

#############################################################################################
###                                     PULL GHS DATA                                     ###
#############################################################################################

## SET DIRECTORY
##REPLACE THIS WITH LOCAL DIRECTORY WHEN REPLICATING
mydir <- 'C:/Users/Russell/Desktop/Disproportionate Decline In Life Expectancy'

## READ IN DATA
globe<- st_read(paste0(mydir, '/data/world'))
locs <- fread(paste0(mydir, '/data/locs.csv'))
ghs  <- data.table(readxl::read_excel(paste0(mydir, '/data/GHS_index.xlsx')))

## GET ISO3 INFORMATION
isos <- fread('https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv')
isos <- isos[, .(`alpha-2`, `alpha-3`)]
setnames(isos, old=names(isos), new=c('AFF_ISO', 'iso3'))

## PROCESS LOCATION DATA
locs <- locs[!(location_id %in% c(533, 25344))]
locs[location_id == 155, location_name := 'Turkey']

## PROCESS GHS DATA
setnames(ghs, old=c('Country', 'Year', 'OVERALL SCORE'), new=c('location_name', 'year', 'ghs_score'))

## PROCESS GHS DATA
ghs <- ghs[, .(location_name, year, ghs_score)]
ghs <- ghs[year == 2021]

## MERGE ON LOCATION INFORMATION
locs <- locs[, .(location_name, iso3)]
ghs <- merge(locs, ghs, by='location_name', all.y=T)
ghs[location_name == 'Liechtenstein', iso3 := 'LIE']

## GET ISO INFORMATION 
ghs <- merge(isos, ghs, by='iso3')
ghs[iso3=='NAM', AFF_ISO := 'NA']

## GET CATEGORIES FOR PLOTTING
ghs[, cats := cut(ghs_score, breaks=c(15, 25, 30, 35, 40, 45, 50, 60, 70, 80), include.lowest=T, 
                    labels=c('16.0 to 25.0', '25.0 to 30.0', '30.0 to 35.0', '35.0 to 40.0', '40.0 to 45.0', 
                             '45.0 to 50.0', '50.0 to 60.0', '60.0 to 70.0', '70.0 to 76.0'))]

#############################################################################################
###                                    CREATE PLOTS                                       ###
#############################################################################################

## PULL SHAPEFILE
globe <- globe[globe$ISO != 'AQ',]
globe <- merge(globe, ghs, by='AFF_ISO', all.x=T)

## SET COLORS FOR MAP
cols <- c('#2166AC', '#4393C3', '#92C5DE', '#D1E5F0', "#F7F7F7", "#FCBBA1", "#FC9272", "#FB6A4A", "#EF3B2C", "#CB181D", "#A50F15")

## CREATE MAP
pdf(file=paste0(mydir, '/results/FIGURE_1.pdf'), width=11)
  ggplot(data=globe, aes(fill=cats)) + 
    geom_sf(size=0.001) + theme_minimal()+
    scale_fill_manual(name='Global health security score', values=brewer.pal(10, 'RdBu'), na.translate=F) +
    guides(fill=guide_legend(ncol=2)) +
    theme(axis.text=element_blank(), axis.ticks=element_blank(),
          rect=element_blank(), legend.position=c(0.15, 0.30),
          legend.key.size=unit(0.45, 'cm'), legend.title=element_text(size=9),
          legend.text=element_text(size=8), panel.grid=element_blank())
dev.off()

