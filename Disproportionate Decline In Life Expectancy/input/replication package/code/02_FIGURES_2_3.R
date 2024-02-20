## EMPTY THE ENVIRONMENT
rm(list = ls())

## LOAD PACKAGES
library(ggpubr)
library(ggplot2)
library(data.table)

#############################################################################################
###                               PULL ALL DATA FROM GITHUB                               ###
#############################################################################################

## SET DIRECTORY
mydir <- 'C:/Users/Russell/Desktop/Disproportionate Decline In Life Expectancy'

## READ DATA
life <- fread(paste0(mydir, '/data/life_table.csv'))
dths <- fread(paste0(mydir, '/data/all_cause_dth_rates.csv'), header=T)

#############################################################################################
###                                   CREATE FIGURE 2A                                    ###
#############################################################################################

## RESHPAPE LIFE EXP DATA
life <- melt(life, id.vars='year_id', variable.name='race', value.name='life_exp')
life[race == 'All', race := 'All races and origins']
life[, `Racial-ethnic group` := race]

## CREATE PANEL A
A<-ggplot(data=life[!(race %like% 'AIAN|Asian')], aes(x=year_id, y=life_exp, color=`Racial-ethnic group`)) +
  geom_point(aes(shape=`Racial-ethnic group`), size=4.1, alpha=0.9) + geom_line(alpha=0.56, size=1.5) +
  scale_y_continuous(name='Life expectancy at birth', limits=c(65, 85)) +
  scale_x_continuous(name='', breaks=seq(2006, 2020, by=2), limits=c(2005.5, 2021.5)) +
  scale_shape_manual(values=c(15:18)) + theme_minimal() +
  scale_color_manual(values=c('#186d38', '#144970', '#6b3f98', '#7f1621'))  +
  theme(axis.title=element_text(size=18), axis.text=element_text(size=14),
        legend.position='top', legend.justification='left',
        legend.text=element_text(size=14), legend.title=element_text(size=16, face='bold'))

#############################################################################################
###                                   CREATE FIGURE 2B                                    ###
#############################################################################################

## DATA FOR PANEL B
tmp <- dcast(data=life[year_id %in% 2019:2021], formula=race~year_id, value.var='life_exp')
tmp[, diff_2020_2019 := `2020`-`2019`]
tmp[, diff_2021_2020 := `2021`-`2020`]

## PREP FOR PLOTTING
tmp <- melt(tmp, id.vars=c('race'), measure.vars=c('diff_2020_2019', 'diff_2021_2020'), value.name='diff', variable.name='period')
tmp[, `Racial-ethnic group` := factor(race, levels=c('All races and origins', 'Hispanic', 'Non-Hispanic AIAN', 'Non-Hispanic Asian', 'Non-Hispanic Black', 'Non-Hispanic White'))]

## CREATE PANEL B
B<-ggplot(data=tmp, aes(x=`Racial-ethnic group`, y=diff, fill=period)) +
  geom_bar(stat='identity', position='dodge', alpha=0.71) +
  scale_y_continuous(name='Change (years)', breaks=-5:0, limits=c(-5.5, 0)) +
  scale_fill_manual(name='Time period', values=c('#243f29', '#186d38'), labels=c('2020-2019', '2021-2020')) + 
  theme_minimal() + labs(x='') +
  geom_text(aes(label=sprintf(diff, fmt='%.1f')), position=position_dodge(width=0.9), vjust=1.3, size=6) +
  theme(axis.title=element_text(size=18), axis.text=element_text(size=14),
        legend.text=element_text(size=12), legend.title=element_text(size=14, face='bold'))

## SAVE PANEL
pdf(paste0(mydir, '/results/FIGURE_2.pdf'), width=14, height=16)
  ggarrange(A, B, ncol=1, labels=c('A)', 'B)'), font.label=list(size=17))
dev.off()

#############################################################################################
###                                 PREP DATA FOR FIGURE 3                                ###
#############################################################################################

## PREP DATA
dths[, `:=` (`Indicator Name`=NULL, `Indicator Code`=NULL)]
setnames(dths, old=names(dths), new=c('location_name', 'iso3', paste0('year_', 1960:2022)))

## RESHAPE DATA
dths <- melt(data=dths, id.vars=c('location_name', 'iso3'), variable.name='year', value.name='dth_rate')
dths[, year_id := as.integer(tstrsplit(year, '_')[[2]])]

## SUBSET TO LOCS OF INTERESTS
dths <- dths[iso3 %in% c('USA', 'GBR', 'NZL', 'AUS', 'KOR', 'DEU')]
dths <- dths[year_id %in% 2013:2021]
dths <- dths[order(iso3, year_id)]

## RENAME
dths[iso3 == 'KOR', location_name := 'South Korea']
dths[, t:= 1:.N, by=iso3]

## GET PREDICTIONS
for(loc in unique(dths$iso3)) {
  tmp <- dths[iso3 == loc]
  reg <- lm(data=tmp[year_id < 2020], formula=dth_rate~t)
  dths[iso3 == loc, preds := predict.lm(reg, newdata=.SD)]
}

## CHANGE ORDER FOR PLOTTING
dths[, location_name := factor(location_name, levels=c('United States', 'United Kingdom', 'Germany', 'New Zealand', 'Australia', 'South Korea'))]

#############################################################################################
###                                   CREATE FIGURE 3A                                    ###
#############################################################################################

A<-ggplot(data=dths, aes(x=year_id, y=dth_rate)) +
  geom_point(size=1.7, color='#144970') +
  geom_line(alpha=0.25, size=0.95, color='#144970') +
  geom_line(aes(y=preds, color=as.factor(1)), linetype=2, size=0.82, alpha=0.76) +
  facet_wrap(~location_name) + theme_minimal() +
  scale_x_continuous(name='', limits=c(2012.5, 2021.5), breaks=2013:2021) +
  scale_y_continuous(name='All-cause death rate (per 1,000 population)', limits=c(4.5, 13)) +
  scale_color_manual(name='', values='black', label='Expected death rate') +
  theme(axis.title=element_text(size=16, face='bold'), axis.text=element_text(size=10),
        strip.text=element_text(size=15, face='bold'), legend.position='top', legend.justification='left',
        legend.text=element_text(size=14, face='bold'), legend.key.size = unit(4, "line"))

#############################################################################################
###                                   CREATE FIGURE 3B                                    ###
#############################################################################################

## PREP FOR PANEL B
tmp <- dths[year_id %in% 2020:2021]
tmp[, excess := dth_rate-preds]
tmp[, location_name := factor(location_name, levels=rev(c('United States', 'United Kingdom', 'Germany', 'New Zealand', 'Australia', 'South Korea')))]
tmp[year %like% '2020', year_id := 2023]
tmp[year %like% '2021', year_id := 2022]

## CREATE PANEL B
B<-ggplot(data=tmp, aes(x=location_name, y=excess, fill=as.factor(year_id))) +
  geom_bar(stat='identity', position='dodge', alpha=0.71) +
  scale_y_continuous(name='Excess death rate (per 1,000 population)', breaks=seq(-0.5, 1.5, by=0.25), limits=c(-0.62, 1.6)) +
  labs(x='', title='') + coord_flip() + theme_classic() +
  scale_fill_manual(name='Year', values=c('#a7cde0', '#1e78b4'), labels=c('2021', '2020')) +
  geom_text(aes(label=sprintf(excess, fmt='%.2f'), y=excess+ifelse(excess>=0, 0.01, -0.15)), 
            position=position_dodge(width = 0.9), hjust=-0.20, size=3) +
  theme(axis.title=element_text(size=16, face='bold'), axis.text=element_text(size=13),
        legend.text=element_text(size=12), legend.title=element_text(size=13, face='bold'))

## SAVE
pdf(paste0(mydir, '/results/FIGURE_3.pdf'), width=22)
  ggarrange(A, B, nrow=1, labels=c('A)', 'B)'), widths = c(1.20,1), heights=c(1.2,1))
dev.off()

