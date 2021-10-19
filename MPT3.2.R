setwd("C:/Users/Prol/Documents/R/wd/MPT/3")

library(ggplot2)
library(xts)
library(tidyverse)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(tseries)
library(ggrepel)
library(gridExtra)
library(ggExtra)
library(maptools) # for mapping
library(doBy)
library(gpclib)
# gpclibPermit()
library(RColorBrewer)
library(corrplot)
library(ggridges)
library(scales)
library(viridis)
library(ggpubr)
#devtools::install_github("tidyverse/dplyr")

write.png<- function(object, filename, width=5, height=4, pointsize=12, res=300){
  png(filename= filename, type="cairo",units="in", width=width, height=height, pointsize=pointsize, res=res)
  plot(object)
  dev.off()
  object
}

mytheme <- function(base_size = 12, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.border     = element_blank(),
      axis.line        = element_blank(),
      axis.ticks = element_line(color = "grey"),
      panel.grid.major = element_line(),
      panel.grid.major.x = element_line(colour = "grey", size = 0.1, linetype = "dotted"),
      panel.grid.major.y = element_line(colour = "grey", size = 0.1, linetype = "dotted"),
      panel.grid.minor = element_line(),
      panel.grid.minor.x = element_line(colour = "grey", size = 0.1, linetype = "dotted"),
      panel.grid.minor.y = element_line(colour = "grey", size = 0.1, linetype = "dotted"),
      strip.background = element_blank(),
      legend.key       = element_blank()
    )
}

xts2df <- function(x) {
  data.frame(date=index(x), coredata(x))
}



# 1. Data ##################################################################
# 1.1. Import  ####
# hourly wind capacity factors, transform to xts and save as .RDS

# Europe
europe<- read.csv("./data/raw/Ninja_2021_Simulations_2010-19_Europe.csv",
                  sep = ",", header = T)
europe_dates<- as.POSIXct(europe$time_utc, format = "%Y-%m-%d %H:%M:%OS",
                          tz = "UTC")
europe_xts<- xts(europe[,-1], order.by = europe_dates, tz = "UTC")
# saveRDS(europe_xts, "./data/clean/europe_xts.rds")
# saveRDS(europe, "./data/clean/europe.rds")

# USA
usa<- read.csv("./data/raw/Ninja_2021_Simulations_2010-19_United_States.csv",
               sep = ",", header = T)
usa<- subset(usa, select = -c(AK,HI)) # remove alaska and hawai
  
usa_dates<- as.POSIXct(usa$time, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
usa<- rename(usa, DL = DE) # change names to avoid code repetition
usa_xts<- xts(usa[,-1], order.by = usa_dates, tz = "UTC")
# saveRDS(usa_xts, "./data/clean/usa_xts.rds")
# saveRDS(usa, "./data/clean/usa.rds")

# China
china<- read.csv("./data/raw/Ninja_2021_Simulations_2010-19_China.csv", 
                 sep = ",", header = T)
china_dates<- as.POSIXct(china$time, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
china<- rename(china, SG = SD, EI = HU, SN = SC) # change names to avoid code repetition
china_xts<- xts(china[,-1], order.by = china_dates, tz = "UTC")
# saveRDS(china_xts, "./data/clean/china_xts.rds")
# saveRDS(china, "./data/clean/china.rds")

# alaska only in the intercontinental configuration
alaska<- read.csv("./data/raw/Ninja_2021_Simulations_2010-19_United_States.csv",
                  sep = ",", header = T)[ , c("time_utc", "AK")]
AK_xts<- xts(alaska[,-1], order.by = usa_dates, tz = "UTC")

all_xts<- merge(europe_xts, AK_xts, usa_xts, china_xts)
# saveRDS(all_xts, "./data/clean/all_xts.rds")

all_df<- all_xts %>%  fortify()
# write.csv(all_df, "./data/clean/all_df.xts")


# 1.2. SumStats ####
eu<- readRDS("./data/clean/europe_xts.rds")
us<- readRDS("./data/clean/usa_xts.rds")
cn<- readRDS("./data/clean/china_xts.rds")
all<- readRDS("./data/clean/all_xts.rds")

summarize_stats<- function(data){
  data %>% fortify() %>% gather(country, cf,-Index) %>% group_by(country) %>%
    summarize(mean = mean(cf), sd = sd(cf), cv = sd(cf)/mean(cf),
           low = mean(cf) - sd(cf), high = mean(cf) + sd(cf))
}

eu_ss<- summarize_stats(eu) %>% mutate(region = "Europe")
us_ss<- summarize_stats(us) %>% mutate(region = "USA")
cn_ss<- summarize_stats(cn) %>% mutate(region = "China")

ss<- rbind(eu_ss, us_ss, cn_ss)
# saveRDS(ss, "./data/results/sumstats.rds")
# write.csv(ss, "./data/results/sumstats.csv")

# mean capacity factor summary statistics
p_cf<- ggplot(ss, aes(x = reorder(country, mean), y = mean*100)) +
  geom_point() +
  labs(x = "Province/Country/State", y = "Mean Capacity Factor (%)") +
  coord_flip() +
  facet_wrap(~region, scales = "free") +
  mytheme() 

write.png(p_cf, "figures/p_cf.png", height = 6)

# coefficient of variation summary statistics
p_cv<- ggplot(ss, aes(x = reorder(country, cv), y = cv)) +
  geom_point() +
  labs(x = "Province/Country/State", y = "Coefficient of Variation") +
  coord_flip() +
  facet_wrap(~region, scales = "free") +
  mytheme() 

write.png(p_cv, "figures/p_cv.png", height = 6)

# relationship coefficient of variation and capacity factor ola
p_ss<- ggplot(ss, aes(x = cv, y = mean*100)) +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  geom_point(aes(color = region), alpha = .6) +
  scale_x_log10() +
  labs(x = "Coefficient of Variation (log scale)", y = "Mean Capacity Factor (%)") +
  annotate("text", x = 1.09, y = 42.5, label = "China", color = "#F8766D") +
  annotate("text", x = 1.09, y = 37.5, label = "Europe", color = "#00BA38") +
  annotate("text", x = 1.09, y = 32.5, label = "USA", color = "#619CFF") +
  mytheme() +
  theme(legend.position = "none", aspect.ratio = 1)

write.png(p_ss, "p_SumStats.png")

p_ss2<- ggMarginal(p_ss, type = "density",
                   groupColour = TRUE, groupFill = TRUE, alpha = .05)



write.png(p_ss2, "p_SumStats2.png", height = 8, width = 8)

# Ridge plot chao
ggplot(ss, aes(x = cv, y = region, fill = region)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
  
# corrplots chao
corrplot(cor(us), method="circle", type="upper", order="hclust", tl.col="#984EA3")

# 2. Optimization ##########################################################
# 2.1. Regions ####
# run this section
eu<- readRDS("./data/clean/europe_xts.rds")
us<- readRDS("./data/clean/usa_xts.rds")
cn<- readRDS("./data/clean/china_xts.rds")
all<- readRDS("./data/clean/all_xts.rds") # the 3 + alaska


# Define regions
# Europe
nord<- eu[, c("DK", "NO", "SE", "FI", "LT", "EE", "LV")]
epex<- eu[, c("GB", "FR", "BE", "NL", "DE", "CH", "AT")] # no LX
mibel<- eu[, c("ES", "PT")]

# westeu<- merge(nord, epex, eu[, c("ES", "IT", "PT")])

# USA
wecc<- us[, c("WA", "OR", "CA", "ID", "MT", "WY", "NV", "UT", "CO", "AZ", "NM")]
mro<- us[, c("ND", "MN", "WI", "SD", "NE", "IA", "KS", "OK")]
serc<- us[, c("MO", "IL", "KY", "VA", "AR", "TN", "NC",
              "LA", "MS", "AL", "GA", "SC", "FL")]
rf<- us[, c("MI", "IN", "OH", "PA", "WV", "NJ", "DL", "MD", "DC")]
npcc<- us[, c("NY", "VT", "ME", "NH", "MA", "CT")] # no "RI"

# eastus<- merge(mro, serc, rf, npcc)

# China
northwest<- cn[, c("XJ", "QH", "GS", "NX", "SA")]
central<- cn[, c("SN", "CQ", "EI", "HE", "HN", "JX")]
south<- cn[, c("YN", "GZ", "GX", "HA", "GD")]
east<- cn[, c("AH", "JS", "SH", "ZJ", "FJ")]
# north<- cn[, c("SX", "BJ", "HB", "TJ", "SG", "NMW")]
# northeast<- cn[, c("HL", "JL", "LN", "NME")]
# merge north and northeast into northeast
northeast<- cn[, c("SX", "BJ", "HB", "TJ", "SG", "NMW", "HL", "JL", "LN", "NME")]
eastcncn<- cn[, c("HA", "GD", "FJ", "ZJ", "SH", "JS", "SG", "HB", "TJ", "BJ")]
centralcn<- cn[, c("HE", "AH", "EI", "HN", "JX")]
northeastcn<- cn[, c("LN", "JL", "HL")]
westcn<- cn[, c("XJ", "NMW", "NME", "QH", "GS", "NX", "SA",
                "XZ", "SN", "CQ", "YN", "GZ", "GX")]

regions<- list(nord, epex, mibel, # level 1 europe
               wecc, mro, serc, rf, npcc, # level 1 usa
               northwest, central, south, east, northeast, # l 1 china, north chao
               # westeu, eastus, # level 2 eu & us
               # eastcncn, centralcn, northeastcn, westcn, # l2 china
               eu, us, cn, all) # level 3
# saveRDS(regions, "./data/results/regions.rds")
# regions<- readRDS("./data/results/regions.rds")
regionsnames<- c("nord", "epex", "mibel", # level 1 europe
                 "wecc", "mro", "serc", "rf", "npcc", # level 1 usa
                 "northwest", "central", "south", "east", "northeast", # l 1 china
                 # "westeu", "eastus", # level 2 eu & us
                 # "eastcncn", "centralcn", "northeastcn", "westcn", # l2 china
                 "eu", "us", "cn", "all") # level 3
# saveRDS(regionsnames, "./data/results/regionsnames.rds")
# regionsnames<- readRDS("./data/results/regionsnames.rds")

# color palette
show_col(viridis_pal(option = "cividis")(8)) # 
show_col(viridis_pal(option = "viridis")(8)) # 
show_col(viridis_pal(option = "plasma")(8)) # usa
show_col(viridis_pal(option = "mako")(8)) # europe
show_col(viridis_pal(option = "rocket")(8)) # china

# all cn eu usa
allcolors<- c("black", "#E13342FF", "#37659EFF", "#B93289FF")

# CHINA (2)
cncolors<- c("#701F57FF", "#E13342FF", "#36193EFF",
             "#AE1759FF", "#F37651FF", "#F6B48EFF")

# EUROPE (2)
eucolors<- c("#348FA7FF", "#37659EFF", "#40B7ADFF", "#8AD9B1FF")

# USA (3)
uscolors<- c("#5402A3FF", "#8B0AA5FF","#F48849FF",
             "#DB5C68FF", "#B93289FF", "#FEBC2AFF")

show_col(allcolors)
show_col(uscolors)

# # consistent way to assing colors across plots # chao bc different levels
# mycolors<- c("darkgrey", "black", # Autarky, Intercontinental
#              "#701F57FF", "#E13342FF", "#36193EFF", # china continent 
#              "#AE1759FF", "#F37651FF", "#F6B48EFF", # in alphabetical order
#              "#348FA7FF", "#37659EFF", "#40B7ADFF", "#8AD9B1FF", # europe continent
#              "#5402A3FF", "#8B0AA5FF","#F48849FF", # us continent
#              "#DB5C68FF", "#B93289FF", "#FEBC2AFF")
# names(mycolors) <- levels(df$factor)
# colScale <- scale_colour_manual(name = "factor",values = mycolors)
# # 


# 2.2. Run ####

# import clean data
eu<- readRDS("./data/clean/europe_xts.rds")
us<- readRDS("./data/clean/usa_xts.rds")
cn<- readRDS("./data/clean/china_xts.rds")
all<- readRDS("./data/clean/all_xts.rds")

# define optimization function (data in xts, only argument)
optimize_portfolio<- function(data){
  # find min. value for which the optim. is defined
  for (i in seq(.01, 1, .01)) {
    if (class(tryCatch(portfolio.optim(x = data, i), 
                       error = function(e) "chao")) == "list")
    {min<- i 
    break}
  }
  
  # find max. value for which the optim. is defined
  for (i in seq(.01, 1, .01)) {
    if (class(tryCatch(portfolio.optim(x = data, i), 
                       error=function(e) "chao")) == "list")
    {max<- i}
  }
  
  # create vectors/matrices to fill with results
  # create grid of returns for the frontier
  grid <- seq(from = min, to = max, length.out = 50)
  # Create an empty matrix to store portfolio "returns" (CF)
  returns <- matrix(NA, 50, length(data)/ncol(data))
  # Create empty vectors to store means and std. deviations
  means <- sds <- rep(NA, length(grid)) 
  # Create an empty matrix to store weights
  weights <- matrix(NA, 50, ncol(data)) 
  names(weights)<- colnames(data)
  
  # run optimization model
  for(i in 1:length(grid)) {
    opt <- portfolio.optim(x = data, pm = grid[i]) # default is long_only (shorts = F)
    returns[i, ]<- opt$px # matrix of portfolio "returns" (CF)
    means[i] <- opt$pm # vector of mean portfolio capacity factor
    sds[i] <- opt$ps # vector of portfolio sd
    weights[i, ] <- opt$pw # matrix of weights
  }
  
  # calculate sharpe ratio and weights of extreme portfolios
  sharpe <- means/sds # Sharpe ratio 
  
  # portfolio shares for the three extreme portfolios
  weights_sharpe <- weights[sharpe == max(sharpe), ] # optimal i.e. max sharpe
  weights_minvar <- weights[sds == min(sds), ] # min. var
  weights_maxmean <- weights[means == max(means), ] # max. return
  
  # returns of each of the extreme portfolios
  returns_sharpe <- returns[sharpe == max(sharpe), ] # maxsharpr
  returns_minvar <- returns[sds == min(sds), ] # minvar
  returns_maxmean <- returns[means == max(means), ] # maxret
  
  # name weights and returns
  names(weights_sharpe)<-names(weights_minvar)<-names(weights_maxmean)<-colnames(data)
  names(returns_sharpe)<-names(returns_minvar)<-names(returns_maxmean)<-colnames(data)
  
  # return a list of results
  opt<- list(means = means, sds = sds, sharpe = sharpe, # 1 obs/portfolio
             weights = weights,returns = returns, # 1 matrix/portfolio
             weights_sharpe = weights_sharpe, # weights of extreme portfolios
             weights_minvar = weights_minvar, weights_maxmean = weights_maxmean,
             returns_sharpe = returns_sharpe, # returns of extreme portfolios
             returns_minvar = returns_minvar, returns_maxmean = returns_maxmean)
}

# run all
opt_list<- lapply(regions, optimize_portfolio)
names(opt_list)<- regionsnames

str(opt_list)

# saveRDS(opt_list, "./data/results/opt_list.rds")


# 3. Analysis ##############################################################
# import optimization results
opt_list<- readRDS("./data/results/opt_list.rds")
# import regions definitions
regions<- readRDS("./data/results/regions.rds")
regionsnames<- readRDS("./data/results/regionsnames.rds")
# import xts
eu<- readRDS("./data/clean/europe_xts.rds")
us<- readRDS("./data/clean/usa_xts.rds")
cn<- readRDS("./data/clean/china_xts.rds")
all<- readRDS("./data/clean/all_xts.rds")
# import df
europe<- readRDS("./data/clean/europe.rds")
usa<- readRDS("./data/clean/usa.rds")
china<- readRDS("./data/clean/china.rds")
# import summary statistics
ss<- readRDS("./data/results/sumstats.rds")

# 3.1. Frontiers ####
# efficient frontiers
opt_list <- readRDS("./data/results/opt_list.rds")
front_list<- list()
for (i in opt_list) {
  # create dataframe with efficient frontier data
  frontier<- tibble(sd = i$sds, 
                    cf = i$means, 
                    sr = i$sharpe)  
  # select only the upper part of the frontier
  cfcut<- frontier[frontier$sd == min(frontier$sd), ]$cf
  frontier<- frontier[frontier$cf >= cfcut, ]
  front_list[[length(front_list) + 1]]<- frontier
}

names(front_list)<- regionsnames
front_list<- Map(cbind, front_list, region = names(front_list))


frontiers<- bind_rows(front_list)
# saveRDS(frontiers, "./data/results/frontiers.rds")

# Optimal portfolio: maximum sharpe ratio
sharpe<-  frontiers %>% group_by(region) %>%
  filter(sr == max(sr))


# china
cnnames<- c("cn", "northwest", "central", "south", "east", "northeast")
cnfront<- frontiers %>% filter(region %in% cnnames) %>%
  mutate(sd = 100*sd, cf = 100*cf)
cnsharpe<- sharpe %>% filter(region %in% cnnames)%>%
  mutate(sd = 100*sd, cf = 100*cf)
cnpoints<- ss %>% filter(region == "China") %>% select(-region) %>%
  mutate(sd = 100*sd, mean = 100*mean)

cnfrontp<- ggplot(cnfront, aes(x = sd, y = cf, color = region)) + 
  geom_line(size = 1) +
  geom_point(data = cnsharpe, aes(x = sd, y = cf, color = region), size = 2) +
  geom_point(data = cnpoints, aes(x = sd, y = mean), 
             inherit.aes = F, color = "darkgrey") +
  geom_text_repel(data = cnpoints, aes(x = sd, y = mean, label = country),
                  , inherit.aes = F, , color = "darkgrey") +
  labs(x = "Volatility (SD)", y = "Capacity factor (%)", subtitle = "(a)") +
  annotate("text", x = 8.5, y = 25, label = "China", color = cncolors[2]) +
  annotate("text", x = 15.5, y = 24.5, label = "Central", color = cncolors[1]) +
  annotate("text", x = 11, y = 27, label = "East", color = cncolors[3]) +
  annotate("text", x = 9.5, y = 23, label = "Northeast", color = cncolors[4]) +
  annotate("text", x = 9.5, y = 24, label = "Northwest", color = cncolors[5]) +
  annotate("text", x = 9, y = 22, label = "South", color = cncolors[6]) +
  scale_color_manual(values = cncolors) +
  mytheme() +
  theme(legend.position = "none")

write.png(cnfrontp, "./figures/cnfront.png")

# europe
eunames<- c("eu", "nord", "epex", "mibel")
eufront<- frontiers %>% filter(region %in% eunames) %>%
  mutate(sd = 100*sd, cf = 100*cf)
eusharpe<- sharpe %>% filter(region %in% eunames) %>%
  mutate(sd = 100*sd, cf = 100*cf)
eupoints<- ss %>% filter(region == "Europe") %>% select(-region) %>%
  mutate(sd = 100*sd, mean = 100*mean)

eufrontp<- ggplot(eufront, aes(x = sd, y = cf, color = region)) + 
  geom_line(size = 1) +
  geom_point(data = eusharpe, aes(x = sd, y = cf, color = region),
             size = 2) +
  geom_point(data = eupoints, aes(x = sd, y = mean), 
             inherit.aes = F, color = "darkgrey") +
  geom_text_repel(data = eupoints, aes(x = sd, y = mean, label = country),
                  , inherit.aes = F, , color = "darkgrey") +
  labs(x = "Volatility (SD)", y = "Capacity factor (%)", subtitle = "(b)") +
  annotate("text", x = 10.5, y = 25, label = "Epex Spot", color = eucolors[1]) +
  annotate("text", x = 7, y = 26, label = "Europe", color = eucolors[2]) +
  annotate("text", x = 15, y = 25, label = "Mibel", color = eucolors[3]) +
  annotate("text", x = 12, y = 27.5, label = "Nord Pool", color = eucolors[4]) +
  scale_color_manual(values = eucolors) +
  mytheme() +
  theme(legend.position = "none")

write.png(eufrontp, "./figures/eufront.png")

# us
usnames<- c("wecc", "mro", "serc", "rf", "npcc", "us")
usfront<- frontiers %>% filter(region %in% usnames) %>%
  mutate(sd = 100*sd, cf = 100*cf)
ussharpe<- sharpe %>% filter(region %in% usnames) %>%
  mutate(sd = 100*sd, cf = 100*cf)
uspoints<- ss %>% filter(region == "USA") %>% select(-region) %>%
  mutate(sd = 100*sd, mean = 100*mean)

usfrontp<- ggplot(usfront, aes(x = sd, y = cf, color = region)) + 
  geom_line(size = 1) +
  geom_point(data = ussharpe, aes(x = sd, y = cf, color = region),
             size = 2) +
  geom_point(data = uspoints, aes(x = sd, y = mean), 
             inherit.aes = F, color = "darkgrey") +
  geom_text_repel(data = uspoints, aes(x = sd, y = mean, label = country),
                  , inherit.aes = F, , color = "darkgrey") +
  labs(x = "Volatility (SD)", y = "Capacity factor (%)", subtitle = "(c)") +
  annotate("text", x = 15, y = 38, label = "MRO", color = uscolors[1]) +
  annotate("text", x = 18, y = 27, label = "NPCC", color = uscolors[2]) +
  annotate("text", x = 16, y = 29.5, label = "RF", color = uscolors[3]) +
  annotate("text", x = 12, y = 20, label = "SERC", color = uscolors[4]) +
  annotate("text", x = 7, y = 35, label = "USA", color = uscolors[5]) +
  annotate("text", x = 11, y = 32, label = "WECC", color = uscolors[6]) +
  scale_color_manual(values = uscolors) +
  mytheme() +
  theme(legend.position = "none")

write.png(usfrontp, "./figures/usfront.png")

# global
allnames<- c("cn", "eu", "us", "all")
allfront<- frontiers %>% filter(region %in% allnames) %>%
  mutate(sd = 100*sd, cf = 100*cf)
allsharpe<- sharpe %>% filter(region %in% allnames) %>%
  mutate(sd = 100*sd, cf = 100*cf)
allpoints<- ss %>% filter(region == "all") %>% select(-region) %>%
  mutate(sd = 100*sd, mean = 100*mean)

allfrontp<- ggplot(allfront, aes(x = sd, y = cf, color = region)) + 
  geom_line(size = 1) +
  geom_point(data = allsharpe, aes(x = sd, y = cf, color = region),
             size = 2) +
  # geom_point(data = allpoints, aes(x = sd, y = mean), 
  #           inherit.aes = F, color = "darkgrey") +
  #geom_point(data = min(opt_list$all$sds), aes(x = sd, y = mean), 
  #           inherit.aes = F, color = "darkgrey") +
  geom_text_repel(data = allpoints, aes(x = sd, y = mean, label = country),
                  , inherit.aes = F, , color = "darkgrey") +
  labs(x = "Volatility (SD)", y = "Capacity factor (%)", subtitle = "(a)") +
  annotate("text", x = 5, y = 35, label = "Inter- \n continental",
           color = allcolors[1]) +
  annotate("text", x = 12, y = 31, label = "China", color = allcolors[2]) +
  annotate("text", x = 10.5, y = 25, label = "Europe", color = allcolors[3]) +
  annotate("text", x = 7.5, y = 31, label = "USA", color = allcolors[4]) +
  # geom_segment(aes(x = .2, y = 38, xend = .25, yend = 45),
  #             arrow = arrow(length = unit(0.5, "cm")), color = "darkgrey") +
  # annotate("text", x = .2, y = 36, 
  #         label = "Region with highest CF: \n Nebraska, USA", 
  #         color = "darkgrey") +
  #geom_point(x = .25, y = 45, color = "darkgrey") +
  scale_color_manual(values = allcolors) +
  mytheme() +
  theme(legend.position = "none")

write.png(allfrontp, "./figures/allfront.png")

# 3.2. CumProb ####
# need to run 1.1 for raw data

# global extremes (as densities, chao cumprob)
cumprobpext<- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  stat_function(fun = ecdf(opt_list$all$returns_sharpe*100), color = "green") +
  # stat_function(fun = ecdf(usa$NE*100), color = "red") + # maxmean global & USA
  stat_function(fun = ecdf(opt_list$all$returns_minvar*100), color = "blue") +
  stat_function(fun = ecdf(opt_list$eu$returns_sharpe*100), color = "grey") +
  stat_function(fun = ecdf(opt_list$eu$returns_minvar*100), color = "darkgrey") +
  stat_function(fun = ecdf(opt_list$eu$returns_maxmean*100), color = "lightgrey") +
  xlim(0,100) +
  xlab("Capacity factor (%)") + ylab("Cumulative probability") +
  mytheme()

write.png(cumprobpext, "./figures/cumprobext.png")

# global
cumprobp<- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  stat_function(fun = ecdf(europe$DE*100), color = "grey") +
  stat_function(fun = ecdf(europe$MK*100), color = "grey") +
  # stat_function(fun = ecdf(china$YN*100), color = "grey") +
  stat_function(fun = ecdf(usa$NE*100), color = "grey") +
  stat_function(fun = ecdf(opt_list$cn$returns_sharpe*100),
                color = allcolors[2], size = 1) +
  stat_function(fun = ecdf(opt_list$eu$returns_sharpe*100),
                color = allcolors[3], size = 1) +
  stat_function(fun = ecdf(opt_list$us$returns_sharpe*100),
                color = allcolors[4], size = 1) +
  stat_function(fun = ecdf(opt_list$all$returns_sharpe*100), 
                color = "black", size = 1) +
  annotate("text", x = 30, y = .05, label = "Intercontinental", color = "black") +
  annotate("text", x = 25, y = .75, label = "China", color = allcolors[2]) +
  annotate("text", x = 14, y = .24, label = "Europe", color = allcolors[3]) +
  annotate("text", x = 42, y = .8, label = "USA", color = allcolors[4]) +
  annotate("text", x = 40, y = .24, 
           label = "Region with highest CF: \n Nebraska, USA", 
           color = "darkgrey") +
  annotate("text", x = 8, y = .95, 
           label = "Region with lowest CF: \n North Makedonia, \n Europe", 
           color = "darkgrey") +
  annotate("text", x = 11, y = .5, label = "Germany", color = "darkgrey") +
  xlim(0,50) +
  labs(x = "Capacity factor (%)", y = "Cumulative probability", subtitle = "(b)") +
  mytheme()

write.png(cumprobp, "./figures/cumprob.png")

# global detail: ask for cn, us and eu actual
cumprobpdet<- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  stat_function(fun = ecdf(opt_list$cn$returns_sharpe*100), color = "#984EA3") +
  stat_function(fun = ecdf(opt_list$eu$returns_sharpe*100), color = "#F8766D") +
  stat_function(fun = ecdf(opt_list$us$returns_sharpe*100), color = "#377EB8") +
  stat_function(fun = ecdf(opt_list$all$returns_sharpe*100), color = "#4DAF4A") +
  #stat_function(fun = ecdf(opt_list$china$returns*100), color = "#4DAF4A") +
  #stat_function(fun = ecdf(opt_list$usa$returns*100), color = "#4DAF4A") +
  #stat_function(fun = ecdf(opt_list$all$returns*100), color = "#4DAF4A") +
  stat_function(fun = ecdf(europe$DE*100), color = "grey") +
  stat_function(fun = ecdf(europe$SI*100), color = "grey") +
  stat_function(fun = ecdf(china$NMW*100), color = "grey") +
  stat_function(fun = ecdf(usa$TX*100), color = "grey") +
  xlim(0,20) + ylim(0,0.5) +
  mytheme() +
  theme(axis.title = element_blank())

write.png(cumprobpdet, "./figures/cumprobdet.png")

# cumprob by country
# cn
cncumprobp<- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  stat_function(fun = ecdf(opt_list$cn$returns_sharpe*100),
                color = cncolors[2], size = 1) +
  annotate("text", x = 25, y = .12, label = "China", color = cncolors[2]) +
  stat_function(fun = ecdf(opt_list$northwest$returns_sharpe*100),
                color = cncolors[5], size = 1) +
  annotate("text", x = 55, y = .8, label = "Northwest", color = cncolors[5]) +
  stat_function(fun = ecdf(opt_list$northeast$returns_sharpe*100),
                color = cncolors[4], size = 1) +
  annotate("text", x = 55, y = .85, label = "Northeast", color = cncolors[4]) +
  geom_segment(x = 35, y = .825, xend = 48, yend = .825, color = "darkgrey") +
  stat_function(fun = ecdf(opt_list$south$returns_sharpe*100),
                color = cncolors[6], size = 1) +
  annotate("text", x = 20, y = .62, label = "South", color = cncolors[6]) +
  stat_function(fun = ecdf(opt_list$east$returns_sharpe*100),
                color = cncolors[3], size = 1) +
  annotate("text", x = 35, y = .62, label = "East", color = cncolors[3]) +
  stat_function(fun = ecdf(opt_list$central$returns_sharpe*100),
                color = cncolors[1], size = 1) +
  annotate("text", x = 5, y = .2, label = "Central", color = cncolors[1]) +
  xlim(0,70) +
  labs(x = "Capacity factor (%)", y = "Cumulative probability", subtitle = "(a)") +
  mytheme()

write.png(cncumprobp, "./figures/cncumprob.png")

# eu
eucumprobp<- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  stat_function(fun = ecdf(opt_list$mibel$returns_sharpe*100), 
                color = eucolors[3], size = 1) +
  annotate("text", x = 8, y = .25, label = "Mibel", color = eucolors[3]) +
  stat_function(fun = ecdf(opt_list$eu$returns_sharpe*100),
                color = eucolors[2], size = 1) +
  annotate("text", x = 25, y = .125, label = "Europe", color = eucolors[2]) +
  stat_function(fun = ecdf(opt_list$epex$returns_sharpe*100),
                color = eucolors[1], size = 1) +
  annotate("text", x = 10, y = .875, label = "Epex Spot", color = eucolors[1]) +
  geom_segment(x = 20, y = .875, xend = 40, yend = .875, color = "darkgrey") +
  stat_function(fun = ecdf(opt_list$nord$returns_sharpe*100), 
                color = eucolors[4], size = 1) +
  annotate("text", x = 40, y = .625, label = "Nord Pool", color = eucolors[4]) +
  xlim(0,80) +
  labs(x = "Capacity factor (%)", y = "Cumulative probability", subtitle = "(b)") +
  mytheme()

write.png(eucumprobp, "./figures/eucumprob.png")

# us
uscumprobp<- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  stat_function(fun = ecdf(opt_list$us$returns*100), 
                color = uscolors[5], size = 1) +
  annotate("text", x = 35, y = .6, label = "USA", color = uscolors[5]) +
  stat_function(fun = ecdf(opt_list$wecc$returns*100),
                color = uscolors[6], size = 1) +
  annotate("text", x = 18, y = .75, label = "WECC", color = uscolors[6]) +
  geom_segment(x = 24, y = .75, xend = 31, yend = .75, color = "darkgrey") +
  stat_function(fun = ecdf(opt_list$mro$returns*100),
                color = uscolors[1], size = 1) +
  annotate("text", x = 40, y = .4, label = "MRO", color = uscolors[1]) +
  stat_function(fun = ecdf(opt_list$serc$returns*100),
                color = uscolors[4], size = 1) +
  annotate("text", x = 30, y = .875, label = "SERC", color = uscolors[4]) +
  stat_function(fun = ecdf(opt_list$rf$returns*100),
                color = uscolors[3], size = 1) +
  annotate("text", x = 4, y = .13, label = "RF", color = uscolors[3]) +
  stat_function(fun = ecdf(opt_list$npcc$returns*100),
                color = uscolors[2], size = 1) +
  annotate("text", x = 55, y = .875, label = "NPCC", color = uscolors[2]) +
  xlim(0,80) +
  labs(x = "Capacity factor (%)", y = "Cumulative probability", subtitle = "(c)") +
  mytheme()

write.png(uscumprobp, "./figures/uscumprob.png")



# 3.3. Weights ####

# example
w<- opt_list$nord$weights_sharpe
w1<- w[w >0.01]*100
w2<- data.frame(weight = w1, country = names(w1))

wplot<- ggplot(w2, aes(y = reorder(country, weight), x = weight)) +
  geom_bar(stat = "identity") +
  labs(x = "Share of installed capacity (%)", subtitle = "Green") +
  mytheme() +
  theme(axis.title.y = element_blank())
# end example    

# weight plots  
for (region in regionsnames){
  for(portfolio in c("weights_minvar", "weights_maxmean", "weights_sharpe")){
    w<- opt_list[[region]][[portfolio]]
    w1<- w[w >0.01]*100
    w2<- data.frame(weight = w1, country = names(w1), region = region) %>% print
    
    wplot<- ggplot(w2, aes(y = reorder(country, weight), x = weight)) +
      geom_bar(stat = "identity") +
      labs(x = "Share of installed capacity (%)", 
           subtitle = paste(region, portfolio, sep = "")) +
      mytheme() +
      theme(axis.title.y = element_blank())
    write.png(wplot, paste("./figures/weights/", region, portfolio, ".png", sep = ""))
  }
}

# export weights data in csv
for (region in regionsnames){
  for(portfolio in c("weights_minvar", "weights_maxmean", "weights_sharpe")){
    w<- opt_list[[region]][[portfolio]]
    w1<- w[w >0.01]*100
    w2<- data.frame(weight = w1, country = names(w1), region = region) %>% print
    
    write.csv(w2, paste("./data/results/weights/", region, portfolio,
                           ".csv", sep = ""))
  }
}

# check
# checklist<- list()
#  for (region in regionsnames){
#   for(portfolio in c("weights_minvar", "weights_maxmean", "weights_sharpe")){
#     w<- opt_list[[region]][[portfolio]]
#     w1<- w[w >0.01]*100
#     w2<- data.frame(weight = w1, country = names(w1), region = region)
#     
#     checklist[region, portfolio]<- w2
#     }
# }

# X3.4. Sharpe ####

# autarky sharpe ratios
eudf<- readRDS("./data/clean/europe_xts.rds") %>% fortify() %>%
  select(-"Index") %>% mutate(region = "Europe") %>% gather(country, CF, -region)
usdf<- readRDS("./data/clean/usa_xts.rds") %>% fortify() %>%
  select(-"Index") %>% mutate(region = "USA") %>% gather(country, CF, -region)
cndf<- readRDS("./data/clean/china_xts.rds") %>% fortify() %>%
  select(-"Index") %>% mutate(region = "China") %>% gather(country, CF, -region)

alldf<- bind_rows(cndf, eudf, usdf) %>% group_by(region, country) %>%
  summarize(sd = sd(CF), mean = mean(CF)) %>%
  mutate(sharpe = mean/sd)

optsharpe<- data.frame(sharpe = 
                         c(max(opt_list$cn$sharpe), 
                           max(opt_list$eu$sharpe), 
                           max(opt_list$us$sharpe)),
                       region = c("China", "Europe", "USA"))

allsharpe<- data.frame(sharpe = rep(max(opt_list$all$sharpe),3),
                       region = c("China", "Europe", "USA"))

# viz sharpe
sharpep<- ggplot(alldf, aes(x = sharpe, color = region)) +
  geom_density() +
  geom_segment(data = optsharpe, 
               aes(x = sharpe, xend = sharpe, y = 0, yend = 2, color = region)) + 
  geom_segment(aes(x = max(opt_list$all$sharpe),
                   xend = max(opt_list$all$sharpe), y = 0, yend = 2,
             color = "black")) + 
  scale_x_continuous(breaks = seq(1,6,1)) +
  scale_color_manual(values = allcolors) +
  labs(x = expression(paste(CV^-1, "(CF/SD)"))) +
  geom_segment(aes(x = 2.2, y = .1, xend = 3, yend = .1),
               arrow = arrow(length = unit(0.5, "cm")), color = allcolors[3]) +
  annotate("text", x = 2.6, y = .25, color = allcolors[3], label = "Europe") +
  geom_segment(aes(x = 2.2, y = 1.1, xend = 3.6, yend = 1.1),
               arrow = arrow(length = unit(0.5, "cm")), color = allcolors[2]) +
  annotate("text", x = 2.6, y = 1.25, color = allcolors[2], label = "China") +
  geom_segment(aes(x = 2.2, y = 0.6, xend = 3.4, yend = 0.6),
               arrow = arrow(length = unit(0.5, "cm")), color = allcolors[4]) +
  annotate("text", x = 2.6, y = 0.75, color = allcolors[4], label = "USA") +
  geom_segment(aes(x = 2.2, y = 1.6, xend = 5.7, yend = 1.6),
               arrow = arrow(length = unit(0.5, "cm")), color = "black") +
  annotate("text", x = 5, y = 1.75, color = "black", label = "Intercontinental") +
  geom_bracket(xmin = 0, xmax = 2.2, y.position = 2.1,
    label = "Distribution of the inverse CV \n in autarky by continent", 
    color = "darkgrey") +
  geom_segment(aes(x = 2.4, y = 2.1, xend = 5.8, yend = 2.1),
               arrow = arrow(length = unit(0.5, "cm")), color = "darkgrey") +
  annotate("text", x = 4, y = 2.25, color = "darkgrey",
           label = "Gains from spatial integration \n and deployment coordination") +
  coord_cartesian(ylim = c(0, 2.3)) +
  theme(legend.position = "none", axis.title.y = element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

write.png(sharpep, "./figures/sharpep.png")

# calculate equal shares sharpe ratios for the three regions #  chao
equalwport<- function(i){
  a<- i %>% fortify %>% select(-"Index") %>% 
    mutate(equal = rowSums(.)/ncol(i)) %>% 
    gather(country, value) %>%  
    summarize(sd = sd(value), mean = mean(value)) %>%
    mutate(sharpe = mean/sd)
}
cnequal<- equalwport(cn) %>% mutate(region = "China")
euequal<- equalwport(eu) %>% mutate(region = "Europe")
usequal<- equalwport(us) %>% mutate(region = "USA")

equalshares<- bind_rows(cnequal, euequal, usequal)
mibel %>% fortify %>% mutate(equal = ES*.5+PT*.5) %>% select(-"Index") %>%
  gather(country, value) %>% group_by(country) %>% 
  summarize(sd(value), mean(value))

all_same<- all %>% fortify %>% select(-"Index") %>% 
  mutate(equal = rowSums(.)/111) %>% 
  gather(country, value) %>% group_by(country) %>% 
  summarize(sd = sd(value), mean = mean(value))

all_same %>% filter(country == "equal")

ggplot(all_same) + geom_density(aes(x = sd)) + 
  geom_vline(xintercept = 0.0621)

ggplot(all_same) + geom_density(aes(x = mean)) + 
  geom_vline(xintercept = 0.249)

opt_list$all$sds

# X3.6. Density ####
# select returns
ret<- data.frame(Optimal = opt_list$all$returns_sharpe,
                 MinVar = opt_list$all$returns_minvar,
                 MaxCF = opt_list$all$returns_maxmean) %>%
  gather(key, value)

# Intercontinental Optimal portfolio analysis
median(opt_list$all$returns_sharpe)
summary(opt_list$all$returns_sharpe)
mean(opt_list$all$returns_sharpe)
quantile(opt_list$all$returns_sharpe, c(.1, .9))
quantile(opt_list$all$returns_sharpe, c(.05, .95))
quantile(opt_list$all$returns_sharpe, c(.01, .99))
minpoint<- data.frame(x = min(opt_list$all$returns_sharpe)*100, y = 0)

all_cf<- opt_list$all$returns_sharpe

100* (sum(all_cf < mean(all_cf)-0.5*mean(all_cf)) + sum(all_cf > mean(all_cf)+0.5*mean(all_cf))) / length(all_cf)
100* (sum(all_cf < mean(all_cf)-0.25*mean(all_cf)) + sum(all_cf > mean(all_cf)+0.25*mean(all_cf))) / length(all_cf)
100* (sum(all_cf < mean(all_cf)-0.3*mean(all_cf)) + sum(all_cf > mean(all_cf)+0.3*mean(all_cf))) / length(all_cf)

#show_col(viridis_pal(option = "turbo")(3))
# order      maxCF     minVar     opt
#greycol<- c("#30123BFF", "#A2FC3CFF", "#7A0403FF")

greycol<- c("darkgrey", "darkgrey", "black")


basicdensp<- ggplot(ret) +
  geom_density(aes(x = value*100, y = ..count../10, color = key), size = 1) + # /years
  # geom_vline(xintercept = quantile(opt_list$all$returns_sharpe, 0.05)*100) +
  geom_point(data = minpoint, aes(x=x,y=y), color = greycol[3], size = 2) +
  labs(x = "Capacity Factor", y = "Hours/year") +
  scale_color_manual(values = greycol) +
  mytheme() +
  theme(legend.position = "none") +
  annotate("text", x = 6, y = 950, label = "Min. \n Volatility", color = greycol[2]) +
  annotate("text", x = 40, y = 600, label = "Optimal",
           color = greycol[3]) +
  annotate("text", x = 60, y = 150, label = "Max. CF", color = greycol[1])

write.png(basicdensp, "./figures/basicdensp.png")

# add annotations only for the last version computationally heavy and manual labels
densityp<- basicdensp +
  geom_curve(aes(x = 50, y = 300, xend = minpoint$x, yend = minpoint$y),
             arrow = arrow(length = unit(0.03, "npc")), curvature = 0.3, angle = 90)+
   annotate("text", x = 65, y = 300, label = "Min. CF 13%") +
   geom_segment(aes(x = quantile(opt_list$all$returns_sharpe, 0.05)*100, 
                   y = 900, xend = 95, yend = 900),
               arrow = arrow(length = unit(0.5, "cm"))) +
   annotate("text", x = 50, y = 950, label = "95% of hours CF >19.9%") +


  
  write.png(densityp, "./figures/densityp.png")
  
# full
densityp<- ggplot(ret) +
  geom_density(aes(x = value*100, y = ..count../10, color = key)) + # /years
  geom_vline(xintercept = quantile(opt_list$all$returns_sharpe, 0.05)*100,
             color = "darkgrey") +
  geom_point(data = minpoint, aes(x=x,y=y), color = "darkgrey") +
  geom_curve(aes(x = 50, y = 300, xend = minpoint$x, yend = minpoint$y),
             arrow = arrow(length = unit(0.03, "npc"), type="closed"), 
             colour = "darkgrey", curvature = 0.3, angle = 90) +
  annotate("text", x = 70, y = 300, color = "darkgrey", 
           label = "Min. CF 13%") +
  labs(x = "Capacity Factor", y = "Hours/year") +
  geom_segment(aes(x = minpoint$x, y = 800, xend = 90, yend = 800),
               arrow = arrow(length = unit(0.5, "cm")), color = "darkgrey") +
  annotate("text", x = 5, y = 1.75, color = "darkgrey", 
           label = "95% of hours CF >19.9%") +
  scale_color_viridis_d(option = "turbo") +
  mytheme() +
  theme(legend.position = "none")

write.png(densityp, "./figures/densityp.png")


# 3.7. Firm ####
# scatterplot min-range
# regions
# probability of being beyond +- prob mean
opt_ret<- data.frame()
for (i in c(regionsnames)){
  a<- opt_list[[i]][["returns_sharpe"]]
  b<- data.frame(cf =  a,
                 region = i)
  opt_ret<- bind_rows(opt_ret, b) %>% mutate(region = as.factor(region))
}


minrange<- opt_ret %>% group_by(region) %>% # range: prob CF beyond mean+-50%
  summarize(range5 =  (sum(cf < mean(cf)-.5*mean(cf)) +
                        sum(cf > mean(cf)+.5*mean(cf))) /n() *100,
            range25 =  (sum(cf < mean(cf)-.25*mean(cf)) +
                         sum(cf > mean(cf)+.25*mean(cf))) /n() *100,
            range30 =  (sum(cf < mean(cf)-.3*mean(cf)) +
                          sum(cf > mean(cf)+.3*mean(cf))) /n() *100,
            min = min(cf),
            one =  quantile(cf, .01),
            five =  quantile(cf, .05),
            ten =  quantile(cf, .1))

# recode region names
minrange$region<- recode_factor(minrange$region, 
                                all = "Intercontinental",
                                cn = "China",
                                eu = "Europe",
                                us = "USA",
                                central = "Central",
                                east = "East",
                                northeast = "Northeast",
                                northwest= "Northwest",
                                south = "South",
                                epex = "Epex Spot",
                                mibel = "Mibel",
                                nord = "Nord Pool",
                                mro = "MRO",
                                npcc = "NPCC",
                                rf = "RF",
                                serc = "SERC",
                                wecc = "WECC") 

levels(minrange$region)

mycolors<- c("black", "#E13342FF", "#37659EFF", "#B93289FF", "#701F57FF", "#36193EFF",
             "#AE1759FF", "#F37651FF", "#F6B48EFF", "#348FA7FF", "#40B7ADFF",
             "#8AD9B1FF", "#5402A3FF", "#8B0AA5FF", "#DB5C68FF", "#B93289FF", "#FEBC2AFF")



# autarky
all_df<- read.csv("./data/clean/all_df.xts")
allminrange<- all_df[,-c(1,2)] %>% gather(country, cf) %>% group_by(country) %>% 
  summarize(range5 =  (sum(cf < mean(cf)-.5*mean(cf)) +
                         sum(cf > mean(cf)+.5*mean(cf))) /n() *100,
            range25 =  (sum(cf < mean(cf)-.25*mean(cf)) +
                          sum(cf > mean(cf)+.25*mean(cf))) /n() *100,
            range30 =  (sum(cf < mean(cf)-.3*mean(cf)) +
                          sum(cf > mean(cf)+.3*mean(cf))) /n() *100,
            min = min(cf),
            one =  quantile(cf, .01),
            five =  quantile(cf, .05),
            ten =  quantile(cf, .1))

levels(minrange$region)

firmplot<- ggplot(minrange, aes(x = one*100, y = 100-range5, color = region)) +
  geom_point() +
  geom_point(data = allminrange, aes(x = one*100, y = 100-range5),
             color = "darkgrey", alpha = .2) +
  geom_text_repel(data = minrange, aes(x = one*100, y = 100-range5, label = region)) +
  annotate("text", x = 2.5, y = 40, label = "Autarky", color = "darkgrey") +
  labs(x = "Reliability \n (1% lowest capacity factor)",
       y = "Certainty \n (Probability of CF within mean ±50%)") +
  scale_color_manual(values = mycolors) +
  mytheme() +
  theme(legend.position = "none")

write.png(firmplot, "./figures/firmplot.png")

firmplot<- ggplot(minrange, aes(x = ten*100, y = 100-range30, color = region)) +
  geom_point() +
  geom_point(data = allminrange, aes(x = ten*100, y = 100-range30),
             color = "darkgrey", alpha = .2) +
  geom_text_repel(data = minrange, aes(x = ten*100, y = 100-range30, label = region)) +
  annotate("text", x = 5, y = 15, label = "Autarky", color = "darkgrey") +
  labs(x = "Reliability \n (10% lowest capacity factor)",
       y = "Certainty \n (Probability of CF within mean ±30%)") +
  scale_color_manual(values = mycolors) +
  mytheme() +
  theme(legend.position = "none")

write.png(firmplot, "./figures/firmplot10-30.png")


# violin

minpoints<- opt_ret %>% group_by(region) %>% summarise(cf = min(cf))

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m - .5*m
  ymax <- m + .5*m
  return(c(y=m,ymin=ymin,ymax=ymax))
}

# violin plot
violinp<- ggplot(opt_ret, aes(x = cf*100, y = fct_reorder(region, cf, .fun = "min"))) +
  geom_violin() +
  stat_summary(fun.data=data_summary, color = "darkgrey") +
  geom_point(data = minpoints, aes(x = cf*100, y  = fct_reorder(region, cf))) +
  labs(x = "Capacity factor (%)") +
  scale_x_log10() +
  mytheme() +
  theme(axis.title.y = element_blank())

write.png(violinp, "./figures/violinp.png", height = 6)

# quantiles
quantpoints<- opt_ret %>% group_by(region) %>% 
  summarise(one = quantile(cf, .01), five= quantile(cf, .05), ten = quantile(cf, .1))

ggplot(quantpoints, aes(x = five*100, y = fct_reorder(region, five))) +
  geom_point()

opt_ret %>% group_by(region) %>%  summarise(mean(cf))


# 3.8. Prob. ####
# 3.8.1. P<x ####
# calculate probability of "low wind" events in autarky
# how do we define "low wind"? 10-20%CF? or 

eudf<- readRDS("./data/clean/europe_xts.rds") %>% fortify() %>%
  select(-"Index") %>% mutate(region = "Europe") %>% gather(country, CF, -region)
usdf<- readRDS("./data/clean/usa_xts.rds") %>% fortify() %>%
  select(-"Index") %>% mutate(region = "USA") %>% gather(country, CF, -region)
cndf<- readRDS("./data/clean/china_xts.rds") %>% fortify() %>%
  select(-"Index") %>% mutate(region = "China") %>% gather(country, CF, -region)

probplot<- function(prob = .1){
  # calculate prob <10%CF for each configuration
  result<- bind_rows(cndf, eudf, usdf) %>% group_by(region, country) %>%
    summarize(autarky = sum(CF < prob)/n()*100)
}


alldfp10<- probplot()  
  
  p10list<- list()
  for (i in c(regionsnames)){
    a<- opt_list[[i]][["returns_sharpe"]]
    b<- data.frame(country = names(get(i)),
                   name = (sum(a<prob)/length(a))*100)
    names(b)<- c("country", i)
    p10list[[i]]<- b
  }
  
  
  p10df<- p10list %>% reduce(full_join, by = "country")
  
  
  p10<- full_join(alldfp10, p10df) %>% gather(conf, prob, -region, -country) %>%
    ungroup() %>% 
    mutate(region = as.factor(region), country = as.factor(country),
           conf = as.factor(conf))
  
  # order levels for each region
  cnorder<- p10 %>% filter(region == "China" & conf == "autarky") %>% 
    arrange(prob) %>%  select(country)
  euorder<- p10 %>% filter(region == "Europe" & conf == "autarky") %>% 
    arrange(prob) %>%  select(country)
  usorder<- p10 %>% filter(region == "USA" & conf == "autarky") %>% 
    arrange(prob) %>%  select(country)
  
  opt_ret<-  opt_list[["all"]][["returns_sharpe"]]
  intercontp10<- sum(opt_ret < prob)/length(opt_ret)*100
  
  
  # china
  p10cn<- na.omit(p10) %>% filter(region == "China", conf != "all") %>% 
    mutate(conf = droplevels(conf))
  p10cn$country<- ordered(p10cn$country, levels = cnorder$country)
  levels(p10cn$country)
  levels(p10cn$conf)<- c("Autarky", "Central", "China", "East",
                         "Northeast", "Northwest", "South")
  
  shapes<- c(16,17,18,15,8,3,4)
  
  p10cnp<- ggplot(p10cn, aes(x = prob, y = country, color = conf, shape = conf)) +
    geom_point() +
    labs(x = "Probability of low-wind events (%)") +
    scale_color_manual(values = c("darkgrey", cncolors)) +
    scale_shape_manual(values = shapes) +
    labs(subtitle = "(a)") +
    mytheme() +
    #  annotate("text", x = 15, y = "BJ", label = "Northeast", color = cncolors[4]) +
    #  annotate("text", x = 5, y = "XZ", label = "China", color = cncolors[2]) +
    # annotate("text", x = 20, y = "JX", label = "Central", color = cncolors[1]) +
    # annotate("text", x = 20, y = "GZ", label = "South", color = cncolors[6]) +
    # annotate("text", x = 15, y = "GS", label = "Northwest", color = cncolors[5]) +
    # annotate("text", x = 15, y = "AH", label = "East", color = cncolors[3]) +
    theme(legend.position = c(.8,.2), legend.title = element_blank(), 
          axis.title.x = element_blank(), axis.title.y = element_blank())
  
  write.png(p10cnp, paste("./figures/prob", prob, "cnp.png", sep =""), height = 6)
  
  # europe
  p10eu<- na.omit(p10)  %>% filter(region == "Europe", conf != "all") %>% 
    mutate(conf = droplevels(conf))
  p10eu$country<- ordered(p10eu$country, levels = euorder$country)
  levels(p10eu$country)
  levels(p10eu$conf)<- c("Autarky", "Epex Spot", "Europe", "Mibel", "Nord Pool")
  
  p10eup<- ggplot(p10eu, aes(x = prob, y = country, color = conf, shape = conf)) +
    geom_point() +
    labs(x = "Probability of low-wind events (%)") +
    scale_color_manual(values = c("darkgrey", eucolors)) +
    labs(subtitle = "(b)") +
    mytheme() +
    theme(legend.position = c(.8,.2), legend.title = element_blank(), 
          axis.title.x = element_blank(), axis.title.y = element_blank())
  
  write.png(p10eup, paste("./figures/prob", prob, "eup.png", sep =""), height = 6)
  
  # usa
  p10us<- na.omit(p10) %>% filter(region == "USA", conf != "all") %>% 
    mutate(conf = droplevels(conf))
  p10us$country<- ordered(p10us$country, levels = usorder$country)
  levels(p10us$country)
  levels(p10us$conf)<- c("Autarky", "MRO", "NPCC", "RF",
                         "SERC", "USA", "WECC")
  
  shapes<- c(16,17,18,15,8,3,4)
  
  p10usp<- ggplot(p10us, aes(x = prob, y = country, color = conf, shape = conf)) +
    geom_point() +
    labs(x = "Probability of low-wind events (%)") +
    scale_color_manual(values = c("darkgrey", uscolors)) +
    scale_shape_manual(values = shapes) +
    labs(subtitle = "(c)") +
    mytheme() +
    theme(legend.position = c(.8,.2), legend.title = element_blank(), 
          axis.title.x = element_blank(), axis.title.y = element_blank())
  
  write.png(p10usp, paste("./figures/prob", prob, "usp.png", sep =""), height = 6)
  
  intercontp10
}

probplot(prob = .3)




# 3.8.2. Range ####
# probability of CF <> x% of the mean


rangeprobplot<- function(prob = .5){
  alldfp10<- bind_rows(cndf, eudf, usdf) %>% group_by(region, country) %>%
    summarize(autarky = (sum(CF < mean(CF)-prob*mean(CF)) +
                           sum(CF > mean(CF)+prob*mean(CF))) /n() *100)
  
  # calculate prob <10%CF for each configuration
  p10list<- list()
  for (i in c(regionsnames)){
    a<- opt_list[[i]][["returns_sharpe"]]
    b<- data.frame(country = names(get(i)),
                   name = (sum(a < mean(a)-prob*mean(a)) +
                             sum(a > mean(a)+prob*mean(a))) /length(a) *100)
    names(b)<- c("country", i)
    p10list[[i]]<- b
  }
  
  p10df<- p10list %>% reduce(full_join, by = "country")
  
  
  p10<- full_join(alldfp10, p10df) %>% gather(conf, prob, -region, -country) %>%
    ungroup() %>% 
    mutate(region = as.factor(region), country = as.factor(country),
           conf = as.factor(conf))
  
  # order levels for each region
  cnorder<- p10 %>% filter(region == "China" & conf == "autarky") %>% 
    arrange(prob) %>%  select(country)
  euorder<- p10 %>% filter(region == "Europe" & conf == "autarky") %>% 
    arrange(prob) %>%  select(country)
  usorder<- p10 %>% filter(region == "USA" & conf == "autarky") %>% 
    arrange(prob) %>%  select(country)
  
  opt_ret<-  opt_list[["all"]][["returns_sharpe"]]
  intercontp10<- (sum(opt_ret < mean(opt_ret)-prob*mean(opt_ret)) +
    sum(opt_ret > mean(opt_ret)+prob*mean(opt_ret))) /length(opt_ret) *100 
  
  
  # china
  p10cn<- na.omit(p10) %>% filter(region == "China", conf != "all") %>% 
    mutate(conf = droplevels(conf))
  p10cn$country<- ordered(p10cn$country, levels = cnorder$country)
  levels(p10cn$country)
  levels(p10cn$conf)<- c("Autarky", "Central", "China", "East",
                         "Northeast", "Northwest", "South")
  
  shapes<- c(16,17,18,15,8,3,4)
  
  p10cnp<- ggplot(p10cn, aes(x = prob, y = country, color = conf, shape = conf)) +
    geom_point() +
    labs(x = "Probability (%)") +
    scale_color_manual(values = c("darkgrey", cncolors)) +
    scale_shape_manual(values = shapes) +
    labs(subtitle = "(a)") +
    mytheme() +
    theme(legend.position = c(.85,.2), legend.title = element_blank(), 
          axis.title.y = element_blank())
  
  write.png(p10cnp, paste("./figures/rangeprob", prob, "cnp.png"), height = 6)
  
  # europe
  p10eu<- na.omit(p10)  %>% filter(region == "Europe", conf != "all") %>% 
    mutate(conf = droplevels(conf))
  p10eu$country<- ordered(p10eu$country, levels = euorder$country)
  levels(p10eu$country)
  levels(p10eu$conf)<- c("Autarky", "Epex Spot", "Europe", "Mibel", "Nord Pool")
  
  p10eup<- ggplot(p10eu, aes(x = prob, y = country, color = conf, shape = conf)) +
    geom_point() +
    labs(x = "Probability (%)") +
    scale_color_manual(values = c("darkgrey", eucolors)) +
    labs(subtitle = "(b)") +
    mytheme() +
    theme(legend.position = c(.85,.2), legend.title = element_blank(), 
          axis.title.y = element_blank())
  
  write.png(p10eup, paste("./figures/rangeprob", prob, "eup.png"), height = 6)
  
  # usa
  p10us<- na.omit(p10) %>% filter(region == "USA", conf != "all") %>% 
    mutate(conf = droplevels(conf))
  p10us$country<- ordered(p10us$country, levels = usorder$country)
  levels(p10us$country)
  levels(p10us$conf)<- c("Autarky", "MRO", "NPCC", "RF",
                         "SERC", "USA", "WECC")
  
  shapes<- c(16,17,18,15,8,3,4)
  
  p10usp<- ggplot(p10us, aes(x = prob, y = country, color = conf, shape = conf)) +
    geom_point() +
    labs(x = "Probability (%)") +
    scale_color_manual(values = c("darkgrey", uscolors)) +
    scale_shape_manual(values = shapes) +
    labs(subtitle = "(c)") +
    mytheme() +
    theme(legend.position = c(.85,.2), legend.title = element_blank(), 
          axis.title.y = element_blank())
  
  write.png(p10usp, paste("./figures/rangeprob", prob, "usp.png"), height = 6)
  
  intercontp10
}

rangeprobplot(prob = .5)




# X3.7. Constrained ####
optimize_constrained<- function(data, low, high){
  # find min. value for which the optim. is defined
  for (i in seq(.01, 1, .01)) {
    if (class(tryCatch(portfolio.optim(x = data, pm = i,
                                       reslow = rep(low, ncol(data)),
                                       reshigh = rep(high, ncol(data))), 
                       error = function(e) "chao")) == "list")
    {min<- i 
    break}
  }
  
  # find max. value for which the optim. is defined
  for (i in seq(.01, 1, .01)) {
    if (class(tryCatch(portfolio.optim(x = data, pm = i,
                                       reslow = rep(low, ncol(data)),
                                       reshigh = rep(high, ncol(data))), 
                       error=function(e) "chao")) == "list")
    {max<- i}
  }
  
  # create vectors/matrices to fill with results
  # create grid of returns for the frontier
  grid <- seq(from = min, to = max, length.out = 50)
  # Create an empty matrix to store portfolio "returns" (CF)
  returns <- matrix(NA, 50, length(data)/ncol(data))
  # Create empty vectors to store means and std. deviations
  means <- sds <- rep(NA, length(grid)) 
  # Create an empty matrix to store weights
  weights <- matrix(NA, 50, ncol(data)) 
  names(weights)<- colnames(data)
  
  # run optimization model
  for(i in 1:length(grid)) {
    opt <- portfolio.optim(x = data, pm = grid[i],
                           reslow = rep(low, ncol(data)),
                           reshigh = rep(high, ncol(data)))
    returns[i, ]<- opt$px # matrix of portfolio "returns" (CF)
    means[i] <- opt$pm # vector of mean portfolio capacity factor
    sds[i] <- opt$ps # vector of portfolio sd
    weights[i, ] <- opt$pw # matrix of weights
  }
  
  # calculate sharpe ratio and weights of extreme portfolios
  sharp <- means/sds # Sharpe ratio 
}

allsharp<- optimize_constrained(data = all, low = 0, high = 1)

sensitivitylow<- data.frame()
for (i in seq(0,1,.5)){
    a<- optimize_constrained(data = all, low = i, high = NULL)
    b<- data.frame(sharpe = max(a), low = i, high = j)
    sensitivitylow<- bind_rows(b)
}

sensitivityhigh<- data.frame()
for (i in seq(0,1,.5)){
  a<- optimize_constrained(data = all, low = NULL, high = i)
  b<- data.frame(sharpe = max(a), low = i, high = j)
  sensitivitylow<- bind_rows(b)
}



# example constrained portfolio

portfolio.optim(x = all, pm = 0.1,
                reslow = rep(0, ncol(all)),
                reshigh = rep(.5, ncol(all)))








