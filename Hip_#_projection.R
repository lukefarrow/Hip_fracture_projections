library(tidyverse)
library(ggplot2)
library(readxl)
library(forecast)
library(lubridate)
library(grid)

###time series###

##hip # all ages raw##
Hip_projections_all <- Hip_projections_all <- read_excel("~/Current projects/Hip # projections/Hip # projections all.xlsx", 
                                                         range = "C1:C61")
View(Hip_projections_all)
hip_allage_allsex <- ts(Hip_projections_all2, frequency=12, start=c(2017,1))
view(hip_allage_allsex)
plot.ts(hip_allage_allsex)

#ES model#

hip_allage_allsex_forecast1 <- HoltWinters(hip_allage_allsex, gamma = TRUE)
hip_allage_allsex_forecast1
hip_allage_allsex_forecast1$SSE
plot(hip_allage_allsex_forecast1)
hip_allage_allsex_forecast2 <- forecast:::forecast.HoltWinters(hip_allage_allsex_forecast1, 96)
forecast:::plot.forecast(hip_allage_allsex_forecast2)
hip_allage_allsex_forecast2

#ARIMA model#
auto.arima(hip_allage_allsex)
hip_allage_allsex_arima <- arima(x = hip_allage_allsex, order = c(0, 1, 1), seasonal = list(order = c(1, 0, 0), period = 12), 
                                 method = "ML")
hip_allage_allsex_arima
hip_allage_allsex_arima_forecast <- forecast(hip_allage_allsex_arima, h=96)
forecast:::plot.forecast(hip_allage_allsex_arima_forecast)
hip_allage_allsex_arima_forecast

##hip # male all ages ##
Hip_projections_all_male <- read_excel("~/Current projects/Hip # projections/Hip # projections all_male.xlsx", 
                                         +     range = "C1:C61")
hip_allage_male <- ts(Hip_projections_all_male, frequency=12, start=c(2017,1))
view(hip_allage_male)
plot.ts(hip_allage_male)

#ES model#

hip_allage_male_forecast1 <- HoltWinters(hip_allage_male, gamma = TRUE)
hip_allage_male_forecast1
hip_allage_male_forecast1$SSE
plot(hip_allage_male_forecast1)
hip_allage_male_forecast2 <- forecast:::forecast.HoltWinters(hip_allage_male_forecast1, 96)
forecast:::plot.forecast(hip_allage_male_forecast2)
hip_allage_male_forecast2

#ARIMA model#
auto.arima(hip_allage_male)
hip_allage_male_arima <- arima(x = hip_allage_male, order = c(0, 1, 1), seasonal = list(order = c(1, 0, 0), period = 12), 
                                 method = "ML")
hip_allage_male_arima
hip_allage_male_arima_forecast <- forecast(hip_allage_male_arima, h=96)
forecast:::plot.forecast(hip_allage_male_arima_forecast)
hip_allage_male_arima_forecast

##hip # female all ages ##
Hip_projections_all_female <- read_excel("~/Current projects/Hip # projections/Hip # projections all_female.xlsx", 
                                         +     range = "C1:C61")
hip_allage_female <- ts(Hip_projections_all_female, frequency=12, start=c(2017,1))
view(hip_allage_female)
plot.ts(hip_allage_female)

#ES model#

hip_allage_female_forecast1 <- HoltWinters(hip_allage_female, gamma = TRUE)
hip_allage_female_forecast1
hip_allage_female_forecast1$SSE
plot(hip_allage_female_forecast1)
hip_allage_female_forecast2 <- forecast:::forecast.HoltWinters(hip_allage_female_forecast1, 96)
forecast:::plot.forecast(hip_allage_female_forecast2)
hip_allage_female_forecast2

#ARIMA model#
auto.arima(hip_allage_female)
hip_allage_female_arima <- arima(x = hip_allage_female, order = c(0, 1, 1), seasonal = list(order = c(1, 0, 0), period = 12), 
                               method = "ML")
hip_allage_female_arima
hip_allage_female_arima_forecast <- forecast(hip_allage_female_arima, h=96)
forecast:::plot.forecast(hip_allage_female_arima_forecast)
hip_allage_female_arima_forecast

##hip # 90+ # ##
Hip_projections_90 <- read_excel("~/Current projects/Hip # projections/Hip # projections 90+.xlsx", 
                                 range = "C1:C61")
View(Hip_projections_90)
hip_90 <- ts(Hip_projections_90, frequency=12, start=c(2017,1))
view(hip_90)
plot.ts(hip_90)

#ES model#

hip_90_forecast1 <- HoltWinters(hip_90, gamma = TRUE)
hip_90_forecast1
hip_90_forecast1$SSE
plot(hip_90_forecast1)
hip_90_forecast2 <- forecast:::forecast.HoltWinters(hip_90_forecast1, 96)
forecast:::plot.forecast(hip_90_forecast2)
hip_90_forecast2

#ARIMA model#
auto.arima(hip_90)
hip_90_arima <- arima(x = hip_90, order = c(0, 0, 0), seasonal = list(order = c(1, 0, 0), period = 12), 
                               method = "ML")
hip_90_arima
hip_90_arima_forecast <- forecast(hip_90_arima, h=96)
forecast:::plot.forecast(hip_90_arima_forecast)
hip_90_arima_forecast

##hip # 80-89 # ##
Hip_projections_80_89 <- read_excel("~/Current projects/Hip # projections/Hip # projections 80-89.xlsx", 
                                    +     range = "C1:C61")
View(Hip_projections_80_89)
hip_80_89 <- ts(Hip_projections_80_89, frequency=12, start=c(2017,1))
view(hip_80_89)
plot.ts(hip_80_89)

#ES model#

hip_80_89_forecast1 <- HoltWinters(hip_80_89, gamma = TRUE)
hip_80_89_forecast1
hip_80_89_forecast1$SSE
plot(hip_80_89_forecast1)
hip_80_89_forecast2 <- forecast:::forecast.HoltWinters(hip_80_89_forecast1, 96)
forecast:::plot.forecast(hip_80_89_forecast2)
hip_80_89_forecast2

#ARIMA model#
auto.arima(hip_80_89)
hip_80_89_arima <- arima(x = hip_80_89, order = c(0, 1, 1), seasonal = list(order = c(0, 0, 1), period = 12), 
                      method = "ML")
hip_80_89_arima
hip_80_89_arima_forecast <- forecast(hip_80_89_arima, h=96)
forecast:::plot.forecast(hip_80_89_arima_forecast)
hip_80_89_arima_forecast

##hip # 70-79 # ##
Hip_projections_70_79 <- read_excel("~/Current projects/Hip # projections/Hip # projections 70-79.xlsx", 
                                    range = "C1:C61")
View(Hip_projections_70_79)
hip_70_79 <- ts(Hip_projections_70_79, frequency=12, start=c(2017,1))
view(hip_70_79)
plot.ts(hip_70_79)

#ES model#

hip_70_79_forecast1 <- HoltWinters(hip_70_79, gamma = TRUE)
hip_70_79_forecast1
hip_70_79_forecast1$SSE
plot(hip_70_79_forecast1)
hip_70_79_forecast2 <- forecast:::forecast.HoltWinters(hip_70_79_forecast1, 96)
forecast:::plot.forecast(hip_70_79_forecast2)
hip_70_79_forecast2

#ARIMA model#
hip_70_79_arima <- auto.arima(hip_70_79)
hip_70_79_arima
hip_70_79_arima_forecast <- forecast(hip_70_79_arima, h=96)
forecast:::plot.forecast(hip_70_79_arima_forecast)
hip_70_79_arima_forecast

##hip # 60-69 # ##
Hip_projections_60_69 <- read_excel("~/Current projects/Hip # projections/Hip # projections 60-69.xlsx", 
                                    +     range = "C1:C61")
View(Hip_projections_60_69)
hip_60_69 <- ts(Hip_projections_60_69, frequency=12, start=c(2017,1))
view(hip_60_69)
plot.ts(hip_60_69)

#ES model#

hip_60_69_forecast1 <- HoltWinters(hip_60_69, alpha = 0.012, beta = 0.2, gamma = TRUE)
hip_60_69_forecast1
hip_60_69_forecast1$SSE
plot(hip_60_69_forecast1)
hip_60_69_forecast2 <- forecast:::forecast.HoltWinters(hip_60_69_forecast1, 96)
forecast:::plot.forecast(hip_60_69_forecast2)
hip_60_69_forecast2

#ARIMA model#
hip_60_69_arima <- auto.arima(hip_60_69)
hip_60_69_arima
hip_60_69_arima_forecast <- forecast(hip_60_69_arima, h=96)
forecast:::plot.forecast(hip_60_69_arima_forecast)
hip_60_69_arima_forecast

##hip # 50-59 # ##
Hip_projections_50_59 <- read_excel("~/Current projects/Hip # projections/Hip # projections 50-59.xlsx", 
                                    +     range = "C1:C61")
View(Hip_projections_50_59)
hip_50_59 <- ts(Hip_projections_50_59, frequency=12, start=c(2017,1))
view(hip_50_59)
plot.ts(hip_50_59)

#ES model#

hip_50_59_forecast1 <- HoltWinters(hip_50_59, alpha = 0.008, beta = 0.2, gamma = TRUE)
hip_50_59_forecast1
hip_50_59_forecast1$SSE
plot(hip_50_59_forecast1)
hip_50_59_forecast2 <- forecast:::forecast.HoltWinters(hip_50_59_forecast1, 96)
forecast:::plot.forecast(hip_50_59_forecast2)
hip_50_59_forecast2

#ARIMA model#
hip_50_59_arima <- auto.arima(hip_50_59)
hip_50_59_arima
hip_50_59_arima_forecast <- forecast(hip_50_59_arima, h=96)
forecast:::plot.forecast(hip_50_59_arima_forecast)
hip_50_59_arima_forecast

## graphs ##

# all #
library(readxl)

Hip_projections_all2 <- read_excel("~/Current projects/Hip # projections/Hip # projections all.xlsx", 
                                  sheet = "Combined")
View(Hip_projections_all2)

Hip_projections_all2 <- Hip_projections_all2 %>% mutate('date' = make_date(year = Year, month = Month, day = 1))
Hip_projections_all2$date <- lubridate::myd(paste(Hip_projections_all2$Month, Hip_projections_all2$Year, "1"))


View(Hip_projections_all2)

plot_projection_all <- ggplot(Hip_projections_all2,mapping = aes(x = date, y = Incidence)) +
  geom_point(mapping = aes(color = act_pred)) +
  geom_smooth(color="black",se = FALSE) +
  geom_errorbar(aes(ymin=Low, ymax=High), colour="grey", width=1, size=0.1, alpha=1, linetype=2) +
  ggtitle("Historic & Future Incidence of Hip Fracture (all ages)") +
  scale_y_continuous(breaks=seq(20,60,5), name = "Cases per 100k population at risk") +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y", name = "Year") +
  theme_classic() +
  theme(plot.title = element_text(color="black", size=12, face="bold.italic"), axis.title.x = element_text(color="red", size=12, face="bold"), 
        axis.title.y = element_text(color="red", size=12, face="bold")) +
  theme(legend.title=element_blank())

plot_projection_all

# 90+ #
Hip_projections_90_ <- read_excel("~/Current projects/Hip # projections/Hip # projections 90+.xlsx", 
                                  sheet = "Combined")
Hip_projections_90_ <- Hip_projections_90_ %>% mutate('date' = make_date(year = Year, month = Month, day = 1))
Hip_projections_90_$date <- lubridate::myd(paste(Hip_projections_90_$Month, Hip_projections_90_$Year, "1"))

View(Hip_projections_90_)

p1 <- ggplot(Hip_projections_90_,mapping = aes(x = date, y = Incidence)) +
  geom_point(mapping = aes(color = act_pred)) +
  geom_smooth(color="black",se = FALSE) +
  geom_errorbar(aes(ymin=Low, ymax=High), colour="grey", width=1, size=0.1, alpha=1, linetype=2) +
  ggtitle("Historic & Future Incidence of Hip Fracture (90+)") +
  scale_y_continuous(breaks=seq(0,4,0.2), name = "Cases per 100k population at risk") +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y", name = "Year") +
  theme_classic() +
  theme(plot.title = element_text(color="black", size=12, face="bold.italic"), axis.title.x = element_text(color="red", size=12, face="bold"), 
        axis.title.y = element_text(color="red", size=12, face="bold")) +
  theme(legend.title=element_blank())
p1

#80-89#
Hip_projections_80_89 <- read_excel("~/Current projects/Hip # projections/Hip # projections 80-89.xlsx", 
                                  sheet = "Combined")
Hip_projections_80_89 <- Hip_projections_80_89 %>% mutate('date' = make_date(year = Year, month = Month, day = 1))
Hip_projections_80_89$date <- lubridate::myd(paste(Hip_projections_80_89$Month, Hip_projections_80_89$Year, "1"))

View(Hip_projections_80_89)

p2 <- ggplot(Hip_projections_80_89,mapping = aes(x = date, y = Incidence)) +
  geom_point(mapping = aes(color = act_pred)) +
  geom_smooth(color="black",se = FALSE) +
  geom_errorbar(aes(ymin=Low, ymax=High), colour="grey", width=1, size=0.1, alpha=1, linetype=2) +
  ggtitle("Historic & Future Incidence of Hip Fracture (80-89)") +
  scale_y_continuous(breaks=seq(20,60,5), name = "Cases per 100k population at risk") +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y", name = "Year") +
  theme_classic() +
  theme(plot.title = element_text(color="black", size=12, face="bold.italic"), axis.title.x = element_text(color="red", size=12, face="bold"), 
        axis.title.y = element_text(color="red", size=12, face="bold")) +
  theme(legend.title=element_blank())
p2

#70-79#
Hip_projections_70_79 <- read_excel("~/Current projects/Hip # projections/Hip # projections 70-79.xlsx", 
                                    sheet = "Combined")
Hip_projections_70_79 <- Hip_projections_70_79 %>% mutate('date' = make_date(year = Year, month = Month, day = 1))
Hip_projections_70_79$date <- lubridate::myd(paste(Hip_projections_70_79$Month, Hip_projections_70_79$Year, "1"))

View(Hip_projections_70_79)

p3 <- ggplot(Hip_projections_70_79,mapping = aes(x = date, y = Incidence)) +
  geom_point(mapping = aes(color = act_pred)) +
  geom_smooth(color="black",se = FALSE) +
  geom_errorbar(aes(ymin=Low, ymax=High), colour="grey", width=1, size=0.1, alpha=1, linetype=2) +
  ggtitle("Historic & Future Incidence of Hip Fracture (70-79)") +
  scale_y_continuous(breaks=seq(20,60,5), name = "Cases per 100k population at risk") +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y", name = "Year") +
  theme_classic() +
  theme(plot.title = element_text(color="black", size=12, face="bold.italic"), axis.title.x = element_text(color="red", size=12, face="bold"), 
        axis.title.y = element_text(color="red", size=12, face="bold")) +
  theme(legend.title=element_blank())
p3

#60-69#
Hip_projections_60_69 <- read_excel("~/Current projects/Hip # projections/Hip # projections 60-69.xlsx", 
                                    sheet = "Combined")
Hip_projections_60_69 <- Hip_projections_60_69 %>% mutate('date' = make_date(year = Year, month = Month, day = 1))
Hip_projections_60_69$date <- lubridate::myd(paste(Hip_projections_60_69$Month, Hip_projections_60_69$Year, "1"))

View(Hip_projections_60_69)

p4 <- ggplot(Hip_projections_60_69,mapping = aes(x = date, y = Incidence)) +
  geom_point(mapping = aes(color = act_pred)) +
  geom_smooth(color="black",se = FALSE) +
  geom_errorbar(aes(ymin=Low, ymax=High), colour="grey", width=1, size=0.1, alpha=1, linetype=2) +
  ggtitle("Historic & Future Incidence of Hip Fracture (60-69)") +
  scale_y_continuous(breaks=seq(20,60,5), name = "Cases per 100k population at risk") +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y", name = "Year") +
  theme_classic() +
  theme(plot.title = element_text(color="black", size=12, face="bold.italic"), axis.title.x = element_text(color="red", size=12, face="bold"), 
        axis.title.y = element_text(color="red", size=12, face="bold")) +
  theme(legend.title=element_blank())
p4

#50-59#
Hip_projections_50_59 <- read_excel("~/Current projects/Hip # projections/Hip # projections 50-59.xlsx", 
                                    sheet = "Combined")
Hip_projections_50_59 <- Hip_projections_50_59 %>% mutate('date' = make_date(year = Year, month = Month, day = 1))
Hip_projections_50_59$date <- lubridate::myd(paste(Hip_projections_50_59$Month, Hip_projections_50_59$Year, "1"))

View(Hip_projections_50_59)

p5 <- ggplot(Hip_projections_50_59,mapping = aes(x = date, y = Incidence)) +
  geom_point(mapping = aes(color = act_pred)) +
  geom_smooth(color="black",se = FALSE) +
  geom_errorbar(aes(ymin=Low, ymax=High), colour="grey", width=1, size=0.1, alpha=1, linetype=2) +
  ggtitle("Historic & Future Incidence of Hip Fracture (50-59)") +
  scale_y_continuous(breaks=seq(20,60,5), name = "Cases per 100k population at risk") +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y", name = "Year") +
  theme_classic() +
  theme(plot.title = element_text(color="black", size=12, face="bold.italic"), axis.title.x = element_text(color="red", size=12, face="bold"), 
        axis.title.y = element_text(color="red", size=12, face="bold")) +
  theme(legend.title=element_blank())
p5

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(p1, p4, p2, p5, p3, cols=3)

