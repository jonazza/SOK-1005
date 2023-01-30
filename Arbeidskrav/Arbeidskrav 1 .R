#laster inn pakker og rydder opp
rm(list = ls())
library(tidyverse)
library(lubridate)
library(zoo)
library(ggthemes)

#laster inn datasett
lt <- read.table("http://vortex.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt", fill=TRUE, header=TRUE)
mt <- read.table("http://vortex.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt", fill=TRUE, header=TRUE)
tp <- read.table("http://vortex.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt", fill=TRUE, header=TRUE)
ls <- read.table("http://vortex.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt", fill=TRUE, header=TRUE)

#rydder i data (fjerner bunntekst urelevant i alle datsettene. dynamisk ved oppdatering av datasett.)
lt <- lt[1:which(lt$Year %in% "Year")-1, ]
mt <- mt[1:which(mt$Year %in% "Year")-1, ]
tp <- tp[1:which(tp$Year %in% "Year")-1, ]
ls <- ls[1:which(ls$Year %in% "Year")-1, ]

#lager datokolonne i datasettene og var til å skille dataene når datasettene kombineres:
lt <- lt %>% 
  mutate(Date = ymd(paste(lt$Year, lt$Mo, 1, sep="-"))) %>% 
  mutate(Var = "Lower Troposphere") %>%
  select(Date, Globe, Var) 
mt <- mt %>% 
  mutate(Date = ymd(paste(mt$Year, mt$Mo, 1, sep="-"))) %>% 
  mutate(Var = "Mid Troposphere") %>%
  select(Date, Globe, Var) 
tp <- tp %>% 
  mutate(Date = ymd(paste(tp$Year, tp$Mo, 1, sep="-"))) %>% 
  mutate(Var = "Tropopause") %>%
  select(Date, Globe, Var)
ls <- ls %>% 
  mutate(Date = ymd(paste(ls$Year, ls$Mo, 1, sep="-"))) %>% 
  mutate(Var = "Lower Stratosphere") %>%
  select(Date, Globe, Var) 

#lager ett kombinert datasett. Velger måneder etter desember 1979. Gjør også globe variabel numerisk.
temp_data <- rbind(lt, mt, tp, ls) 
temp_data <- temp_data %>% 
  filter(Date > "1979-12-01") %>%
  mutate(Globe=as.numeric(Globe))

#lager ny avg globe temp, for å kunne lage en 12 month moving average av de ulike 12 month moving avg.
temp_data_avg <- temp_data %>% 
  group_by(Date) %>%
  summarize(Globe = mean(Globe)) %>%
  mutate(Var = "Average")

#legger til avg i datasettet.
temp_data <- rbind(temp_data, temp_data_avg)

#lager 12 måneders moving average
temp_data <- temp_data %>% 
  group_by(Var) %>%
  mutate(twelve_ma = rollmean(Globe, 12, fill=NA)) 

#farger til bruk i plot
myColors = c("#A6611A", "#DFC27D", "#6e6c6b", "#80CDC1", "#018571")

temp_data %>%
  mutate(isAverage = (Var == 'Average')) %>% #lager ny var for å skille ut Average i pplot
  ggplot(aes(x = Date, y = twelve_ma, color = Var)) + #definerer akser og farge
  geom_line(aes(linetype = isAverage), size = 1.1, alpha = 0.8) + #lineplot
  labs(title = "12 Month average global temperature",
       subtitle = "Average global temperature from different areas",
       x = "Year",
       y = "Temperature",
       color = "Var") + #tittler til figur, akser, og color var
  theme_fivethirtyeight() + #tema 
  theme(axis.title = element_text(), text = element_text()) + #navn på figur
  theme(legend.title = element_blank()) + #fjerner legende navn
  scale_linetype_manual(values = c("dashed", "solid"), guide = "none") +#dashed linjer foruten average
  scale_color_manual(values = myColors) #velger farger som definert før plot


  





    
