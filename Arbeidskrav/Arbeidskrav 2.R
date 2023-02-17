## ASSIGNMENT 2

###############################################################################
#TASK 1
###############################################################################
#cleanup
rm(list=ls())
#packages
library(tidyverse) #data clean
library(rvest) #read tables
library(naniar)#replace_with_na function

#Motors range stats
url <- "https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132"

#reads the first table from the html
df <- url %>%
  read_html() %>% 
  html_nodes("table") %>%  
  html_table() %>% .[[1]]

#colnames from first row
colnames(df) <- df[1,]

#removes first row, since this is the colnames
df <- df[-1, ] 

#puts x values to NA, then removes na rows.
df <- df %>% replace_with_na_all(condition = ~.x == "x")  %>%
  na.omit() 
 
#seperates the unit from the number and renames model column
df <- df %>% separate("WLTP-tall", 
          into=c("WLTP", "Effekt"), 
          sep="/") %>% 
  separate("WLTP", 
           into=c("wltp_km", "var_wltp"), 
           sep=" ") %>% 
  separate("Effekt", 
           into=c("effekt_kwh", "var_effekt"), 
           sep=" ") %>% 
  separate("STOPP", 
           into=c("stopp_km", "var_stopp"), 
           sep=" ") %>% 
  separate("Avvik", 
           into=c("avvik_percent", "var_avvik"), 
           sep=" ") %>% 
  rename(modell = `Modell (temp. varierte fra 0° til -10°)`) 

#selects only wanted columns and turns values into numeric values
df <- df %>%
  select(c(modell, wltp_km, effekt_kwh, stopp_km, avvik_percent)) %>%
  mutate(wltp_km = as.numeric(wltp_km), 
         stopp_km = as.numeric(stopp_km))
#x and y axis limits
xy.limits <- range( c(200,700) )

#plot task 1
df %>%
  ggplot(aes(x = wltp_km, y = stopp_km)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color="red") +
  scale_x_continuous(limits=xy.limits) + 
  scale_y_continuous(limits=xy.limits) + 
  coord_fixed( ratio=1) + 
  theme_minimal() +
  labs(title="Range of new electric cars: specifications vs actual", y="Range (producer)", 
       x="Range (Actual)", caption ="Source: Motor.no (2023)",
       subtitle = "Red line shows how far the cars should have been going. 
All values in km.") 

###############################################################################
#TASK 2
###############################################################################

lm(formula = stopp_km ~ wltp_km, data = df)

#from runnning the code above, you can see where the linear line would 
#intercept the y axis (-26.645) aswell as the rate of increase for the linear
#line of the regression (0.8671).

#plot task 2
df %>%
  ggplot(aes(x = wltp_km, y = stopp_km)) +
  geom_point() + 
  geom_smooth(method=lm) +
  geom_abline(intercept = 0, slope = 1, color="red") +
  scale_x_continuous(limits=xy.limits) + 
  scale_y_continuous(limits=xy.limits) + 
  coord_fixed( ratio=1) + 
  theme_minimal() +
  labs(title="Range of new electric cars: specifications vs actual", y="Range (producer)", 
       x="Range (Actual)", caption ="Source: Motor.no (2023)",
       subtitle = "Red line shows how far the cars should have been going. 
All values in km.") 

#this new linear line (blue) uses the regression method mentioned from the lm
#function to calculate the intercept of the y axis (-26.654) and the rate of
#increasing (0.8671). We can the the rate of increasing is lower than the red
#line showing how far the cars should have been going. We could still find the
#correlation between the actual and how far it should have been going. This is 
#the rate of increaing.