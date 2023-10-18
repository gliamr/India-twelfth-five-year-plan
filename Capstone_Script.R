library(readxl)
library(tidyverse)
library(olsrr)
library(modelr)
library(tidysynth)
library(stargazer)
library(sjPlot)
library(dplyr)

df<-read_excel("growth_3.xlsx")

var1<-df$enrollment
df2 <- df %>% filter(!is.na(var1))

fit<-lm(gdpgrowth~enrollment,data=df2)
growth <- df %>% 
  mutate(pred = predict(fit, .)) %>%
  # Replace NA with pred in var1
  mutate(var1 = ifelse(is.na(var1), pred, var1))

growth$time <- ifelse(growth$year >= 2012, 1, 0)

growth$did <- growth$treated*growth$time

didreg <- lm(gdpgrowth~treated+time+did+enrollment,data=growth)

summary(didreg)

tab_model(didreg, p.style="stars",
       file="didreg.doc")

windowsFonts(Times=windowsFont("Times New Roman"))
Helvetica=windowsFont("Helvetica 45 Light")

ggplot(growth, aes(year, gdpgrowth, color = Country)) +
  labs(caption="Source: World Bank", title="GDP Growth, percentage, India vs. Bangladesh (1973 - 2019)", 
       x="Year", y="GDP Growth (%)") +
  scale_color_manual(values=c('#f42a41','#FF9933')) +
  stat_summary(geom = 'line', lwd=1) +
  geom_vline(xintercept = 2012) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  theme_bw() +
  theme(text=element_text(family="Times", size=14))
  
growth<-add_residuals(growth,didreg)

ggplot(data=growth, aes(x=gdpgrowth,y=resid)) +
  geom_point()

growth.out<-growth %>%
  synthetic_control(outcome=gdpgrowth,
                    unit=Country,
                    time=year,
                    i_unit="India",
                    i_time=2012,
                    generate_placebos=T) %>%
  
  generate_predictor(time_window = 1970:2011,
                   exports=mean(trade, na.RM=TRUE),
                   rents=mean(rents,na.RM=TRUE)) %>%
  
  generate_predictor(time_window = 1979,
                     gdp_1979 = gdpgrowth) %>%
  generate_predictor(time_window = 1980,
                     gdp_1980 = gdpgrowth) %>%
  generate_predictor(time_window = 1988,
                     gdp_1988 = gdpgrowth) %>%
  generate_predictor(time_window = 1993,
                     gdp_1993 = gdpgrowth) %>%
  generate_predictor(time_window = 1995,
                     gdp_1995 = gdpgrowth) %>%
  generate_predictor(time_window = 2003,
                     gdp_2003 = gdpgrowth) %>%
  generate_predictor(time_window = 2005,
                     gdp_2005 = gdpgrowth) %>%
  generate_predictor(time_window = 2006,
                     gdp_2006 = gdpgrowth) %>%
  generate_predictor(time_window = 2008,
                     gdp_2008 = gdpgrowth) %>%
  generate_predictor(time_window = 2009,
                     gdp_2009 = gdpgrowth) %>%
  generate_predictor(time_window = 2010,
                     gdp_2010 = gdpgrowth) %>%
  
  generate_weights(optimzation_window=1970:2011,
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6) %>%
  generate_control()

growth.out %>% plot_trends()
