library(prophet); library(tidyverse); library(skimr); library(MLmetrics)

setwd("~/Dimensionamento/Meta 2/Metodologia 4 - necessidades/prophet")

# aplicando sinasc --------------------------------------------------------


sinasc_go <- fetch_datasus(year_start = 1996, year_end = 2019, information_system = "SINASC",
                           month_start = 1, month_end = 12, uf="GO") %>% 
  janitor::clean_names()

sinasc_go_tratado <- sinasc_go %>% 
  select(codestab,codmunnasc,dtnasc,codmunres) %>% 
  group_by(dtnasc) %>% 
  summarise(total = n())

sinasc_go_tratado$dtnasc <- lubridate::dmy(sinasc_go_tratado$dtnasc)


# separando teste e treino  -----------------------------------------------

teste <- sinasc_go_tratado %>%  
          filter(dtnasc > "2018-12-31")

sinasc_go_tratado <- sinasc_go_tratado %>% 
          filter(dtnasc < "2019-01-01")


# prophet sinasc ----------------------------------------------------------

sinasc_go_tratado <- sinasc_go_tratado %>% 
  rename(ds = dtnasc, y = total)

m <- prophet(sinasc_go_tratado)

future <- make_future_dataframe(m, periods = 365)

forecast <- predict(m, future)

plot(m, forecast) + theme_bw()
prophet_plot_components(m, forecast) + theme_bw()

dyplot.prophet(m, forecast)

# Comparando --------------------------------------------------------------

forecast_2019 <- forecast %>%  
  select(ds, yhat, yhat_lower, yhat_upper) %>% 
  filter(ds > "2018-12-31")

forecast_2019$dtnasc <- as.Date(forecast_2019$ds)

resultado <- forecast_2019 %>% 
  left_join(teste, by = "dtnasc") %>% 
  mutate(diferenca = yhat - total) %>%  
  mutate(diferenca_2 = diferenca ^ 2) %>% 
  mutate(weekday = weekdays(ds)) %>% 
  mutate(dentro = if_else(total > yhat_lower & total < yhat_upper, "dentro de intervalos", "fora de intervalos"))


maiores_erros <- resultado %>% 
                    filter(diferenca > 15 & diferenca > -15) %>% 
                    group_by(weekday) %>%
                    count()


# plotando  ---------------------------------------------------------------

ggplot(resultado) +
  geom_line(aes(ds,yhat), col = "blue", size = 1) + 
  geom_line(aes(ds,total), col = "#C62603", size = 1) + 
  theme_bw() + ylim(c(0,500))


resultado %>% 
  ggplot(aes(ds,diferenca)) + geom_point() + geom_hline(yintercept = 4.095)

resultado %>% 
  ggplot(aes(total,yhat)) + geom_point() + geom_smooth(method = "lm") + 
  facet_wrap(~weekday, scales = "free")

resultado %>% 
ggplot(aes(total,yhat)) + geom_point(aes(col = weekday)) + geom_smooth(method = "lm") + theme_bw()

resultado %>% 
  ggplot(aes(ds, diferenca)) + geom_line()

summary(resultado$diferenca)
sd(resultado$diferenca)
cor(resultado$yhat, resultado$total)

sqrt(sum(resultado$diferenca_2)/365)

R2_Score(resultado$yhat, resultado$total)


