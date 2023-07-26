##################################################################
# FISH & DATA
# DATA SCIENCE e ANALYTICS
################################################################

# Instalação e Carregamento de Todos os Pacotes ---------------------------

pacotes <- c("readr","readxl","plotly","tidyverse","gridExtra","forecast","TTR",
             "smooth", "tsibble", "fable","tsibbledata", "fpp3","lubridate",
             "urca", "dygraphs", "quantmod","BETS","tseries","FinTS","feasts",
             "gridExtra", "scales", "caret","xtable", "tsutils","GetBCBData", 
             "quantmod","dgof","seasonal","devtools","transformr","gganimate")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

##### PLEASE CONSIDER TO CHANGE FILE LOCATION WHEN RUNNING ON YOUR OWN COMPUTER
#IMPORT DATABASE FROM LONDON DATASTORE
new_base <- read_excel("C:/Users/fprudente/OneDrive/Treinamento/ESALQ USP/IBS- intercambio em Londres/DS1/LEGGI_2020.xlsx",
                                     sheet = "Summary") %>%
  slice(80:105) %>%
  select(c(-1,-25)) 

new_base[1,1] <- "ENERGY_SOURCE"
names(new_base) <- new_base[1,]

new_base[9,1] <- "DOMESTIC"
new_base[17,1] <- "INDUSTRIAL_COMMERCIAL"
new_base[25,1] <- "TRANSPORT"

new_base <- new_base %>%
  slice(9,17,25) 

new_base_pl <- pivot_longer(new_base, !ENERGY_SOURCE,
                      names_to = "PERIOD",
                      values_to = "GWh") 


new_base_pl <- new_base_pl %>%
  mutate(GWh = as.numeric(GWh)) %>%
  select(2,1,3) %>%
  filter(PERIOD != 1990) # Excluding 1990 as it appears as NA 

sum(new_base_pl$GWh)

################################ DOMESTIC CONSUMPTION PREDICTION ########################################

domestic_source <- new_base_pl %>%
  filter(ENERGY_SOURCE == "DOMESTIC")

#TRANFORMING DATA INTO TIME SERIES 
domestic_source=ts(domestic_source[3], start = c(2000), end = c(2020), frequency = 1)
plot(domestic_source)

# SPLITING INTO TRAINNING AND TESTING DATA
domestic_source_trainning=window(domestic_source, start=c(2000), end=c(2016))
domestic_source_testing=window(domestic_source,start=c(2017), end=c(2020))
length(domestic_source_testing)

dygraph(domestic_source)

# ploting both series together for checking
autoplot(domestic_source) +
  autolayer(domestic_source_trainning, series="Trainning") +
  autolayer(domestic_source_testing, series="Testing") +
  scale_color_viridis_d() +
  theme_bw()

## Analyzing the serie 
ggtsdisplay(domestic_source_trainning) 
### NOTE PROBABLY SASOANLITY 

# Dickey-Fuller Testing - IF ACCEPTING H0 - SERIE IS STABILIZED. IF REJECTING H0 - SERIE IS NOT STABLIZED AND REQUIRES A DIFFERENTIATOR. 
testing_df_domestic_source=ur.df(domestic_source_trainning)
summary(testing_df_domestic_source)

arima_domestic_sourcing=auto.arima(domestic_source_trainning, trace=T)

#### Ljung-Box test - IF ACCEPTING H0 - THERE IS NO CORRELATIONS BETWEEN ERRORS. IF H0 IS REJECTED - ERRORS THERE ARE CORRELATIONS BETWEEN ERRORS 
checkresiduals(arima_domestic_sourcing)

# One-sample Kolmogorov-Smirnov testing - ERROS NORMALITY - IF ACCEPTING H0 = ERRORS ARE NORMAL WITH MEAN = 0. IF REJECTING H0 ERROS ARE NOT NORMALIZED
ks.test(arima_domestic_sourcing$residuals, "pnorm", mean(arima_domestic_sourcing$residuals),
        sd(arima_domestic_sourcing$residuals))

# ARCH LM-test; Null hypothesis: no ARCH effects. ACCEPTING H0 = THERE IS NO ARCH EFFECTS (HETEROCIDACITY IN ERRORS). REJECTING HO = THERE IS HETEROCIDACITY EFFECTS IN ERRORS  
ArchTest(arima_domestic_sourcing$residuals)

# PREDICTING ENERGY FOR DOMESTIC CONSUMERS UP TO 2050 
pred_domestic_sourcing=forecast::forecast(arima_domestic_sourcing, h=14)

autoplot(pred_domestic_sourcing) +
  theme_bw()

forecast::accuracy(pred_domestic_sourcing, domestic_source_testing)

ggplotly(
  autoplot(domestic_source_trainning)+
    autolayer(domestic_source_testing,serie="REAL VALUES")+
    autolayer(pred_domestic_sourcing$mean, serie="FORECASTING")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)

# ETS MODELING TEST 

domestic_sourcing_ETS=forecast(ets(domestic_source_trainning),h=14)
summary(domestic_sourcing_ETS)

ggplotly(
  autoplot(domestic_source_trainning)+
    autolayer(domestic_source_testing,serie="REAL VALUES")+
    autolayer(domestic_sourcing_ETS$mean, serie="FORECASTING")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)
autoplot(domestic_sourcing_ETS) +
  theme_bw()

forecast::accuracy(domestic_sourcing_ETS$mean, domestic_source_testing)

#### Ljung-Box test - IF ACCEPTING H0 - THERE IS NO CORRELATIONS BETWEEN ERRORS. IF H0 IS REJECTED - ERRORS THERE ARE CORRELATIONS BETWEEN ERRORS 
checkresiduals(domestic_sourcing_ETS)

# One-sample Kolmogorov-Smirnov testing - ERROS NORMALITY - IF ACCEPTING H0 = ERRORS ARE NORMAL WITH MEAN = 0. IF REJECTING H0 ERROS ARE NOT NORMALIZED
ks.test(domestic_sourcing_ETS$residuals, "pnorm", mean(domestic_sourcing_ETS$residuals),
        sd(domestic_sourcing_ETS$residuals))

# ARCH LM-test; Null hypothesis: no ARCH effects. ACCEPTING H0 = THERE IS NO ARCH EFFECTS (HETEROCIDACITY IN ERRORS). REJECTING HO = THERE IS HETEROCIDACITY EFFECTS IN ERRORS  
ArchTest(domestic_sourcing_ETS$residuals)

# Fazendo uma transformação de Box-Cox
l = BoxCox.lambda(domestic_source)

domestic_source_ETS_BC=forecast(ets(domestic_source_trainning, lambda = l),h=14)
summary(domestic_source_ETS_BC)

autoplot(forecast(domestic_source_ETS_BC,h=14)) +
  xlab("PERIOD") +
  ylab("GWH") +
  ggtitle("DOMESTIC CONSUMPTION GHW ") +
  theme_bw()

#### Ljung-Box test - IF ACCEPTING H0 - THERE IS NO CORRELATIONS BETWEEN ERRORS. IF H0 IS REJECTED - ERRORS THERE ARE CORRELATIONS BETWEEN ERRORS 
checkresiduals(domestic_source_ETS_BC)

# One-sample Kolmogorov-Smirnov testing - ERROS NORMALITY - IF ACCEPTING H0 = ERRORS ARE NORMAL WITH MEAN = 0. IF REJECTING H0 ERROS ARE NOT NORMALIZED
ks.test(domestic_source_ETS_BC$residuals, "pnorm", mean(domestic_source_ETS_BC$residuals),
        sd(domestic_source_ETS_BC$residuals))

# ARCH LM-test; Null hypothesis: no ARCH effects. ACCEPTING H0 = THERE IS NO ARCH EFFECTS (HETEROCIDACITY IN ERRORS). REJECTING HO = THERE IS HETEROCIDACITY EFFECTS IN ERRORS  
ArchTest(domestic_source_ETS_BC$residuals)

forecast::accuracy(domestic_source_ETS_BC,domestic_source_testing)["Test set","MAPE"]


################################ COMERCIAL AND INDUSTRIAL CONSUMPTION PREDICTION ########################################

ind_com_source <- new_base_pl %>%
  filter(ENERGY_SOURCE == "INDUSTRIAL_COMMERCIAL")

#TRANFORMING DATA INTO TIME SERIES 
ind_com_source=ts(ind_com_source[3], start = c(2000), end = c(2020), frequency = 1)
plot(ind_com_source)

# SPLITING INTO TRAINNING AND TESTING DATA
ind_com_source_trainning=window(ind_com_source, start=c(2000), end=c(2016))
ind_com_source_testing=window(ind_com_source,start=c(2017), end=c(2020))
length(ind_com_source_testing)

dygraph(ind_com_source)

# ploting both series together for checking
autoplot(ind_com_source) +
  autolayer(ind_com_source_trainning, series="Trainning") +
  autolayer(ind_com_source_testing, series="Testing") +
  scale_color_viridis_d() +
  theme_bw()

## Analyzing the serie 
ggtsdisplay(ind_com_source_trainning) 
### NOTE PROBABLY SASOANLITY 

# Dickey-Fuller Testing - IF ACCEPTING H0 - SERIE IS STABILIZED. IF REJECTING H0 - SERIE IS NOT STABLIZED AND REQUIRES A DIFFERENTIATOR. 
testing_df_ind_com_source=ur.df(ind_com_source_trainning)
summary(testing_df_ind_com_source)

arima_ind_com_sourcing=auto.arima(ind_com_source_trainning, trace=T)

#### Ljung-Box test - IF ACCEPTING H0 - THERE IS NO CORRELATIONS BETWEEN ERRORS. IF H0 IS REJECTED - ERRORS THERE ARE CORRELATIONS BETWEEN ERRORS 
checkresiduals(arima_ind_com_sourcing)

# One-sample Kolmogorov-Smirnov testing - ERROS NORMALITY - IF ACCEPTING H0 = ERRORS ARE NORMAL WITH MEAN = 0. IF REJECTING H0 ERROS ARE NOT NORMALIZED
ks.test(arima_ind_com_sourcing$residuals, "pnorm", mean(arima_ind_com_sourcing$residuals),
        sd(arima_ind_com_sourcing$residuals))

# ARCH LM-test; Null hypothesis: no ARCH effects. ACCEPTING H0 = THERE IS NO ARCH EFFECTS (HETEROCIDACITY IN ERRORS). REJECTING HO = THERE IS HETEROCIDACITY EFFECTS IN ERRORS  
ArchTest(arima_ind_com_sourcing$residuals)

# PREDICTING ENERGY FOR DOMESTIC CONSUMERS UP TO 2050 
pred_ind_com_sourcing=forecast::forecast(arima_ind_com_sourcing, h=14)

autoplot(pred_ind_com_sourcing) +
  theme_bw()

forecast::accuracy(pred_ind_com_sourcing, ind_com_source_testing)

ggplotly(
  autoplot(ind_com_source_trainning)+
    autolayer(ind_com_source_testing,serie="REAL VALUES")+
    autolayer(pred_ind_com_sourcing$mean, serie="FORECASTING")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)

# ETS MODELING TEST 

ind_com_sourcing_ETS=forecast(ets(ind_com_source_trainning),h=14)
summary(ind_com_sourcing_ETS)

ggplotly(
  autoplot(ind_com_source_trainning)+
    autolayer(ind_com_source_testing,serie="REAL VALUES")+
    autolayer(ind_com_sourcing_ETS$mean, serie="FORECASTING")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)
autoplot(ind_com_sourcing_ETS) +
  theme_bw()

forecast::accuracy(ind_com_sourcing_ETS$mean, ind_com_source_testing)

#### Ljung-Box test - IF ACCEPTING H0 - THERE IS NO CORRELATIONS BETWEEN ERRORS. IF H0 IS REJECTED - ERRORS THERE ARE CORRELATIONS BETWEEN ERRORS 
checkresiduals(ind_com_sourcing_ETS)

# One-sample Kolmogorov-Smirnov testing - ERROS NORMALITY - IF ACCEPTING H0 = ERRORS ARE NORMAL WITH MEAN = 0. IF REJECTING H0 ERROS ARE NOT NORMALIZED
ks.test(ind_com_sourcing_ETS$residuals, "pnorm", mean(ind_com_sourcing_ETS$residuals),
        sd(ind_com_sourcing_ETS$residuals))

# ARCH LM-test; Null hypothesis: no ARCH effects. ACCEPTING H0 = THERE IS NO ARCH EFFECTS (HETEROCIDACITY IN ERRORS). REJECTING HO = THERE IS HETEROCIDACITY EFFECTS IN ERRORS  
ArchTest(ind_com_sourcing_ETS$residuals)

# Fazendo uma transformação de Box-Cox
l = BoxCox.lambda(ind_com_source)

ind_com_source_ETS_BC=forecast(ets(ind_com_source_trainning, lambda = l),h=14)
summary(ind_com_source_ETS_BC)

autoplot(forecast(ind_com_source_ETS_BC,h=14)) +
  xlab("PERIOD") +
  ylab("GWH") +
  ggtitle("INDUSTRIAL AND COMMERCIAL CONSUMPTION GHW ") +
  theme_bw()

forecast::accuracy(ind_com_source_ETS_BC,ind_com_source_testing)["Test set","MAPE"]

#### Ljung-Box test - IF ACCEPTING H0 - THERE IS NO CORRELATIONS BETWEEN ERRORS. IF H0 IS REJECTED - ERRORS THERE ARE CORRELATIONS BETWEEN ERRORS 
checkresiduals(ind_com_source_ETS_BC)

# One-sample Kolmogorov-Smirnov testing - ERROS NORMALITY - IF ACCEPTING H0 = ERRORS ARE NORMAL WITH MEAN = 0. IF REJECTING H0 ERROS ARE NOT NORMALIZED
ks.test(ind_com_source_ETS_BC$residuals, "pnorm", mean(ind_com_source_ETS_BC$residuals),
        sd(ind_com_source_ETS_BC$residuals))

# ARCH LM-test; Null hypothesis: no ARCH effects. ACCEPTING H0 = THERE IS NO ARCH EFFECTS (HETEROCIDACITY IN ERRORS). REJECTING HO = THERE IS HETEROCIDACITY EFFECTS IN ERRORS  
ArchTest(ind_com_source_ETS_BC$residuals)


################################ TRANSPORT CONSUMPTION PREDICTION ########################################

transp_source <- new_base_pl %>%
  filter(ENERGY_SOURCE == "TRANSPORT")

#TRANFORMING DATA INTO TIME SERIES 
transp_source=ts(transp_source[3], start = c(2000), end = c(2020), frequency = 1)
plot(transp_source)

# SPLITING INTO TRAINNING AND TESTING DATA
transp_source_trainning=window(transp_source, start=c(2000), end=c(2019))
transp_source_testing=window(transp_source,start=c(2020), end=c(2020))
length(transp_source_testing)

dygraph(transp_source)

# ploting both series together for checking
autoplot(transp_source) +
  autolayer(transp_source_trainning, series="Trainning") +
  autolayer(transp_source_testing, series="Testing") +
  scale_color_viridis_d() +
  theme_bw()

## Analyzing the serie 
ggtsdisplay(transp_source_trainning) 
### NOTE PROBABLY SASOANLITY 

# Dickey-Fuller Testing - IF ACCEPTING H0 - SERIE IS STABILIZED. IF REJECTING H0 - SERIE IS NOT STABLIZED AND REQUIRES A DIFFERENTIATOR. 
testing_df_transp_source=ur.df(transp_source_trainning)
summary(testing_df_transp_source)

arima_transp_sourcing=auto.arima(transp_source_trainning, trace=T)

#### Ljung-Box test - IF ACCEPTING H0 - THERE IS NO CORRELATIONS BETWEEN ERRORS. IF H0 IS REJECTED - ERRORS THERE ARE CORRELATIONS BETWEEN ERRORS 
checkresiduals(arima_transp_sourcing)

boxplot(arima_transp_sourcing$residuals)

# One-sample Kolmogorov-Smirnov testing - ERROS NORMALITY - IF ACCEPTING H0 = ERRORS ARE NORMAL WITH MEAN = 0. IF REJECTING H0 ERROS ARE NOT NORMALIZED
ks.test(arima_transp_sourcing$residuals, "pnorm", mean(arima_transp_sourcing$residuals),
        sd(arima_transp_sourcing$residuals))

# ARCH LM-test; Null hypothesis: no ARCH effects. ACCEPTING H0 = THERE IS NO ARCH EFFECTS (HETEROCIDACITY IN ERRORS). REJECTING HO = THERE IS HETEROCIDACITY EFFECTS IN ERRORS  
ArchTest(arima_transp_sourcing$residuals)

# PREDICTING ENERGY FOR DOMESTIC CONSUMERS UP TO 2050 
pred_transp_sourcing=forecast::forecast(arima_transp_sourcing, h=11)

autoplot(pred_transp_sourcing) +
  theme_bw()

forecast::accuracy(pred_transp_sourcing, transp_source_testing)

ggplotly(
  autoplot(transp_source_trainning)+
    autolayer(transp_source_testing,serie="REAL VALUES")+
    autolayer(pred_transp_sourcing$mean, serie="FORECASTING")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)

# ETS MODELING TEST 

transp_sourcing_ETS=forecast(ets(transp_source_trainning),h=11)
summary(transp_sourcing_ETS)

ggplotly(
  autoplot(transp_source_trainning)+
    autolayer(transp_source_testing,serie="REAL VALUES")+
    autolayer(transp_sourcing_ETS$mean, serie="FORECASTING")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)
autoplot(transp_sourcing_ETS) +
  theme_bw()

forecast::accuracy(transp_sourcing_ETS$mean, transp_source_testing)

#### Ljung-Box test - IF ACCEPTING H0 - THERE IS NO CORRELATIONS BETWEEN ERRORS. IF H0 IS REJECTED - ERRORS THERE ARE CORRELATIONS BETWEEN ERRORS 
checkresiduals(transp_sourcing_ETS)

boxplot(transp_sourcing_ETS$residuals)

# One-sample Kolmogorov-Smirnov testing - ERROS NORMALITY - IF ACCEPTING H0 = ERRORS ARE NORMAL WITH MEAN = 0. IF REJECTING H0 ERROS ARE NOT NORMALIZED
ks.test(transp_sourcing_ETS$residuals, "pnorm", mean(transp_sourcing_ETS$residuals),
        sd(transp_sourcing_ETS$residuals))

# ARCH LM-test; Null hypothesis: no ARCH effects. ACCEPTING H0 = THERE IS NO ARCH EFFECTS (HETEROCIDACITY IN ERRORS). REJECTING HO = THERE IS HETEROCIDACITY EFFECTS IN ERRORS  
ArchTest(transp_sourcing_ETS$residuals)

# Fazendo uma transformação de Box-Cox
l = BoxCox.lambda(transp_source)

transp_source_ETS_BC=forecast(ets(transp_source_trainning, lambda = l),h=11)
summary(transp_source_ETS_BC)

autoplot(forecast(transp_source_ETS_BC,h=11)) +
  xlab("PERIOD") +
  ylab("GWH") +
  ggtitle("TRANSPORT CONSUMPTION GHW ") +
  theme_bw()

forecast::accuracy(transp_source_ETS_BC,transp_source_testing)["Test set","MAPE"]

#### Ljung-Box test - IF ACCEPTING H0 - THERE IS NO CORRELATIONS BETWEEN ERRORS. IF H0 IS REJECTED - ERRORS THERE ARE CORRELATIONS BETWEEN ERRORS 
checkresiduals(transp_source_ETS_BC)

boxplot(transp_source_ETS_BC$residuals)

# One-sample Kolmogorov-Smirnov testing - ERROS NORMALITY - IF ACCEPTING H0 = ERRORS ARE NORMAL WITH MEAN = 0. IF REJECTING H0 ERROS ARE NOT NORMALIZED
ks.test(transp_source_ETS_BC$residuals, "pnorm", mean(transp_source_ETS_BC$residuals),
        sd(transp_source_ETS_BC$residuals))

# ARCH LM-test; Null hypothesis: no ARCH effects. ACCEPTING H0 = THERE IS NO ARCH EFFECTS (HETEROCIDACITY IN ERRORS). REJECTING HO = THERE IS HETEROCIDACITY EFFECTS IN ERRORS  
ArchTest(transp_source_ETS_BC$residuals)

# Analisando as acurácias das previsões - DOMESTIC
forecast::accuracy(pred_domestic_sourcing, domestic_source_testing)
forecast::accuracy(domestic_sourcing_ETS, domestic_source_testing)
forecast::accuracy(domestic_source_ETS_BC, domestic_source_testing)

# Analisando as acurácias das previsões - IND_COM
forecast::accuracy(pred_ind_com_sourcing, ind_com_source_testing)
forecast::accuracy(ind_com_sourcing_ETS, ind_com_source_testing)
forecast::accuracy(ind_com_source_ETS_BC, ind_com_source_testing)

# Analisando as acurácias das previsões - TRANSP 
forecast::accuracy(pred_transp_sourcing, transp_source_testing)
forecast::accuracy(transp_sourcing_ETS, transp_source_testing)
forecast::accuracy(transp_source_ETS_BC, transp_source_testing)


pred_domestic_sourcing$mean[5:14]
ind_com_sourcing_ETS$mean[5:14]
pred_transp_sourcing$mean[2:11]

consol_models_consumption_energy <- data_frame("Period" = c(2021:2030),
                                               "Domestic Energy Consumption" = pred_domestic_sourcing$mean[5:14],
                                               "Industrial & Commercial Energy Consumption" = ind_com_sourcing_ETS$mean[5:14],
                                               "Transport energy consumption" = pred_transp_sourcing$mean[2:11]) %>%
  mutate(Period = as.numeric(Period))


domestic_base <- new_base_pl %>%
  filter(ENERGY_SOURCE == "DOMESTIC") %>%
  rename("Period" = 1, "Domestic Energy Consumption" = 3) %>%
  select(1,3) %>%
  mutate(Period = as.numeric(Period))

ind_com_base <- new_base_pl %>%
  filter(ENERGY_SOURCE == "INDUSTRIAL_COMMERCIAL") %>%
  rename("Period" = 1, "Industrial & Commercial Energy Consumption" = 3) %>%
  select(1,3) %>%
  mutate(Period = as.numeric(Period))

transp_base <- new_base_pl %>%
  filter(ENERGY_SOURCE == "TRANSPORT") %>%
  rename("Period" = 1, "Transport energy consumption" = 3) %>%
  select(1,3) %>%
  mutate(Period = as.numeric(Period))

consol_base_hist <- left_join(domestic_base, ind_com_base, by ="Period")
consol_base_hist <- left_join(consol_base_hist, transp_base, by ="Period") 

consol_base_pred_models <- bind_rows(consol_base_hist, consol_models_consumption_energy)

glimpse(consol_base_hist)
glimpse(consol_models_consumption_energy)

############### PLEASE CONSIDER TO CHANGE FILE LOCATION WHEN SAVING ON YOUR OWN COMPUTER 
writexl::write_xlsx(consol_base_pred_models, "C:/Users/fprudente/OneDrive/Treinamento/ESALQ USP/IBS- intercambio em Londres/DS1/consol_energy_consumption.xlsx")  
