df <- temp2 %>% filter(state%in%c(ufs)) %>% group_by(ano =epiyear(DT_SIN_PRI),week = epiweek(DT_SIN_PRI)) %>% summarise(n= sum(n)) %>% as.data.frame()


df$week[df$week=='53'] <- "52"

df <- df %>% group_by(ano,week) %>% summarise(n= sum(n)) %>% as.data.frame()

df$date <-  str_c(as.character(df$ano),as.character(df$week),'1', sep = '-')

df$date <- as.Date(df$date, "%Y-%U-%u")

df$n <- log(df$n)

df <- df %>% select(n,date)


splits <- initial_time_split(df, prop = 0.92)

recipe_spec <- recipe(n ~ date, training(splits)) %>%
  step_timeseries_signature(date) %>%
  #step_normalize(date_index.num, date_year) %>% 
  step_fourier(date, period = 365, K = 5) %>%
  step_dummy(all_nominal())




set.seed(123)

modelo_1 <- arima_boost(min_n = 2,learn_rate = 0.015,seasonal_period = 52) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(n ~ date, data = training(splits))


#modelo 2 : 
modelo_2 <- workflow() %>%
  add_model(
    spec = boost_tree(
      mode = "regression"
    ) %>%
      set_engine("xgboost")
  ) %>%
  add_recipe(recipe_spec %>% 
               update_role(date, new_role = "indicator")) %>%
  fit(training(splits)) 



#modelo 3 : PROPHET
modelo_3 <- prophet_reg(seasonality_weekly = T) %>%
  set_engine(engine = "prophet") %>%
  fit(n ~ date, data = training(splits))


modelo_RF <- workflow() %>%
  add_model(spec = rand_forest(trees=500, min_n=45, mode="regression") %>%  set_engine("randomForest")) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>% fit(training(splits))

#model 6
modelo_6 <- seasonal_reg(seasonal_period_1 = 52) %>%
  set_engine(engine = "stlm_arima") %>%
  fit(n ~ date, data = training(splits))

#model 7
modelo_7 <- seasonal_reg(seasonal_period_1 = 52) %>%
  set_engine(engine = "stlm_ets") %>%
  fit(n ~ date, data = training(splits))


tabela_de_modelos <- modeltime_table(
  modelo_1,
  modelo_2,
  modelo_3,
  modelo_RF,
  modelo_6,
  modelo_7
  # modelo_10
)

previsoes <- tabela_de_modelos %>%
  modeltime_calibrate(new_data = testing(splits))

#plotando das previs?es dos modelos com os valores reais
previsoes %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = df
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 30, # For mobile screens
  )

#Tabela com as m?tricas de avalia??o para cada um dos modelos
previsoes %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.round_digits = 4)
