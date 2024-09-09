library(readxl)
df <- read_excel("C:/Users/Martyna/Desktop/projekt Szymek/BT_microbiome_fitness_18-19/standarized_data_bt.xlsx")



model_mass_shanon <- lm(MEAN_MASS ~ age+sex+year+seq_date+shannon_entropy_stand+cluth_manipulation+LD+sex*cluth_manipulation, data=df) 
summary(model_mass_shanon)
par(mfrow = c(2, 2))
plot(model_mass_shanon)

model_mass_evenness <- lm(MEAN_MASS ~ age+sex+year+seq_date+pielou_evenness_stand+cluth_manipulation+LD+sex*cluth_manipulation, data=df)
summary(model_mass_evenness)
plot(model_mass_evenness)

model_mass_faith <- lm(MEAN_MASS ~ age+sex+year+seq_date+faith_pd_stand+cluth_manipulation+LD+sex*cluth_manipulation, data=df)
summary(model_mass_faith)
plot(model_mass_faith)

model_mass_observed_f <- lm(MEAN_MASS ~ age+sex+year+seq_date+observed_features_stand+cluth_manipulation+LD+sex*cluth_manipulation, data=df)
summary(model_mass_observed_f)
plot(model_mass_observed_f)

model_tars_shanon <- lm(MEAN_TARS ~ age+sex+year+seq_date+shannon_entropy_stand+cluth_manipulation+LD+sex*cluth_manipulation, data=df) 
summary(model_tars_shanon)
plot(model_tars_shanon)

model_tars_evenness <-lm(MEAN_TARS ~ age+sex+year+seq_date+pielou_evenness_stand+cluth_manipulation+LD+sex*cluth_manipulation, data=df)
summary(model_tars_evenness)
plot(model_tars_evenness)

model_tars_faith <- lm(MEAN_TARS ~ age+sex+year+seq_date+faith_pd_stand+cluth_manipulation+LD+sex*cluth_manipulation, data=df)
summary(model_tars_faith)
plot(model_tars_faith)

model_tars_observed_f <- lm(MEAN_TARS ~ age+sex+year+seq_date+observed_features_stand+cluth_manipulation+LD+sex*cluth_manipulation, data=df)
summary(model_tars_observed_f)
plot(model_tars_observed_f)

model_cs_shanon <- glm(CS ~ age+sex+year+seq_date+shannon_entropy_stand+cluth_manipulation+LD, data=df) 
summary(model_cs_shanon)
plot(model_cs_shanon)

model_cs_evenness <-glm(CS ~ age+sex+year+seq_date+pielou_evenness_stand+cluth_manipulation+LD, data=df)
summary(model_cs_evenness)
plot(model_cs_evenness)

model_cs_faith <- glm(CS ~ age+sex+year+seq_date+faith_pd_stand+cluth_manipulation+LD, data=df)
summary(model_cs_faith)
plot(model_cs_faith)

model_cs_observed_f <- glm(CS ~ age+sex+year+seq_date+observed_features_stand+cluth_manipulation+LD, data=df)
summary(model_cs_observed_f)
plot(model_cs_observed_f)


model_chicks_shanon <- glm(chicks_number ~ age+sex+year+seq_date+shannon_entropy_stand+cluth_manipulation+LD, data=df) 
summary(model_chicks_shanon)
plot(model_chicks_shanon)

model_chicks_evenness <-glm(chicks_number ~ age+sex+year+seq_date+pielou_evenness_stand+cluth_manipulation+LD, data=df)
summary(model_chicks_evenness)
plot(model_chicks_evenness)

model_chicks_faith <- glm(chicks_number ~ age+sex+year+seq_date+faith_pd_stand+cluth_manipulation+LD, data=df)
summary(model_chicks_faith)
plot(model_chicks_faith)

model_chicks_observed_f <- glm(chicks_number ~ age+sex+year+seq_date+observed_features_stand+cluth_manipulation+LD, data=df)
summary(model_chicks_observed_f)
plot(model_chicks_observed_f)
