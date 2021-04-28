####Modeling ###
####Shanelle Wikramanayake###
#### 2021-03-28####

### Load libraries ###
library(tidyverse)
library(here)
library(palmerpenguins)
library(broom)
library(performance)
library(modelsummary)
library(tidymodels)

###Load data###
Peng_mod<-lm(bill_length_mm ~ bill_depth_mm*species, data = penguins) #ANCOVA with penguin data 

#check your model assumption
check_model(Peng_mod) #Produces lots of nice plots and uses lots of different packages using the performance package


#view your results in base R
anova(Peng_mod) #gives you df, mean squared, F and p values

summary(Peng_mod) #gives standard errors, r squared, f statistics, min, median, quartiles, max
#this an important output ^^

#broom makes all your coefficient outputs into a df
coeffs <- tidy(Peng_mod)
coeffs #very nice

results <- glance(Peng_mod) #the model statistics into a tibble
results

resid_fitted <- augment(Peng_mod) #adds residuals and predicted values to your original data and requires that yuo put in both model and data
resid_fitted

#export model summary data into a word 

Peng_mod_noX <- lm(bill_length_mm ~ bill_depth_mm, data = penguins)
models <- list("Model with interaction" = Peng_mod, 
               "Model without interaction" = Peng_mod_noX)

modelsummary(models, output = here("week13", "Outputs", "table.docx"))



modelplot(models) +
  labs(x = 'Coefficients', 
       y = 'Term names') +
  scale_colour_viridis_d()

#if you want to make a model for each species you can make multiple independent models in a single data set

models<- penguins %>%
  ungroup()%>% #ungroup the penguin data cos it's grouped. COuld also group by species and island and then you can look at both of those too
  nest(-species) %>%  #nest data except for species so nests everything by species 
  mutate(fit = map(data, ~lm(bill_length_mm~body_mass_g, data = .))) # the fullstop makes it broad. Applies the linear model to the three different dfs


models$fit
#outputs have different dfs for each species


results <- models %>% 
  mutate(coeffs = map(fit, tidy), 
         modelresults = map(fit, glance)) %>% 
  select(species, coeffs, modelresults) %>% 
  unnest(cols = c(coeffs, modelresults))

results #have species, firs, coefficients, and model results for each species


#Tidy models###
linear_reg() #sets it up to say we are doing linear regression

lm_mod <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(bill_length_mm ~ bill_depth_mm*species, data = penguins) %>% 
  tidy() %>% 
  ggplot()+
  geom_point(aes(x = term, y = estimate))+
  geom_errorbar(aes(x = term, ymin = estimate-std.error,
                    ymax = estimate+std.error), width = 0.1 )+
  coord_flip()


lm_mod













###NOTES###
#name he model something and ~ y as a function of x 
#mod <- lm(y~x, data = df)

#mod <- lm(y~x1_x2, data = lf)