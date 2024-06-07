# Visualization of Likert type data 

library(dplyr)
library(sjPlot)
library(sjmisc)
library(parameters)

# Example for the illustration, not relevant to our case 
# mydf <- find_var(efc, pattern = "cop", out = "df")
# plot_likert(mydf)

# For full_data change for pre and post Q output from numeric to string
full_data_string <- full_data %>% mutate_at(
  vars(starts_with(c("pre.", "post."))),
  funs(case_when(
    .== 1 ~ "Strongly Disagree" , 
    .== 2 ~ "Disagree" ,
    .== 3 ~ "Somewhat Disagree", 
    .== 4 ~ "Neither Agree or Disagree" ,
    .== 5 ~ "Somewhat Agree" , 
    .== 6 ~ "Agree"  , 
    .== 7 ~ "Strongly Agree"  
  ))
)

# Consider new data for likert ploting 

# For pre test, Affect component 
mydf_pre_Affect <- full_data %>% select(c("pre.Q3", "pre.Q4", "pre.Q15", "pre.Q18", "pre.Q19", "pre.Q28"))
#names <- c(1:6)
#mydf_pre_Affect[, names] <- lapply(mydf_pre_Affect[, names], factor)

attributes(mydf_pre_Affect$pre.Q3)$label <- "I will like Statistics"
attributes(mydf_pre_Affect$pre.Q3)$labels <- setNames(c(1:7), c('Strongly Disagree', 'Disagree', 'Somewhat Disagree', 'Neither Agree or Disagree', 
                   'Somewhat Agree', 'Agree', 'Strongly Agree'))

attributes(mydf_pre_Affect$pre.Q4)$label <- "I will feel insecure when I have to do Statistics problems"
attributes(mydf_pre_Affect$pre.Q4)$labels <- setNames(c(1:7), c('Strongly Agree', 'Agree', 'Somewhat Agree', 'Neither Agree or Disagree', 
                                                                'Somewhat Disagree', 'Disagree', 'Strongly Disagree'))

attributes(mydf_pre_Affect$pre.Q15)$label <- "I will get frustrated going over statistics tests in class"
attributes(mydf_pre_Affect$pre.Q15)$labels <- setNames(c(1:7), c('Strongly Agree', 'Agree', 'Somewhat Agree', 'Neither Agree or Disagree', 
                                                                'Somewhat Disagree', 'Disagree', 'Strongly Disagree'))

attributes(mydf_pre_Affect$pre.Q18)$label <- "I will be under stress during statistics class"
attributes(mydf_pre_Affect$pre.Q18)$labels <- setNames(c(1:7), c('Strongly Agree', 'Agree', 'Somewhat Agree', 'Neither Agree or Disagree', 
                                                                 'Somewhat Disagree', 'Disagree', 'Strongly Disagree'))

attributes(mydf_pre_Affect$pre.Q19)$label <- "I will enjoy taking statistics course"
attributes(mydf_pre_Affect$pre.Q19)$labels <- setNames(c(1:7), c('Strongly Disagree', 'Disagree', 'Somewhat Disagree', 'Neither Agree or Disagree', 
                                                                'Somewhat Agree', 'Agree', 'Strongly Agree'))

attributes(mydf_pre_Affect$pre.Q28)$label <- "I am scared by statistics"
attributes(mydf_pre_Affect$pre.Q28)$labels <- setNames(c(1:7), c('Strongly Agree', 'Agree', 'Somewhat Agree', 'Neither Agree or Disagree', 
                                                                 'Somewhat Disagree', 'Disagree', 'Strongly Disagree'))

# Plotting centered likert data 
#plot_likert(
#  mydf_pre_Affect,
#  #grid.range = c(1.2, 1.4),
#  expand.grid = FALSE,
#  sort.frq = "pos.asc",
#  show.prc.sign = TRUE
# )

tab_itemscale(mydf_pre_Affect ,show.shapiro = TRUE, 
              show.kurtosis = TRUE)

# Frequency plots for items separately 
plot_frq(mydf_pre_Affect$pre.Q3, sort.frq = 'asc', show.mean = T, show.na = T, 
         coord.flip= TRUE, axis.labels = NULL)

plot_frq(mydf_pre_Affect$pre.Q4, sort.frq = 'asc', show.mean = T, show.na = T, 
         coord.flip= TRUE)

plot_frq(mydf_pre_Affect$pre.Q15, sort.frq = 'asc', show.mean = T, show.na = T, 
         coord.flip= TRUE)

plot_frq(mydf_pre_Affect$pre.Q18, sort.frq = 'asc', show.mean = T, show.na = T, 
         coord.flip= TRUE)

plot_frq(mydf_pre_Affect$pre.Q19, sort.frq = 'asc', show.mean = T, show.na = T, 
         coord.flip= TRUE)

plot_frq(mydf_pre_Affect$pre.Q28, sort.frq = 'asc', show.mean = T, show.na = T, 
         coord.flip= TRUE)

# Plot items (variables) of a scale as stacked proportional bars. 
#This function is useful when several items with identical scale/categoroies should be plotted to compare the distribution of answers.
plot_stackfrq(mydf_pre_Affect)

# plot Chi2-contingency-table
# sjp.chi2(mydf_pre_Affect)

# plot correlation matrix using circles
sjp.corr(mydf_pre_Affect, p.numeric = TRUE)

# MULTIPLE boxplot ???
plot_frq(mydf_pre_Affect$pre.Q3, type = "box")

# Calculations on likert scale, our 6 key components 
plot_frq(full_data$Affect_ave_pre, type = "hist", show.mean = TRUE)

# plot correlation matrix using circles
pre_key_comp <- full_data %>% select(c("Affect_ave_pre", "Cognitive_ave_pre", "Value_ave_pre", 
                                       "Difficulty_ave_pre", "Interest_ave_pre", "Effort_ave_pre" ))

tab_itemscale(pre_key_comp, show.shapiro = TRUE, show.kurtosis = TRUE)

library("car")
qqPlot(pre_key_comp$Affect_ave_pre)
# They seem normal 
shapiro.test(pre_key_comp$Affect_ave_pre)
shapiro.test(pre_key_comp$Cognitive_ave_pre)
shapiro.test(pre_key_comp$Value_ave_pre)
shapiro.test(pre_key_comp$Difficulty_ave_pre)
# They seem not normal 
shapiro.test(pre_key_comp$Interest_ave_pre)
shapiro.test(pre_key_comp$Effort_ave_pre)

  # Correlation plots with p-values as well
# COMPARE WITH YOUR RESULTS !!!
# Pearson Correlation
sjp.corr(pre_key_comp, p.numeric = TRUE, sort.corr = T)
# Spearman Correlation
sjp.corr(pre_key_comp, p.numeric = TRUE, sort.corr = T, corr.method = 'spearman')

post_key_comp <- full_data %>% select(c("Affect_ave_post", "Cognitive_ave_post", "Value_ave_post", 
                                       "Difficulty_ave_post", "Interest_ave_post", "Effort_ave_post" ))

tab_itemscale(post_key_comp, show.shapiro = TRUE, show.kurtosis = TRUE)

shapiro.test(post_key_comp$Affect_ave_post)
shapiro.test(post_key_comp$Cognitive_ave_post)
shapiro.test(post_key_comp$Value_ave_post)
shapiro.test(post_key_comp$Difficulty_ave_post)
shapiro.test(post_key_comp$Interest_ave_post)
shapiro.test(post_key_comp$Effort_ave_post)

# Correlation plots with p-values as well
# COMPARE WITH YOUR RESULTS !!!
# Pearson Correlation
sjp.corr(post_key_comp, p.numeric = TRUE, sort.corr = T)
# Spearman Correlation
sjp.corr(post_key_comp, p.numeric = TRUE, sort.corr = T, corr.method = 'spearman')

# About regression

# fit model
lm_pre_affect <- lm(Affect_ave_pre ~ Gender + Programme + Region, data = full_data)
summary(lm_pre_affect)

tab_model(lm_pre_affect, show.se = TRUE, show.std = TRUE, show.stat = TRUE)

plot_model(lm_pre_affect, vline.color = "red", sort.est = TRUE,
           show.values = TRUE, show.p = TRUE, value.offset = .3)


plot_model(lm_pre_affect, type = "pred", terms = c('Programme', 'Region', "Gender"))

# Use of some other packages for lm object 
library(performance)

# Some checking tools on the fitted model
check_heteroscedasticity(lm_pre_affect)
check_normality(lm_pre_affect)
check_collinearity(lm_pre_affect)
check_outliers(lm_pre_affect)

# General model performance
# model_performance(lm_pre_affect)

# Plotting model diagnostics
library(ggplot2); library(patchwork)
check_model(lm_pre_affect)


# Possible Use of rstatix -------------------------------------------------

library(rstatix)

full_data %>% freq_table(Gender, Programme, Region)

# Two-samples unpaired test
#:::::::::::::::::::::::::::::::::::::::::
full_data %>% t_test(Affect_ave_pre ~ Gender)

# pairwise comparisons
#::::::::::::::::::::::::::::::::::::::::
# As dose contains more than two levels ==>
# pairwise test is automatically performed.
full_data %>% t_test(Affect_ave_pre ~ Programme)
full_data %>% t_test(Affect_ave_pre ~ Region)

# One-way ANOVA test
#:::::::::::::::::::::::::::::::::::::::::
full_data %>% anova_test(Affect_ave_pre ~ Gender)
get_anova_table(full_data %>% anova_test(Affect_ave_pre ~ Gender))

res.anova <- Anova(lm(Affect_ave_pre ~ Gender*Programme, data = full_data))
anova_summary(res.anova)

res.anova <- Anova(lm(Affect_ave_pre ~ Gender*Region, data = full_data))
anova_summary(res.anova)

res.anova <- Anova(lm(Affect_ave_pre ~ Region*Programme, data = full_data))
anova_summary(res.anova)
get_anova_table(res.anova)


# Kruskal-wallis rank sum test
#:::::::::::::::::::::::::::::::::::::::::
full_data %>% kruskal_test(Affect_ave_pre ~ Gender)

# Grouped data
full_data %>% group_by(Gender) %>%
  kruskal_test(Affect_ave_pre ~ Programme)

## Assuming NONequal variances
oneway.test(Affect_ave_pre ~ Gender, data = full_data, var.equal = FALSE)
oneway.test(Affect_ave_pre ~ Programme, data = full_data, var.equal = FALSE)
oneway.test(Affect_ave_pre ~ Region, data = full_data, var.equal = FALSE)

# Simple test
full_data %>% games_howell_test(Affect_ave_pre ~ Gender)
full_data %>% games_howell_test(Affect_ave_pre ~ Programme)
full_data %>% games_howell_test(Affect_ave_pre ~ Region)

# Grouped data
full_data %>% group_by(Gender) %>%
  games_howell_test(Affect_ave_pre ~ Region)

full_data %>% group_by(Programme) %>%
  games_howell_test(Affect_ave_pre ~ Region)

