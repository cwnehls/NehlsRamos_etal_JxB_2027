ss# ---- Script header ----


# CA1Pase Activity Relative by SEC Elution Fraction
# Date of last edit: 04/08/2026
# Description: To plot and compare the CA1Pase broad phosphatase activity to CRBP from the Improved Phosphate Detection Assay across different SEC elution fractions.
  # Additional code for further analysis is included and can be used by removing the additional #'s present.

#### Load packages 
library(ggplot2)
library(multcomp)
library(multcompView)
library(forcats)
library(dplyr)
library(viridisLite) # for accessible colours
library(FSA)         # For dunnTest()
library(rcompanion)  # For cldList()


# ---- data Loading and prep -----


#### Read the Excel file into a data frame
df <- read.csv("~/CA1Pase Paper/VuC_SEC_Fraction_Activity.csv")


# Validate for 1 way ANOVA

## test for normality

#mod <- aov(fraction ~ activity, data = df)

#plot(mod, which = 2)

#hist(mod$residuals)

#shapiro.test(mod$residuals)

## test for variance

#plot(mod, which = 3)

## outliers

#ggplot(df) +
#aes(group = fraction, x = fraction, y = activity) +
#geom_boxplot()

## One Way ANOVA

##Prepare data
#df$fraction <- as.factor(df$fraction)

res_aov <- aov(activity ~ fraction,
               data = df)

summary(res_aov)

HSD_res_aov <- HSD.test(res_aov, trt = c("fraction"), console = TRUE)

# ---- Prepare Data for Plotting ----

## Make summary dataframe

df_summary <- df %>%
  group_by(fraction) %>%
  summarise(
    mean = mean(activity),
    sd = sd(activity),
    n = n(),
    se = sd / sqrt(n)) 

# ---- Final Plot ----

ggplot(df, aes(x = fraction, y = activity, group = 1)) +
  geom_jitter(width = 0.05, size = 4, height = 0, alpha = 0.3, aes(x = fraction, y = activity)) +  # Show individual points
  geom_crossbar(data = df_summary,
                aes(x = fraction, y = mean, ymin = mean, ymax = mean),
                width = 0.1, color = "black", fatten = 3) + 
  geom_errorbar(data = df_summary,
                aes(x = fraction, y = mean, ymin = mean - se, ymax = mean + se), width = 0.2, size = 1) +
  ylim(0,3) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20), 
        axis.title = element_text(size = 24, face = "bold"), legend.position = "none", legend.text = element_text(size = 18),
        legend.title = element_text(size = 24), plot.title = element_text(size = 24, face = "bold")) +
  labs(title = "",
       x = "SEC Eluted Fraction", y = expression(bold(Activity~(µmol~min^{-1}~mg^{-1})))) + 
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

# ---- End ----
