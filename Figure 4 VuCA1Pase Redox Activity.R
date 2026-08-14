# ---- Script header ----


# CA1Pase Activity Across Redox States in 10mM Glutathione
# Date of last edit: 14/08/2026
# Description: To plot and compare the CA1Pase broad phosphatase activity to CRBP from the Improved Phosphate Detection Assay at different proportions of GSH to GSSG in 10mM Glutathione. 
  # This plots the activity of wild type VuCA1Pase as well as the three cysteine to alanine mutant variants. Additional code for further analysis is included and can be used by removing the additional #'s present.

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
df <- read.csv("~/CA1Pase Paper/20260514_VuC_Redox_IPDA(Data).csv")

## Select group

selected <- c("C306A_C311A") #select the variant you wish to graph

## filter for given group & range
df_selected <- df %>%
  filter(variant == selected)

# Validate for 2 way ANOVA

## test for normality

#mod <- aov(activity_to_0percentGSH ~ treatment_percentGSH, data = df)

#plot(mod, which = 2)

#hist(mod$residuals)

#shapiro.test(mod$residuals)

## test for variance

#plot(mod, which = 3)

## outliers

#ggplot(df) +
#aes(group = treatment_percentGSH, x = treatment_percentGSH, y = activity_to_0percentGSH) +
#geom_boxplot()

#ggplot(df) +
#aes(group = Replicate, x = Replicate, y = activity_to_0percentGSH) +
#geom_boxplot()

# One Way ANOVA

#res_aov <- aov(activity_to_0percentGSH ~ treatment_percentGSH,
               #data = df)

#summary(res_aov)

# Post-hoc

## Perform Dunn test on Kruskal-wallis results to compare significances between Tribe
#dunn_results <- dunnTest(activity_to_0percentGSH ~ treatment_percentGSH, data = df, method = "bonferroni")

#print(dunn_results)

## Extract the p-values
#dunn_df <- dunn_results$res

## Create the CLD to represent Tribe differences
#cld <- cldList(P.adj ~ Comparison,
               #data = dunn_df,
               #threshold = 0.05)

# ---- Prepare Data for Plotting ----

# Make summary dataframe

df_summary <- df_selected %>%
  group_by(treatment_percentGSH) %>%
  summarise(
    mean = mean(activity),
    sd = sd(activity),
    n = n(),
    se = sd / sqrt(n)) 

# ---- Final Plot ----

ggplot(df_selected, aes(x = treatment_percentGSH, y = activity, group = 1)) +
  stat_summary(
    fun = mean, geom = "line", size = 2, colour = "#440154", linetype = "dotdash") + #NOTE: change the linetype to the relevant variant
  geom_jitter(width = 0.15, size = 4, height = 0, alpha = 0.6, aes(x = treatment_percentGSH, y = activity), colour = "#440154") +  # Show individual points
  geom_crossbar(data = df_summary,
                aes(x = treatment_percentGSH, y = mean, ymin = mean, ymax = mean),
                width = 2, fatten = 2) + 
  geom_errorbar(data = df_summary,
                aes(x = treatment_percentGSH, y = mean, ymin = mean - se, ymax = mean + se), width = 2, size = 1, colour = "#440154") +
  ylim(0,2.5) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18), 
        axis.title = element_text(size = 24, face = "bold"), legend.position = "bottom", legend.text = element_text(size = 14),
        legend.title = element_text(size = 18, face = "bold"), plot.title = element_text(size = 24, face = "bold")) +
  labs(title = "",
       x = "% Reduced (10mM Glutathione)", y = expression(bold("Activity ("*mu*"mol*mg"^-1*"*min"^-1*")"))) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

# ---- End ----
