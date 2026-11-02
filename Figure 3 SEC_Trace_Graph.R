# ---- Script header ----


# SEC Trace Graphing
# Date of last edit: 04/08/2026
# Description: To plot and compare the SEC column trace 280nm readings for Protein size analysis.
  # Additional code for further analysis is included and can be used by removing the additional #'s present.

#### Load packages 
library(ggplot2)
library(dplyr)
library(readr)
library(viridis) # for accessible colours
library(pracma) # for peaks

# ---- data Loading and prep -----

## Read the Excel file into a data frame
df <- read_csv("~/CA1Pase Paper/SEC_Runs_CA1Pases_updated.csv")

## Select group

selected <- c("20260305_VuC_251125_5xConc_1mMFBP_SEC")


## filter for given group & range
df_selected <- df %>%
  filter(run == selected, between(volume_ml, 12, 17))
 
## Mutate names for readability
#df_selected <- df %>%
  #mutate(run = case_match(run,
                              #"20260512_SEC_VuC_NoGlycerol_control" ~ "VuC Control"))

# find peaks
peaks <- findpeaks(df_selected$Abs_280nm_mAU, minpeakheight = 18, minpeakdistance = 200) #NOTE: Modifying these values may be needed to mark the visible peaks correctly

## eaxtract peak position
peak_indices <- peaks[, 2]
peak_points <- df_selected[peak_indices, ]

# ---- Final Plot ----

ggplot(data = subset(df_selected, !is.na(run)), aes(x = volume_ml, y = Abs_280nm_mAU)) +
  geom_segment(data = peak_points, aes(x = volume_ml, xend = volume_ml, y = 0, yend = Abs_280nm_mAU),inherit.aes = FALSE, linetype = "dashed", colour = "darkgrey", size = 1.5) +
  geom_line(linetype = "solid", linewidth = 2, color = "#fde725") + #Change colour as needed for given protein
  labs(title = "", x = "Elution Volume (ml)", y = "Absorption 280mAU") + 
  ylim(-10,100) + #may need modification to fit the particular peaks
  xlim(12,17) + 
  scale_color_viridis_d(option = "viridis") +
  #scale_colour_manual(values = "#fde725") +
  #geom_vline(xintercept = c(12.85, 13.5, 14.15, 14.8, 15.45), linetype = "dashed", color = "grey", size = 1) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 20), 
        axis.title = element_text(size = 24, face = "bold"), legend.position = "none", legend.text = element_text(size = 18),
        legend.title = element_text(size = 24), plot.title = element_text(size = 24, face = "bold")) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

# ---- End ----
