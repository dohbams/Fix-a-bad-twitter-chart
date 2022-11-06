# ----- Load libraries ---- #

library(tidyverse)
library(extrafont)

# ----- This section loads, prepares and cleans our dataframe ---- #
# Read the csv into an R data frame
bd =  read.csv("Fix a bad twitter chart.csv", header = T, stringsAsFactors = F)

# Change row names in  income.range column to something more concise
bd = bd %>% mutate(income.range = c("< 10k", "< or > 10k", "10k - 21k", "21k - 51k", "51k - 101k", "101k - 201k", "> 201k"))

# Define factor levels 
bd = bd %>% mutate(income.range = factor(income.range, levels = c("< 10k", "< or > 10k", "10k - 21k", "21k - 51k", "51k - 101k", "101k - 201k", "> 201k")))
# Pivot longer using the central, east, west, and state.indicator columns
bd2 = bd %>% pivot_longer(3:6, names_to = "senatorial.dist" , values_to = "percentage")

# ----- This section constructs the layers of our viz ---- #

ggplot(data = bd2) +
  # create a bar plot
  geom_col(width = 0.8, aes(x = income.range, y = percentage, fill = income.range )) +
  # use the facet_wrap function to create a bar plot for each senatorial district and set the label/header for each plot
  facet_wrap( ~ senatorial.dist, ncol = 2, scales = "free_x",
              labeller = as_labeller(c(central = "Central", east = "East", state.indicator = "State Indicator", west = "West"))) + 
  # add data labels to each bar
  geom_text(aes(x = X, y = percentage, label = paste0(percentage, " %"), family = "Tw Cen MT"), nudge_y = 5, size = 3.5) + 
  # add a custom color assignment to each bar
  scale_fill_manual(values = c("#DEE4E6","#DEE4E6","#DEE4E6", "#30638A","#DEE4E6","#DEE4E6","#DEE4E6")) +
  # add title, subtitle and caption
  labs(title = "TOTAL REGULAR INCOME OF HOUSEHOLD MEMBER IN LAGOS 2020",
       subtitle = "The chart below shows summarises the percent number of respondents in each household income range by senatorial district.
                  The data was obtained from the Lagos state household survey report, 2020.
       
       ",
       caption = "Data : Lagos Household Survey Report, 2020 | #Rstats #tidyverse | @doh_bams") +
  # use the minimal theme to make it pretty
  theme_minimal() + 
  # customise the plot
  theme(plot.title = element_text(size = 17, family = "Tw Cen MT", hjust = 0.5), # set plot title text size, position and font style
        plot.subtitle = element_text(size = 12, family = "Tw Cen MT", hjust = 0.5), # set plot subtitle text size, position and font style
        plot.caption = element_text(size = 10, family = "Tw Cen MT", hjust = 0.5, vjust = -10), # set plot caption text size, position and font style
        plot.margin = margin(1,1,1,1, "cm"), # margins around the plot (top, right, bottom, left)
        panel.spacing = unit(10, "mm"), # spacing between each facet
        plot.background = element_rect(fill = "#FBFBFB") # plot background coloor
        
        ) +
        
  theme(legend.position = "none", # remove plot legend
        strip.text = element_text(size = 14, family = "Tw Cen MT", hjust = 0.5), # set facet header/title text size, position and font style
        panel.grid = element_blank(), # remove plot grid
        axis.title = element_blank(), # remove axis titles
        axis.text.y = element_blank(), # remove y axis text
        axis.text.x = element_text(size = 10, family = "Tw Cen MT",hjust = 0.5, vjust = 0.5)) # set x axis text size, position and font style

# save the plot

ggsave("bdtwtrchrt.png", width = 12, height = 8, dpi = 300)

