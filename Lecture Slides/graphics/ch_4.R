setwd("~/Desktop/3818/Lecture Slides/graphics")

library(tidyverse)
library(kableExtra)
library(patchwork)


## Scatter Plot ----------------------------------------------------------------

nyc_stat <- read_csv("nyc_sat.csv") %>%
	filter(!is.na(`Percent Tested`)) %>% 
	mutate(`Percent Tested`= as.numeric(gsub("%", "", `Percent Tested`)))

(pos_corr <- ggplot(nyc_stat) + 
	geom_point(aes(x= `Average Score (SAT Math)`, y= `Average Score (SAT Reading)`)) +
	theme_gray(base_size= 16) +
	theme(
		plot.title = element_text(hjust = 0.5)
	) +
	labs(title= "NYC Math and Reading SAT Scores"))

ggsave("../Chapter 4/pos_corr.png", pos_corr, dpi= 300, width= 2400/300, height= 1600/300)

(no_corr <- ggplot(nyc_stat) + 
	geom_point(aes(x= Latitude, y= `Average Score (SAT Reading)`)) +
	theme_gray(base_size= 16) +
	theme(
		plot.title = element_text(hjust = 0.5)
	) +
	labs(title= "NYC Reading SAT Scores and School Latitude"))


ggsave("../Chapter 4/no_corr.png", no_corr, dpi= 300, width= 2400/300, height= 1600/300)
