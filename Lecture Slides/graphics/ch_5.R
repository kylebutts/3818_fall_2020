setwd("~/Desktop/3818/Lecture Slides/graphics")

library(tidyverse)
library(kableExtra)
library(patchwork)

## Scatter Plot ----------------------------------------------------------------

nyc_stat <- read_csv("nyc_sat.csv") %>%
	filter(
		!is.na(`Percent Tested`),
		!is.na(`Average Score (SAT Math)`)
	) %>% 
	mutate(`Percent Tested`= as.numeric(gsub("%", "", `Percent Tested`))) 

fit <- lm(`Average Score (SAT Reading)` ~ `Average Score (SAT Math)`, data= nyc_stat)

nyc_stat$residuals <- residuals(fit)
nyc_stat$predicted <- predict(fit)

(reg_line <- ggplot(nyc_stat, aes(x= `Average Score (SAT Math)`, y= `Average Score (SAT Reading)`)) + 
		geom_point() +
		geom_smooth(method='lm', formula= y~x, se= FALSE, color= "steelblue") +
		theme_gray(base_size= 16) +
		theme(
			plot.title = element_text(hjust = 0.5)
		) +
		labs(title= "NYC Math and Reading SAT Scores"))

ggsave("../Chapter 5/reg_line.png", reg_line, dpi= 300, width= 2400/300, height= 1600/300)

(reg_resids <- ggplot(nyc_stat, aes(x= `Average Score (SAT Math)`, y= `Average Score (SAT Reading)`)) + 
		geom_point() +
		geom_smooth(method='lm', formula= y~x, se= FALSE, color= "steelblue") +
		geom_segment(aes(xend = `Average Score (SAT Math)`, yend = .data$predicted), alpha= 0.2) +
		theme_gray(base_size= 16) +
		theme(
			plot.title = element_text(hjust = 0.5)
		) +
		labs(title= "NYC Math and Reading SAT Scores"))

ggsave("../Chapter 5/reg_line_resids.png", reg_resids, dpi= 300, width= 2400/300, height= 1600/300)


## Outlier

nyc_stat_w_outlier <- nyc_stat %>%
	mutate(important= FALSE) %>% 
	add_row(`Average Score (SAT Math)`= 700, `Average Score (SAT Reading)`= 340, important= TRUE)

(outlier_plot <- ggplot(nyc_stat_w_outlier, aes(x= `Average Score (SAT Math)`, y= `Average Score (SAT Reading)`)) + 
		geom_point(aes(color= important)) +
		geom_line(data= nyc_stat, aes(x= `Average Score (SAT Math)`, y= predicted)) +
		geom_smooth(method='lm', formula= y~x, se= FALSE, color= "steelblue") +
		geom_segment(aes(xend = `Average Score (SAT Math)`, yend = .data$predicted), alpha= 0.2) +
		theme_gray(base_size= 16) +
		theme(
			plot.title = element_text(hjust = 0.5)
		) +
		guides(color= FALSE) +
		scale_color_manual(values= c("grey50", "red")) +
		labs(title= "NYC Math and Reading SAT Scores") + 
		geom_curve(data = data.frame(x = 694.5, y = 401.9, xend = 695.3, yend = 346.6),
				   mapping = aes(x = x, y = y, xend = xend, yend = yend),
				   arrow = arrow(30L, unit(0.1, "inches"), "last", "closed"),
				   alpha = 1, inherit.aes = FALSE) +
		geom_text(data = data.frame(x = 694.5, y = 420, label = "Outlier"),
				  mapping = aes(x = x, y = y, label = label),
				  size = 5.6, fontface = 2, alpha = 1, inherit.aes = FALSE) +
		geom_curve(data = data.frame(x = 730.4, y = 578.0, xend = 738.9, yend = 652.3),
				   mapping = aes(x = x, y = y, xend = xend, yend = yend),
				   arrow = arrow(30L, unit(0.1, "inches"), "last", "closed"),
				   alpha = 1, inherit.aes = FALSE) + 
		geom_text(data = data.frame(x = 722, y = 563, label = "Line with Outlier"),
				  mapping = aes(x = x, y = y, label = label),
				  size = 5.6, fontface = 2, alpha = 1, inherit.aes = FALSE)
	)

ggsave("../Chapter 5/influentialobs.png", outlier_plot, dpi= 300, width= 2400/300, height= 1600/300)

