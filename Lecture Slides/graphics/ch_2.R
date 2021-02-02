setwd("~/Desktop/3818/Lecture Slides/graphics")

library(tidyverse)
library(kableExtra)
library(patchwork)


## Density and Sample Density --------------------------------------------------

set.seed(1)
n_samp <- 1000

df <- tibble(
			x= seq(-10, 10, length=1000), 
			y=  dnorm(x, mean=0, sd=3),
			count= y * n_samp
		)

sample_df <- tibble(values= rnorm(n_samp, 0, 3))

(density <- ggplot() +
		geom_line(data= df, aes(x= x, y= y)) +
		theme_gray(base_size= 16) +
		theme(
			plot.title = element_text(hjust = 0.5)
		) +
		labs(title= "Population Distribution of Data"))

ggsave("../Chapter 2/normdistn.png", density, dpi= 300, width= 2400/300, height= 1600/300)

(sample_density <- ggplot(data= sample_df, aes(x= values)) +
	geom_histogram(aes(y= ..density..)) +
	geom_line(data= df, aes(x= x, y= y)) +
	theme_gray(base_size= 16) +
	theme(
		plot.title = element_text(hjust = 0.5)
	) +
	labs(title= "Population and Sample Distribution of Data") +
	geom_curve(
		data = data.frame(x = -3.7, y = 0.13, xend = -1.13, yend = 0.14),
		aes(x = x, y = y, xend = xend, yend = yend),
		inherit.aes= FALSE, size = 1L, angle = 49L, curvature = 0.315, 
		arrow = arrow(30L, unit(0.1, "inches"), "last", "closed")
	) + 
	geom_text(
		data = data.frame(x = -5.9, y = 0.13, label = "Sample Data"), 
		aes(x = x, y = y, label = label), 
		size = 5.6, fontface = 2, alpha = 1, inherit.aes = FALSE)) 

ggsave("../Chapter 2/normdistnsamplehist.png", sample_density, dpi= 300, width= 2400/300, height= 1600/300)


## Sample Box Plot -------------------------------------------------------------

happy_df <- read_csv("world_happiness.csv")


(box_plot <- ggplot(happy_df) + 
	geom_boxplot(aes(y= `Happiness Score`)) + 
	theme_gray(base_size= 16) +
	theme(
		plot.title = element_text(hjust = 0.5),
		axis.title.x=element_blank(),
		axis.text.x=element_blank(),
		axis.ticks.x=element_blank()
	) +
	labs(title= "Distribution of Country Happiness Scores") +
	geom_text(
		data = data.frame(
			x = c(-0.30, -0.30, -0.30, -0.07, -0.065), 
			y = c(6.4, 5.4, 4.4, 7.5, 3), 
			label = c("3rd Quartile", "Median", "1st Quartile", "Maximum", "Minimum")
		), 
		aes(x = x, y = y, label = label), 
		size = 5, alpha = 1, inherit.aes = FALSE
	))
	
ggsave("../Chapter 2/boxplot.png", box_plot, dpi= 300, width= 1800/300, height= 1400/300)
	
	

(box_plot_region <- ggplot(happy_df) + 
		geom_boxplot(aes(y= `Happiness Score`, fill= Region)) + 
		theme_gray(base_size= 16) +
		theme(
			plot.title = element_text(hjust = 0.5, size= 14),
			axis.title.x=element_blank(),
			axis.text.x=element_blank(),
			axis.ticks.x=element_blank()
		) +
		labs(title= "Distribution of Country Happiness Scores by Region"))
	
ggsave("../Chapter 2/boxplot_region.png", box_plot_region, dpi= 300, width= 2400/300, height= 1600/300)
	

boxplot(vgsales$NA_Sales)
