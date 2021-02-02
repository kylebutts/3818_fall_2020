setwd("~/Desktop/3818/Lecture Slides/graphics")

library(tidyverse)
library(kableExtra)
library(patchwork)


## Normal Distribution ---------------------------------------------------------

set.seed(1)
n_samp <- 10000

df <- tibble(
	x= seq(-10, 10, length=1000), 
	y=  dnorm(x, mean=0, sd=3),
	count= y * n_samp
)

(density <- ggplot() +
		geom_line(data= df, aes(x= x, y= y)) +
		theme_gray(base_size= 16) +
		theme(
			plot.title = element_text(hjust = 0.5)
		) +
		labs(title= "Population Distribution of Data", x= "x"))

ggsave("../Chapter 3/normdistn.png", density, dpi= 300, width= 2400/300, height= 1600/300)

sample_df <- tibble(sd3= rnorm(n_samp, 0, 3), sd6= rnorm(n_samp, 0, 6), sd9= rnorm(n_samp, 0, 9))

(sample_density <- ggplot(data= sample_df, aes(x= sd3)) +
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

ggsave("../Chapter 3/normdistnsamplehist.png", sample_density, dpi= 300, width= 2400/300, height= 1600/300)


## Sample Probability Under Histogram ------------------------------------------


(sample_prob <- ggplot(data= sample_df, aes(x= sd3)) +
 	geom_histogram(aes(y= ..density..), binwidth= 1) +
	geom_vline(aes(xintercept= -2.5), color= "red") +
 	theme_gray(base_size= 16) +
 	theme(
 		plot.title = element_text(hjust = 0.5)
 	) +
 	labs(title= "Sample Probability x < -2.5", x= "x"))

ggsave("../Chapter 3/samp_prob.png", sample_prob, dpi= 300, width= 2400/300, height= 1600/300)

(pop_prob <- ggplot() + 
		geom_line(data= df, aes(x= x, y= y)) +
		geom_area(data= df %>% filter(x < -2.5), aes(x = x, y = y), fill = "steelblue") +
		geom_vline(aes(xintercept= -2.5), color= "red") +
		theme_gray(base_size= 16) +
		theme(
			plot.title = element_text(hjust = 0.5)
		) +
		labs(title= "Sample Probability x < -2.5", x= "x"))

ggsave("../Chapter 3/pop_prob.png", pop_prob, dpi= 300, width= 2400/300, height= 1600/300)

## Changes in Variance ---------------------------------------------------------

sample_df <- tibble(sd3= rnorm(n_samp, 0, 3), sd6= rnorm(n_samp, 40, 6), sd9= rnorm(n_samp, 120, 18))

(sample_vars <- sample_df %>% 
 	pivot_longer(everything(), names_prefix= "sd") %>%
 	arrange(name) %>%
 	ggplot(data= ., aes(x= value)) +
		geom_histogram(aes(y= ..density.., color= name), fill= "gray", alpha= 0.2, bins= 100) +
		theme_gray(base_size= 16) +
		theme(
			plot.title = element_text(hjust = 0.5, size= 14)
		) +
		labs(
			title= "Sample Distribution of Normal Curves with Different Variances",
			color= "Standard Deviation"
		))

ggsave("../Chapter 2/normdist_multiplevars.png", sample_vars, dpi= 300, width= 2400/300, height= 1600/300)


## Standard Normal -------------------------------------------------------------


sn_df <- tibble(
	x= seq(-3, 3, length=1000), 
	y=  dnorm(x, mean=0, sd=1)
)

(std_norm <- ggplot() +
	geom_line(data= sn_df, aes(x= x, y= y)) +
	geom_vline(xintercept = c(-1, 1)) +
	xlim(-3,3) + 
	theme_gray(base_size= 16) +
	theme(
		plot.title = element_text(hjust = 0.5)
	) +
	labs(title= "Standard Normal Distribution"))


ggsave("../Chapter 3/stdnormal.png", std_norm, dpi= 300, width= 2400/300, height= 1600/300)



bw_df <- tibble(
	x= seq(0, 200, length=1000), 
	y=  dnorm(x, mean= 113, sd= 22)
)

(norm_prob <- ggplot() +
	geom_line(data= bw_df, aes(x= x, y= y)) +
	geom_vline(xintercept = 88) +
	geom_area(data= bw_df %>% filter(x < 88), aes(x = x, y = y), fill = "steelblue") +
	theme_gray(base_size= 16) +
	theme(
		plot.title = element_text(hjust = 0.5)
	) +
	labs(title= "US Birthweights", x="Birthweight", y="Density"))

ggsave("../Chapter 3/normal_prob.png", norm_prob, dpi= 300, width= 2400/300, height= 1600/300)


## Between Z-Scores ------------------------------------------------------------

(prob_between <- ggplot() +
 	geom_line(data= sn_df, aes(x= x, y= y)) +
 	geom_vline(xintercept = c(-1.13, 0.3)) +
 	geom_area(data= sn_df %>% filter(-1.13 <= x & x <= 0.3), aes(x = x, y = y), fill = "steelblue") +
 	xlim(-3,3) + 
 	theme_gray(base_size= 16) +
 	theme(
 		plot.title = element_text(hjust = 0.5)
 	) +
 	labs(y= "Density", x= ""))


ggsave("../Chapter 3/areabetweenstdnorm.png", prob_between, dpi= 300, width= 2400/300, height= 1600/300)



