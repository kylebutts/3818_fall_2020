setwd("~/Desktop/3818/Lecture Slides/graphics")

library(tidyverse)
library(kableExtra)
library(patchwork)

majors <- tribble(
	~"Field of Study", ~"Percent of Students",
	"Arts and Humanities", 10.1,
	"Biological Sciences", 14.9,
	"Business", 13.4,
	"Education", 4.2,
	"Engineering", 13.1,
	"Health Professions", 11.3,
	"Math and Computer Science", 5.4,
	"Physical Sciences", 10.8,
	"Other majors", 13.9
)

kableExtra::kable(majors, "latex", booktabs = TRUE) %>% 
	kable_styling(latex_options =c("striped", "scale_down"))

majors <- majors %>%
	arrange(desc(`Field of Study`)) %>%
	mutate(lab.ypos = cumsum(`Percent of Students`) - 0.5*`Percent of Students`, 
		   lab= paste(`Percent of Students`, "%"))


## Pie Chart -------------------------------------------------------------------
pie_chart <- ggplot(majors, aes(x = "", y = `Percent of Students`, fill= `Field of Study`)) +
	geom_bar(width = 1, stat = "identity", color = "white") +
	coord_polar("y", start = 0) +
	geom_text(aes(y = lab.ypos, label = lab), color = "white") +
	theme_void(base_size = 16) + 
	theme(
		plot.title = element_text(hjust = 0.5)
	) +
	# set transparency
	theme(
		panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "transparent", colour = NA),
		plot.background = element_rect(fill = "transparent", colour = NA)
	) + 
	labs(title= "Pie Chart of Student Majors")

ggsave("../Chapter 1/pie.png", pie_chart, dpi= 300, width= 2400/300, height= 1600/300)

## Bar Chart -------------------------------------------------------------------


(bar_chart <- ggplot(majors, aes(x = `Field of Study`, y = `Percent of Students`, fill= `Field of Study`)) +
	geom_bar(width = 1, stat = "identity", color = "white") +
	geom_text(aes(y = `Percent of Students`/2, label = lab), color = "white") +
	scale_x_discrete(guide = guide_axis(n.dodge=2)) + 
	theme_gray(base_size= 16) +
	theme(
		plot.title = element_text(hjust = 0.5)
	) +
	# set transparency
	theme(
		panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill = "transparent",colour = NA),
		plot.background = element_rect(fill = "transparent",colour = NA)
	) + 
	labs(title= "Bar Chart of Student Majors"))


ggsave("../Chapter 1/bar.png", bar_chart, dpi= 300, width= 2400/300, height= 1600/300)


## Histogram -------------------------------------------------------------------

grad_rates <- read_csv("grad_rates.csv")

grad_rates %>% select(state, grad_rate) %>% slice(c(1:10, 51)) %>%
	kableExtra::kable(., "latex") 

(hist <- ggplot(grad_rates) + 
	geom_histogram(aes(x= grad_rate)) +
	theme_gray(base_size= 16) +
	theme(
		plot.title = element_text(hjust = 0.5)
	) +
	labs(
		title= "Histogram of State 2017-2018 Graduation Rate"
	))

ggsave("../Chapter 1/hist.png", hist, dpi= 300, width= 2400/300, height= 1600/300)


## Skewed Distributions --------------------------------------------------------

# snorm
library("fGarch")

skew <- data_frame(
	skew_right= rsnorm(n = 5000, mean = 0, sd = 18, xi = 130),
	symmetric= rnorm(n = 5000, mean = 0, sd = 18),
	skew_left= rsnorm(n = 5000, mean = 0, sd = 18, xi = -130)
)

(skew_left <- ggplot(skew) + 
		geom_histogram(aes(x= skew_left)) +
		theme_gray(base_size= 16) +
		theme(
			plot.title = element_text(hjust = 0.5)
		) +
		labs(
			title= "Left-skewed",
			x= "Value"
		))

(symmetric <- ggplot(skew) + 
		geom_histogram(aes(x= symmetric)) +
		theme_gray(base_size= 16) +
		theme(
			plot.title = element_text(hjust = 0.5)
		) +
		labs(
			title= "Symmetric",
			x= "Value"
		))

(skew_right <- ggplot(skew) + 
		geom_histogram(aes(x= skew_right)) +
		theme_gray(base_size= 16) +
		theme(
			plot.title = element_text(hjust = 0.5)
		) +
		labs(
			title= "Right-skewed",
			x= "Value"
		))

skew_plots <- skew_left + symmetric + skew_right

ggsave("../Chapter 1/skew_plots.png", skew_plots, dpi= 300, width= 4000/300, height= 1600/300)



## Time Series -----------------------------------------------------------------

college_costs <- tibble::tribble(
        ~type, ~year, ~`4-year`, ~`2-year`,
    "private", 1985L,    21042L,    14849L,
    "private", 1995L,    28284L,    18571L,
    "private", 2000L,    30973L,    21125L,
    "private", 2001L,    31882L,    22036L,
    "private", 2002L,    32411L,    24189L,
    "private", 2003L,    33427L,    26078L,
    "private", 2004L,    33991L,    26273L,
    "private", 2005L,    34082L,    26689L,
    "private", 2006L,    35151L,    24655L,
    "private", 2007L,    35426L,    25417L,
    "private", 2008L,    36102L,    26266L,
    "private", 2009L,    36459L,    28006L,
    "private", 2010L,    36494L,    25926L,
    "private", 2011L,    36720L,    25737L,
    "private", 2012L,    37614L,    25048L,
    "private", 2013L,    38649L,    25207L,
    "private", 2014L,    39825L,    25504L,
    "private", 2015L,    41168L,    25383L,
    "private", 2016L,    42400L,    25449L,
    "private", 2017L,    43139L,    25596L,
     "public", 1985L,     8798L,     6797L,
     "public", 1995L,    11264L,     6772L,
     "public", 2000L,    12263L,     6857L,
     "public", 2001L,    12805L,     7154L,
     "public", 2002L,    13336L,     7632L,
     "public", 2003L,    14233L,     8016L,
     "public", 2004L,    14789L,     8252L,
     "public", 2005L,    15098L,     8095L,
     "public", 2006L,    15557L,     8284L,
     "public", 2007L,    15739L,     8178L,
     "public", 2008L,    16428L,     8725L,
     "public", 2009L,    17214L,     8824L,
     "public", 2010L,    17866L,     9067L,
     "public", 2011L,    18303L,     9396L,
     "public", 2012L,    18742L,     9575L,
     "public", 2013L,    19113L,     9803L,
     "public", 2014L,    19533L,    10049L,
     "public", 2015L,    19998L,    10324L,
     "public", 2016L,    19928L,    10318L,
     "public", 2017L,    20050L,    10281L
    )


(cost_plot <- ggplot(college_costs) + 
	geom_line(aes(x= year, y= `4-year`, group= type, color= type)) +
	theme_gray(base_size= 16) +
	theme(
		plot.title = element_text(hjust = 0.5)
	) +
	labs(
		title= "Cost of 4-year Education Over Time",
		x= "Year",
		y= "Cost of 4 Year Education"
	) + scale_y_continuous(label= scales::dollar_format())
)

ggsave("../Chapter 1/timeseries.png", cost_plot, dpi= 300, width= 2400/300, height= 1600/300)
