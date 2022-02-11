###########################################################################
# 
#     BAYESIAN ANALYSIS
# 
#     Session 0: Tools for bayesian analysis I: ggplot2
#
#     Autor: Jesus Corral Lopez
#     mail: jesus.corral@upc.edu
# 
#     Date: February 10, 2022
###########################################################################

# 1.1 Introduction --------------------------------------------------------

library(ggplot2)

mpg

ggplot(data = mpg) + 
  geom_point(aes(x = displ, y = hwy))



# 1.2 Visualizing variables (aes) -----------------------------------------

ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy, size = cyl, color = class))


#' Example:
#' 
#' In the previous graphic, make all the dots blue (color = "blue")

ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy, size = cyl), color = "blue")


# 1.2.1 set vs map --------------------------------------------------------

ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy, color = "blue"))


#' Example:
#' 
#' In the above chart, make all the points with displ <5 draw 
#' in one color and those with displ> = 5 in another.

ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy, size = cyl, color = displ<5))


# 1.3 facet ---------------------------------------------------------------

ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) + 
  facet_wrap(~ class)



ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)


#' Example:
#' 
#' Check what happens when `.` is used instead of one of the 
#' variables in the formula inside `facet_grid()`.

ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ .)

ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) + 
  facet_grid(. ~ cyl)

# 1.4 Visualizing cases: geom ---------------------------------------------

ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy))


#' Example:
#' 
#' Make 3 different figures from the previous figure using the 
#' variable drv with the aesthetics `color`, `linetype` and `group`.

ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy, color = drv))

ggplot(mpg) + 
  geom_smooth(aes(x = displ, y = hwy, group = drv))

ggplot(mpg) + 
  geom_smooth(aes(x = displ, y = hwy, linetype = drv, color = drv))


# 1.4.1 Multiple layers ---------------------------------------------------

ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) +
  geom_smooth(aes(x = displ, y = hwy))



# 1.4.2 global vs local ---------------------------------------------------

ggplot(mpg, aes(x = displ, y = hwy)) +
   geom_point() +
   geom_smooth()


ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(aes(color = class)) + 
  geom_smooth()

# Using 2 data.frames
mpg_subcompact <- mpg[mpg$class == "subcompact", ]

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(aes(color = class)) + 
  geom_smooth(data = mpg_subcompact, se = F)

#' Example:
#' 
#' Recreate the code in R needed to generate the following figures, from:

p <- ggplot(mpg, aes(x = displ, y = hwy))

p + 
  geom_point() + 
  geom_smooth()

p + 
  geom_point() + 
  geom_smooth(aes(group = drv), se = F)

p + 
  geom_point(aes(color = drv)) + 
  geom_smooth(aes(color = drv), se = F)

p + 
  geom_point(aes(color = drv)) + 
  geom_smooth(se = F)
  

# 1.5 Position ------------------------------------------------------------

ggplot(diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))

ggplot(diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))

ggplot(diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "stack")

ggplot(diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

ggplot(diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")


#' Ejercicio:
#' 
#' Modifica el gráfico anterior utilizando diferentes valores 
#' en el parámetro position (“stack”, “dodge”, “identity”, “fill”).



# 1.6 Formal aspects of ggplot2 -------------------------------------------


# 1.6.1 Labels: titles, axis, legend --------------------------------------

p <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth()

p + 
  labs(title = "Fuel efficiency vs. Engine size",
       x = "Engine displacement (L)", 
       y = "Highway fuel efficiency (mpg)",
       color = "Type of Car",
       caption = "Data from fueleconomy.gov")



# 1.6.2 Scales ------------------------------------------------------------

(p <- ggplot(mpg, aes(displ, hwy)) + 
  geom_point(aes(color = class)))

p +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_color_discrete()

p +
  scale_color_discrete(labels = c("A" , "B", "C", "D", "E", "F", "G"))

p +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(breaks = seq(15, 40, by = 5))

p +
  scale_y_log10(breaks = seq(15, 40, by = 5))



# 1.6.3 Zoom --------------------------------------------------------------

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() + 
  coord_cartesian(xlim = c(5, 7), ylim = c(10, 30)) 



# 1.6.4 Themes ------------------------------------------------------------

p +  theme_bw()
p +  theme_grey()
p +  theme_light()
p +  theme_dark()



# 1.6.5 Additional themes -------------------------------------------------

library(ggthemes)

p <- ggplot(mpg, aes(x = displ, y = hwy, colour = factor(cyl))) +
  geom_point() +
  labs(title = "mpg")

# Economist theme
p + theme_economist()
# Economist theme + paleta de colores
p + theme_economist() + scale_colour_economist() 


# 1.6.6 Define your own themes --------------------------------------------

theme_jesus <- function () { 
  theme_bw(base_size=12, base_family="Courier") %+replace% 
    theme(
      panel.background  = element_blank(),
      plot.background = element_rect(fill="gray96", colour=NA), 
      legend.background = element_rect(fill="transparent", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA)
    )
}

p + theme_bw()
p + theme_jesus()




# > Exercise:
# >  
# > Experiment with labels, themes and scales in order to create a figure like this, 
# > based on the `diamonds` data (x: carat, y: price, color: cut)


# 1.7 Save plots ----------------------------------------------------------

#ggsave("my-plot.pdf", width = 6, height = 6)
#ggsave("my-plot.png", width = 6, height = 6)

#png("my-plot_4.png", width = 800, height = 600)
#  print(p)
#dev.off()




# 1.8 Plotly --------------------------------------------------------------

library(plotly)

(p <- ggplotly(p))



# 1.9 Display several plots at once ---------------------------------------

library(gridExtra)

p1 <- ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point()

p2 <- ggplot(diamonds, aes(x = carat, y = price)) +
  geom_smooth(aes(color = cut), se = FALSE)

grid.arrange(p1, p2, nrow = 1)
