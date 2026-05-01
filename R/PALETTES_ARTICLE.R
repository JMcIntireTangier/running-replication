library(ggthemes)
library(ggplot2)

# ===== make some gray colors =====

graylevels <- seq(from = 0.1, to = 0.9, by = 0.025)
grays <- gray(graylevels,.625)
grays_1 <- grays[16]
grays_2 <- grays[c(1,28)]
grays_3 <- grays[c(1,16,28)]
grays_4 <- grays[c(1,9,17,25)]
grays_5 <- grays[c(1,7,13,19,25)]

# ===== DEFINE SOME GGPLOT THEMES =====
 
# theme_tufte <- function() {base_size = 11, base_family = "serif", ticks = TRUE}

theme_running <- function() {
     theme(base_size = 10,
           text = element_text(family = "sans", size = 10, face = "plain"),
           plot.title = element_text(size = 11, face = "bold"),
           plot.substitle = element_text(size = 10),
           plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
           plot.background = element_blank(),
           panel.background = element_blank(),
           strip.background.x = element_blank(),
           panel.border = element_rect(color = "black", fill = NA),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.ticks.x = element_blank(),
           axis.title = element_text(size = 10),
           legend.text = element_text(size = 10),
           legend.title = element_blank(),
           legend.position = "bottom")
}


# ===== DEFINE SOME GGPLOT THEMES =====================
# 

# theme_tufte(base_size = 11, base_family = "serif",
#             ticks = TRUE)
# 
# theme_tufte_11 <- theme_tufte() +
#      theme(text = element_text(family = "serif", size = 11, face = "plain"),
#      axis.text.x =
#      element_text(angle = 40,size = rel(0.9)),
#      plot.background = element_blank(),
#          panel.background = element_blank(),
#          panel.border = element_blank(),
#               strip.background.x = element_blank())
# 
# theme_cg <- function() {
#  theme(text = element_text(family = "serif", size = 11, face = "plain"),
#      plot.background = element_blank(),
#        panel.background = element_blank(),
#      strip.background.x = element_blank(),
#      panel.border = element_blank(),
#       plot.caption = element_text(hjust = 0),
#       plot.title.position = "panel",
#       plot.caption.position = "panel",
#       legend.text = element_blank(),
#       legend.position = "none",
#       legend.text.align = 0,
#       legend.direction = "horizontal")}
# 
# theme_cg_1 <- function() {
#  theme(text = element_text(family = "serif", size = 11, face = "plain"),
#      plot.background = element_blank(),
#        panel.background = element_blank(),
#      strip.background.x = element_blank(),
#      panel.border = element_blank(),
#       plot.caption = element_text(hjust = 0),
#       plot.title.position = "panel",
#       plot.caption.position = "panel",
#      legend.position = "bottom",
#       legend.text.align = 0,
#       legend.direction = "horizontal")}
# 
# theme_yield <- function(base_size = 11, base_family = "serif", ticks = T) 
#   {ret <- theme(plot.background = element_blank(),
#        panel.background = element_blank(),
#       plot.caption = element_text(hjust = 0),
#       plot.title.position = "panel",
#       plot.caption.position = "panel",
#       legend.position = "bottom",
#       legend.text.align = 0,
#       legend.direction = "horizontal")
# }

