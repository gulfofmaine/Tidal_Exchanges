####
# Value and Volume issue
####
# Create test data.
data <- data.frame(
  category=c("A", "B", "C", "D", "E", "F", "G", "H", "I"),
  count=c(0.1, 0.2, 0.3, 0.4, 5, 10, 30, 60, 200)
)

# Compute percentages
data$fraction <- data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
library(ggtext)
species <- c("A" = "Merican Lobster", 
             "B" = "Blue Crab", 
             "C" = "Flounder", 
             "D" = "Bluefish", 
             "E" = "Seahorse", 
             "F" = "Sea Potatoe", 
             "G" = "Jellyfish", 
             "H" = "Krill", 
             "I" = "Sea Turtle")
data$label <- paste0("**", species[data$category], "**<br>$", data$count, "M"
                     #"<br>", round(rnorm(1, 0, 2)*data$count, 0), "M Lbs."
                     )

# Make the plot
xlim <- c(1, 4)
nudge_x_lab<- 0.5
ggplot(data) +
  geom_rect(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=label), color = "black", linewidth = 0.2) +
  #   geom_label(x = xlim[2]+nudge_x_lab, aes(y = labelPosition, label = label), size=6) +
  #ggrepel::geom_label_repel(x = xlim[2]+nudge_x_lab, aes(y = labelPosition, label = label), size = 6, force_pull = 1, nudge_x = 0, max.overlaps = 100) +
  scale_fill_brewer(palette=9) +
  coord_polar(theta="y") +
  xlim(c(1, 4)) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.text = element_markdown()) +
  labs(fill = NULL) +
  guides(
    label = NULL,
    fill = guide_legend(nrow = 1, label.position = "bottom", 
                        keyheight = 0.5, keywidth = 4
  ))

