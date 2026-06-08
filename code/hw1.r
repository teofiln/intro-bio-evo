# barchart with 4 alleles, A1 - A4 with different frequencies summing to 1.0

library(ggplot2)

# Create a data frame with allele frequencies
allele_data <- data.frame(
  Allele = c("A1", "A2", "A3", "A4"),
  Frequency = c(0.4, 0.3, 0.2, 0.1) # Example frequencies that sum to 1
)

# Create the bar chart
p_bar_left <- ggplot(
  allele_data,
  aes(x = Allele, y = Frequency, fill = Allele)
) +
  geom_bar(stat = "identity") +
  labs(title = "Allele Frequencies", x = "Allele", y = "Frequency") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none")

ggsave(
  "lectures/04-assets/allele_frequencies-left.png",
  plot = p_bar_left,
  width = 6,
  height = 4
)

# Scatterplot for alleles A1-A4
# one point per allele, assiming 50 individuals in population,
# randomly placed in a 2D space, colored by allele
# same data as above, but with random x and y coordinates

set.seed(123) # For reproducibility
# 50 individuals
alleles <- rep(allele_data$Allele, times = round(allele_data$Frequency * 50))
x_coords <- runif(length(alleles), min = 0, max = 10) # Random x coordinates
y_coords <- runif(length(alleles), min = 0, max = 10) # Random y coordinates
allele_data <- data.frame(Allele = alleles, x = x_coords, y = y_coords)

p_scatter <- ggplot(allele_data, aes(x = x, y = y, color = Allele)) +
  geom_point(size = 5) +
  labs(
    title = "Allele2 in 2D Space",
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "none") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

p_scatter

ggsave(
  "lectures/04-assets/allele_spatial-left.png",
  plot = p_scatter,
  width = 6,
  height = 4
)

# Reshufle the alleles in space and plot again
set.seed(456) # For reproducibility
x_coords <- runif(length(alleles), min = 0, max = 10) # New random x coordinates
y_coords <- runif(length(alleles), min = 0, max = 10) # New random y coordinates
allele_data <- data.frame(Allele = alleles, x = x_coords, y = y_coords)

p_scatter_shuffled <- ggplot(allele_data, aes(x = x, y = y, color = Allele)) +
  geom_point(size = 5) +
  labs(
    title = "Alleles in 2D Space"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "none") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

p_scatter_shuffled

ggsave(
  "lectures/04-assets/allele_spatial-right.png",
  plot = p_scatter_shuffled,
  width = 6,
  height = 4
)


# 4 panels
library(patchwork)
hw_4panels <- p_bar_left +
  p_scatter +
  p_scatter_shuffled +
  p_bar_left +
  plot_layout(ncol = 4, nrow = 1)

ggsave(
  "lectures/04-assets/hw_4panels.png",
  plot = hw_4panels,
  width = 12,
  height = 4
)
