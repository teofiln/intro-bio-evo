library(ggplot2)
library(cowplot)

utils_path <- local({
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    return(file.path(
      dirname(normalizePath(
        sub("^--file=", "", file_arg[1]),
        mustWork = FALSE
      )),
      "utils.R"
    ))
  }

  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(file.path(
      dirname(normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE)),
      "utils.R"
    ))
  }

  cwd <- normalizePath(getwd(), mustWork = FALSE)
  code_candidate <- file.path(cwd, "code", "utils.R")
  if (file.exists(code_candidate)) {
    return(code_candidate)
  }

  file.path(cwd, "utils.R")
})
source(utils_path)

out_dir <- ensure_asset_dir("04-assets")

# Create a data frame with allele frequencies
allele_data <- data.frame(
  Allele = c("A1", "A2", "A3", "A4"),
  Frequency = c(0.4, 0.3, 0.2, 0.1)
)
allele_freq_base <- allele_data

# Create the bar chart
p_bar_left <- ggplot(
  allele_data,
  aes(x = Allele, y = Frequency, fill = Allele)
) +
  geom_bar(stat = "identity") +
  labs(title = "Frecuencias") +
  scale_fill_brewer(palette = "Set2") +
  theme_diagram() +
  theme(legend.position = "none")

ggsave(
  file.path(out_dir, "allele_frequencies-left.png"),
  plot = p_bar_left,
  width = 4,
  height = 4
)

# Scatterplot for alleles A1-A4
set.seed(123)
alleles <- rep(allele_data$Allele, times = round(allele_data$Frequency * 50))
x_coords <- runif(length(alleles), min = 0, max = 10)
y_coords <- runif(length(alleles), min = 0, max = 10)
allele_data <- data.frame(Allele = alleles, x = x_coords, y = y_coords)
allele_data_base <- allele_data

p_scatter <- ggplot(allele_data, aes(x = x, y = y, color = Allele)) +
  geom_point(size = 5) +
  labs(title = "Alelos") +
  scale_color_brewer(palette = "Set2") +
  theme_diagram() +
  theme(legend.position = "none")

ggsave(
  file.path(out_dir, "allele_spatial-left.png"),
  plot = p_scatter,
  width = 4,
  height = 4
)

# Reshuffle the alleles in space and plot again
set.seed(456)
x_coords <- runif(length(alleles), min = 0, max = 10)
y_coords <- runif(length(alleles), min = 0, max = 10)
allele_data <- data.frame(Allele = alleles, x = x_coords, y = y_coords)

p_scatter_shuffled <- ggplot(allele_data, aes(x = x, y = y, color = Allele)) +
  geom_point(size = 5) +
  labs(title = "Alelos") +
  scale_color_brewer(palette = "Set2") +
  theme_diagram() +
  theme(legend.position = "none")

ggsave(
  file.path(out_dir, "allele_spatial-right.png"),
  plot = p_scatter_shuffled,
  width = 4,
  height = 4
)

library(patchwork)
hw_2panels <- p_scatter +
  p_bar_left +
  plot_layout(ncol = 2, nrow = 1, widths = c(1, 1))

ggsave(
  file.path(out_dir, "hw_2panels.png"),
  plot = hw_2panels,
  width = 5.4,
  height = 3
)

hw_4panels <- p_scatter +
  p_bar_left +
  plot_spacer() +
  p_scatter_shuffled +
  p_bar_left +
  plot_layout(ncol = 5, nrow = 1, widths = c(1, 1, 0.45, 1, 1))

hw_4panels_arrows <- ggdraw(hw_4panels) +
  draw_line(
    x = c(0.465, 0.535),
    y = c(0.5, 0.5),
    color = "grey35",
    size = 1,
    arrow = grid::arrow(length = grid::unit(0.18, "cm"), type = "closed")
  ) +
  draw_label(
    "Apareamiento aleatorio",
    x = 0.5,
    y = 0.62,
    size = 11,
    fontface = "bold",
    color = "grey20"
  )

ggsave(
  file.path(out_dir, "hw_4panels.png"),
  plot = hw_4panels,
  width = 12,
  height = 3
)

ggsave(
  file.path(out_dir, "hw_4panels-arrows.png"),
  plot = hw_4panels_arrows,
  width = 12,
  height = 3
)

# Not-HW variant: one allele frequency shifts after natural selection.
selected_freq <- data.frame(
  Allele = c("A1", "A2", "A3", "A4"),
  Frequency = c(0.6, 0.2, 0.15, 0.05)
)

p_bar_selected <- ggplot(
  selected_freq,
  aes(x = Allele, y = Frequency, fill = Allele)
) +
  geom_bar(stat = "identity") +
  labs(title = "Frecuencias") +
  scale_fill_brewer(palette = "Set2") +
  theme_diagram() +
  theme(legend.position = "none")

set.seed(789)
alleles_selected <- rep(
  selected_freq$Allele,
  times = round(selected_freq$Frequency * 50)
)
allele_data_selected <- data.frame(
  Allele = alleles_selected,
  x = runif(length(alleles_selected), min = 0, max = 10),
  y = runif(length(alleles_selected), min = 0, max = 10)
)

p_scatter_selected <- ggplot(
  allele_data_selected,
  aes(x = x, y = y, color = Allele)
) +
  geom_point(size = 5) +
  labs(title = "Alelos") +
  scale_color_brewer(palette = "Set2") +
  theme_diagram() +
  theme(legend.position = "none")

not_hw_4panels <- p_scatter +
  p_bar_left +
  plot_spacer() +
  p_scatter_selected +
  p_bar_selected +
  plot_layout(ncol = 5, nrow = 1, widths = c(1, 1, 0.45, 1, 1))

not_hw_4panels_arrows <- ggdraw(not_hw_4panels) +
  draw_line(
    x = c(0.465, 0.535),
    y = c(0.5, 0.5),
    color = "grey35",
    size = 1,
    arrow = grid::arrow(length = grid::unit(0.18, "cm"), type = "closed")
  ) +
  draw_label(
    "Seleccion natural",
    x = 0.5,
    y = 0.62,
    size = 11,
    fontface = "bold",
    color = "grey20"
  )

ggsave(
  file.path(out_dir, "not_hw_4panels-selection.png"),
  plot = not_hw_4panels,
  width = 12,
  height = 3
)

ggsave(
  file.path(out_dir, "not_hw_4panels-selection-arrows.png"),
  plot = not_hw_4panels_arrows,
  width = 12,
  height = 3
)

# Not-HW variant: one allele is lost by drift.
drift_freq <- data.frame(
  Allele = c("A1", "A2", "A3", "A4"),
  Frequency = c(0.5, 0.3, 0.2, 0.0)
)

p_bar_drift <- ggplot(
  drift_freq,
  aes(x = Allele, y = Frequency, fill = Allele)
) +
  geom_bar(stat = "identity") +
  labs(title = "Frecuencias") +
  scale_fill_brewer(palette = "Set2") +
  theme_diagram() +
  theme(legend.position = "none")

set.seed(246)
alleles_drift <- rep(
  drift_freq$Allele,
  times = round(drift_freq$Frequency * 50)
)
allele_data_drift <- data.frame(
  Allele = alleles_drift,
  x = runif(length(alleles_drift), min = 0, max = 10),
  y = runif(length(alleles_drift), min = 0, max = 10)
)

p_scatter_drift <- ggplot(
  allele_data_drift,
  aes(x = x, y = y, color = Allele)
) +
  geom_point(size = 5) +
  labs(title = "Alelos") +
  scale_color_brewer(palette = "Set2") +
  theme_diagram() +
  theme(legend.position = "none")

drift_4panels <- p_scatter +
  p_bar_left +
  plot_spacer() +
  p_scatter_drift +
  p_bar_drift +
  plot_layout(ncol = 5, nrow = 1, widths = c(1, 1, 0.45, 1, 1))

drift_4panels_arrows <- ggdraw(drift_4panels) +
  draw_line(
    x = c(0.465, 0.535),
    y = c(0.5, 0.5),
    color = "grey35",
    size = 1,
    arrow = grid::arrow(length = grid::unit(0.18, "cm"), type = "closed")
  ) +
  draw_label(
    "Deriva genetica",
    x = 0.5,
    y = 0.62,
    size = 11,
    fontface = "bold",
    color = "grey20"
  )

ggsave(
  file.path(out_dir, "not_hw_4panels-drift.png"),
  plot = drift_4panels,
  width = 12,
  height = 3
)

ggsave(
  file.path(out_dir, "not_hw_4panels-drift-arrows.png"),
  plot = drift_4panels_arrows,
  width = 12,
  height = 3
)

# Not-HW variant: mutation creates a new allele by changing one existing copy.
mutation_palette <- c(
  A1 = "#66C2A5",
  A2 = "#FC8D62",
  A3 = "#8DA0CB",
  A4 = "#E78AC3",
  A5 = "#E41A1C"
)

mutation_source <- allele_data_base
mutation_target <- allele_data_base
mutated_index <- which(mutation_target$Allele == "A2")[1]
mutation_target$Allele[mutated_index] <- "A5"
mutation_target$PointSize <- ifelse(mutation_target$Allele == "A5", 7, 5)

mutation_freq <- as.data.frame(table(factor(
  mutation_target$Allele,
  levels = names(mutation_palette)
)))
names(mutation_freq) <- c("Allele", "Count")
mutation_freq$Frequency <- mutation_freq$Count / sum(mutation_freq$Count)

p_bar_mutation_left <- ggplot(
  allele_freq_base,
  aes(x = Allele, y = Frequency, fill = Allele)
) +
  geom_bar(stat = "identity") +
  labs(title = "Frecuencias") +
  scale_fill_manual(
    values = mutation_palette[names(mutation_palette) != "A5"]
  ) +
  theme_diagram() +
  theme(legend.position = "none")

p_scatter_mutation_left <- ggplot(
  mutation_source,
  aes(x = x, y = y, color = Allele)
) +
  geom_point(size = 5) +
  labs(title = "Alelos") +
  scale_color_manual(
    values = mutation_palette[names(mutation_palette) != "A5"]
  ) +
  theme_diagram() +
  theme(legend.position = "none")

p_bar_mutation_right <- ggplot(
  mutation_freq,
  aes(x = Allele, y = Frequency, fill = Allele)
) +
  geom_bar(stat = "identity") +
  labs(title = "Frecuencias") +
  scale_fill_manual(values = mutation_palette) +
  theme_diagram() +
  theme(legend.position = "none")

p_scatter_mutation_right <- ggplot(
  mutation_target,
  aes(x = x, y = y, color = Allele)
) +
  geom_point(aes(size = PointSize)) +
  labs(title = "Alelos") +
  scale_color_manual(values = mutation_palette) +
  scale_size_identity() +
  theme_diagram() +
  theme(legend.position = "none")

mutation_4panels <- p_scatter_mutation_left +
  p_bar_mutation_left +
  plot_spacer() +
  p_scatter_mutation_right +
  p_bar_mutation_right +
  plot_layout(ncol = 5, nrow = 1, widths = c(1, 1, 0.45, 1, 1))

mutation_4panels_arrows <- ggdraw(mutation_4panels) +
  draw_line(
    x = c(0.465, 0.535),
    y = c(0.5, 0.5),
    color = "grey35",
    size = 1,
    arrow = grid::arrow(length = grid::unit(0.18, "cm"), type = "closed")
  ) +
  draw_label(
    "Mutacion",
    x = 0.5,
    y = 0.62,
    size = 11,
    fontface = "bold",
    color = "grey20"
  )

ggsave(
  file.path(out_dir, "not_hw_4panels-mutation.png"),
  plot = mutation_4panels,
  width = 12,
  height = 3
)

ggsave(
  file.path(out_dir, "not_hw_4panels-mutation-arrows.png"),
  plot = mutation_4panels_arrows,
  width = 12,
  height = 3
)

# Not-HW variant: migration adds two new copies of an outside allele.
migration_palette <- c(
  A1 = "#66C2A5",
  A2 = "#FC8D62",
  A3 = "#8DA0CB",
  A4 = "#E78AC3",
  A6 = "#FFD92F"
)

migration_source <- allele_data_base
migration_target <- allele_data_base
set.seed(321)
migrant_points <- data.frame(
  Allele = c("A6", "A6"),
  x = runif(2, min = 0.8, max = 9.2),
  y = runif(2, min = 0.8, max = 9.2),
  PointSize = c(7, 7)
)
migration_target$PointSize <- 5
migration_target <- rbind(migration_target, migrant_points)

migration_freq <- as.data.frame(table(factor(
  migration_target$Allele,
  levels = names(migration_palette)
)))
names(migration_freq) <- c("Allele", "Count")
migration_freq$Frequency <- migration_freq$Count / sum(migration_freq$Count)

p_bar_migration_left <- ggplot(
  allele_freq_base,
  aes(x = Allele, y = Frequency, fill = Allele)
) +
  geom_bar(stat = "identity") +
  labs(title = "Frecuencias") +
  scale_fill_manual(
    values = migration_palette[names(migration_palette) != "A6"]
  ) +
  theme_diagram() +
  theme(legend.position = "none")

p_scatter_migration_left <- ggplot(
  migration_source,
  aes(x = x, y = y, color = Allele)
) +
  geom_point(size = 5) +
  labs(title = "Alelos") +
  scale_color_manual(
    values = migration_palette[names(migration_palette) != "A6"]
  ) +
  theme_diagram() +
  theme(legend.position = "none")

p_bar_migration_right <- ggplot(
  migration_freq,
  aes(x = Allele, y = Frequency, fill = Allele)
) +
  geom_bar(stat = "identity") +
  labs(title = "Frecuencias") +
  scale_fill_manual(values = migration_palette) +
  theme_diagram() +
  theme(legend.position = "none")

p_scatter_migration_right <- ggplot(
  migration_target,
  aes(x = x, y = y, color = Allele)
) +
  geom_point(aes(size = PointSize)) +
  labs(title = "Alelos") +
  scale_color_manual(values = migration_palette) +
  scale_size_identity() +
  theme_diagram() +
  theme(legend.position = "none")

migration_4panels <- p_scatter_migration_left +
  p_bar_migration_left +
  plot_spacer() +
  p_scatter_migration_right +
  p_bar_migration_right +
  plot_layout(ncol = 5, nrow = 1, widths = c(1, 1, 0.45, 1, 1))

migration_4panels_arrows <- ggdraw(migration_4panels) +
  draw_line(
    x = c(0.465, 0.535),
    y = c(0.5, 0.5),
    color = "grey35",
    size = 1,
    arrow = grid::arrow(length = grid::unit(0.18, "cm"), type = "closed")
  ) +
  draw_label(
    "Migracion",
    x = 0.5,
    y = 0.62,
    size = 11,
    fontface = "bold",
    color = "grey20"
  )

ggsave(
  file.path(out_dir, "not_hw_4panels-migration.png"),
  plot = migration_4panels,
  width = 12,
  height = 3
)

ggsave(
  file.path(out_dir, "not_hw_4panels-migration-arrows.png"),
  plot = migration_4panels_arrows,
  width = 12,
  height = 3
)
