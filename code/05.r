# Visualizing how discrete alleles create continuous variation
# Converting text explanation of polygenic inheritance into graphics

library(ggplot2)
library(tidyverse)

# Resolve paths from project root regardless of invocation directory.
get_script_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(
      sub("^--file=", "", file_arg[1]),
      mustWork = FALSE
    )))
  }

  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE)))
  }

  normalizePath(getwd(), mustWork = FALSE)
}

find_project_root <- function(start_dirs) {
  for (start_dir in start_dirs) {
    current <- normalizePath(start_dir, mustWork = FALSE)

    repeat {
      if (file.exists(file.path(current, "_quarto.yml"))) {
        return(current)
      }

      parent <- dirname(current)
      if (identical(parent, current)) {
        break
      }
      current <- parent
    }
  }

  NULL
}

# Theme consistent with course materials
theme_diagram <- function() {
  theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9)
    )
}

# Create output directory if needed
script_dir <- get_script_dir()
project_root <- find_project_root(c(script_dir, getwd()))
if (is.null(project_root)) {
  project_root <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)
}

output_dir <- file.path(project_root, "lectures", "05-assets")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ==============================================================================
# 1. ONE LOCUS (2 alleles: A and a)
# ==============================================================================

one_locus <- tibble(
  Genotype = c("AA", "Aa", "aa"),
  Height = c(2, 1, 0),
  Count = c(1, 1, 1) # one of each in example
)

p1 <- ggplot(one_locus, aes(x = Genotype, y = Height, fill = Genotype)) +
  geom_col(color = "black", width = 0.6) +
  labs(
    title = "1 locus (2 alelos)",
    x = "Genotipo",
    y = "Contribucion a la estatura (unidades)"
  ) +
  scale_y_continuous(limits = c(0, 2.5), breaks = 0:2) +
  scale_fill_manual(
    values = c("AA" = "#E74C3C", "Aa" = "#F39C12", "aa" = "#3498DB")
  ) +
  theme_diagram() +
  theme(legend.position = "none")

ggsave(
  file.path(output_dir, "01_one_locus.png"),
  p1,
  width = 5,
  height = 4,
  dpi = 150
)

# ==============================================================================
# 2. TWO LOCI (4 possible genotypes at each locus = 9 combinations)
# ==============================================================================

two_loci <- expand_grid(
  Locus1 = c("AA", "Aa", "aa"),
  Locus2 = c("BB", "Bb", "bb")
) %>%
  mutate(
    Locus1_contribution = case_when(
      Locus1 == "AA" ~ 2,
      Locus1 == "Aa" ~ 1,
      Locus1 == "aa" ~ 0
    ),
    Locus2_contribution = case_when(
      Locus2 == "BB" ~ 2,
      Locus2 == "Bb" ~ 1,
      Locus2 == "bb" ~ 0
    ),
    Total_height = Locus1_contribution + Locus2_contribution,
    Genotype = paste(Locus1, Locus2, sep = " / ")
  ) %>%
  arrange(Total_height)

p2_height <- ggplot(
  two_loci,
  aes(
    x = reorder(Genotype, Total_height),
    y = Total_height,
    fill = as.factor(Total_height)
  )
) +
  geom_col(color = "black", width = 0.7) +
  labs(
    title = "2 loci: contribucion a la estatura por genotipo",
    x = "Genotipo",
    y = "Contribucion a la estatura (unidades)"
  ) +
  scale_y_continuous(limits = c(0, 4.5), breaks = 0:4) +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  theme_diagram() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )

ggsave(
  file.path(output_dir, "02_two_loci_height.png"),
  p2_height,
  width = 7,
  height = 4,
  dpi = 150
)

set.seed(105)
two_loci_gxe <- two_loci %>%
  mutate(
    env_shift = runif(n(), min = -0.10, max = 0.10),
    env_multiplier = 1 + env_shift,
    Height_gxe = Total_height * env_multiplier,
    env_label = sprintf("%+.0f%%", env_shift * 100)
  )

p2_height_gxe <- ggplot(
  two_loci_gxe,
  aes(
    x = reorder(Genotype, Total_height),
    fill = as.factor(Total_height)
  )
) +
  geom_col(
    aes(y = Height_gxe),
    width = 0.92,
    alpha = 0.35,
    color = "black",
    linewidth = 0.2
  ) +
  geom_col(
    aes(y = Total_height),
    color = "black",
    width = 0.55
  ) +
  geom_text(
    aes(y = pmax(Total_height, Height_gxe) + 0.12, label = env_label),
    size = 2.8,
    color = "gray25"
  ) +
  labs(
    title = "2 loci: mismo genotipo, diferente entorno (GxE)",
    subtitle = "Multiplicador ambiental aleatorio por genotipo (entre -10% y +10%)",
    x = "Genotipo",
    y = "Contribucion a la estatura (unidades)"
  ) +
  scale_y_continuous(limits = c(0, 4.7), breaks = 0:4) +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  theme_diagram() +
  theme(
    plot.subtitle = element_text(size = 9, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )

ggsave(
  file.path(output_dir, "02_two_loci_height_gxe.png"),
  p2_height_gxe,
  width = 7,
  height = 4,
  dpi = 150
)

p2_histogram_data <- two_loci %>%
  count(Total_height, name = "Count")

p2_histogram <- ggplot(
  p2_histogram_data,
  aes(
    x = Total_height,
    y = Count,
    fill = as.factor(Total_height)
  )
) +
  geom_col(color = "black", width = 0.8) +
  labs(
    title = "2 loci: resumen en histograma",
    x = "Contribucion total a la estatura",
    y = "Frecuencia"
  ) +
  scale_y_continuous(limits = c(0, 4.5), breaks = 0:4) +
  scale_x_continuous(breaks = 0:4) +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  theme_diagram()

ggsave(
  file.path(output_dir, "02_two_loci_histogram.png"),
  p2_histogram,
  width = 5,
  height = 4,
  dpi = 150
)

# ==============================================================================
# 3-5. INCREASING LOCI: 5, 10, and 20 loci
# ==============================================================================

generate_polygenic_distribution <- function(n_loci, title) {
  # For each locus, an individual can have 0, 1, or 2 contributing alleles
  # Total possible range: 0 to 2*n_loci

  # Generate all possible phenotypes from population of individuals
  # Assuming Hardy-Weinberg with p=q=0.5 at each locus (simplification)

  # For visualization, simulate a sample population
  set.seed(42)
  n_individuals <- 1000

  phenotypes <- tibble(
    Individual = 1:n_individuals,
    Height = rowSums(
      replicate(n_loci, rbinom(n_individuals, size = 2, prob = 0.5))
    )
  )

  # Count phenotype frequencies
  phenotype_counts <- phenotypes %>%
    group_by(Height) %>%
    summarize(Count = n(), .groups = "drop")

  p <- ggplot(
    phenotype_counts,
    aes(x = Height, y = Count, fill = as.factor(Height))
  ) +
    geom_col(color = "black", width = 0.8) +
    labs(
      title = title,
      x = "Fenotipo (unidades de estatura)",
      y = "Numero de individuos"
    ) +
    scale_y_continuous(limits = c(0, max(phenotype_counts$Count) * 1.1)) +
    scale_x_continuous(breaks = seq(0, 2 * n_loci, by = 2)) +
    scale_fill_viridis_d(option = "turbo", guide = "none") +
    theme_diagram()

  return(p)
}

p3 <- generate_polygenic_distribution(5, "5 loci\n(0-10 fenotipos posibles)")
p4 <- generate_polygenic_distribution(10, "10 loci\n(0-20 fenotipos posibles)")
p5 <- generate_polygenic_distribution(20, "20 loci\n(0-40 fenotipos posibles)")

ggsave(
  file.path(output_dir, "03_five_loci.png"),
  p3,
  width = 5.5,
  height = 4,
  dpi = 150
)

ggsave(
  file.path(output_dir, "04_ten_loci.png"),
  p4,
  width = 5.5,
  height = 4,
  dpi = 150
)

ggsave(
  file.path(output_dir, "05_twenty_loci.png"),
  p5,
  width = 5.5,
  height = 4,
  dpi = 150
)

# ==============================================================================
# 6. ALL POSSIBLE GENOTYPES -> HEIGHT (exact mapping for 5, 10, and 20 loci)
# ==============================================================================

genotype_height_counts <- function(n_loci) {
  # Coefficients of (1 + x + x^2)^n_loci give the number of genotype
  # combinations producing each total height value.
  coeff <- c(1, 1, 1)
  if (n_loci > 1) {
    for (i in 2:n_loci) {
      coeff <- as.numeric(convolve(coeff, c(1, 1, 1), type = "open"))
    }
  }

  tibble(
    Height = 0:(2 * n_loci),
    Genotype_count = round(coeff)
  )
}

plot_all_genotypes_index <- function(n_loci) {
  counts <- genotype_height_counts(n_loci) %>%
    mutate(
      end = cumsum(Genotype_count),
      start = lag(end, default = 0) + 1,
      midpoint = (start + end) / 2
    )

  ggplot(counts) +
    geom_rect(
      aes(
        xmin = start,
        xmax = end,
        ymin = Height - 0.45,
        ymax = Height + 0.45,
        fill = as.factor(Height)
      ),
      color = "black",
      linewidth = 0.15
    ) +
    labs(
      title = paste0(
        n_loci,
        " loci: todos los genotipos posibles y su estatura"
      ),
      subtitle = paste0(
        "Cada bloque horizontal representa todos los genotipos que dan esa estatura",
        " (genotipos totales = ",
        scales::comma(3^n_loci),
        ")"
      ),
      x = "Indice de genotipo (todas las combinaciones distintas)",
      y = "Contribucion a la estatura (unidades)"
    ) +
    scale_x_continuous(labels = scales::comma) +
    scale_y_continuous(breaks = seq(0, 2 * n_loci, by = 2)) +
    scale_fill_viridis_d(option = "turbo", guide = "none") +
    theme_diagram() +
    theme(
      plot.subtitle = element_text(size = 9, hjust = 0.5),
      axis.text.x = element_text(size = 8)
    )
}

p6 <- plot_all_genotypes_index(5)
p7 <- plot_all_genotypes_index(10)
p8 <- plot_all_genotypes_index(20)

ggsave(
  file.path(output_dir, "07_five_loci_all_genotypes.png"),
  p6,
  width = 9,
  height = 4,
  dpi = 150
)

ggsave(
  file.path(output_dir, "08_ten_loci_all_genotypes.png"),
  p7,
  width = 9,
  height = 4,
  dpi = 150
)

ggsave(
  file.path(output_dir, "09_twenty_loci_all_genotypes.png"),
  p8,
  width = 9,
  height = 4,
  dpi = 150
)

# ==============================================================================
# Bonus: Side-by-side comparison showing the progression
# ==============================================================================

# Simplified version for display
comparison_data <- tibble(
  n_loci = c(1, 2, 5, 10, 20),
  max_phenotype = c(2, 4, 10, 20, 40)
) %>%
  expand_grid(phenotype = 0:40) %>%
  mutate(
    # Simple approximation: normal distribution centered
    count = dnorm(phenotype, mean = max_phenotype / 2, sd = max_phenotype / 6)
  ) %>%
  filter(phenotype <= max_phenotype)

p_comparison <- ggplot(
  comparison_data,
  aes(x = phenotype, y = count, fill = as.factor(n_loci))
) +
  facet_wrap(
    ~n_loci,
    scales = "free",
    nrow = 1,
    labeller = labeller(
      n_loci = c(
        "1" = "1 locus",
        "2" = "2 loci",
        "5" = "5 loci",
        "10" = "10 loci",
        "20" = "20 loci"
      )
    )
  ) +
  geom_area(alpha = 0.7, color = "black") +
  labs(
    title = "Como alelos discretos generan variacion continua",
    x = "Valor fenotipico",
    y = "Frecuencia"
  ) +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  theme_diagram() +
  theme(
    strip.text = element_text(size = 9, face = "bold"),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave(
  file.path(output_dir, "06_progression.png"),
  p_comparison,
  width = 12,
  height = 3,
  dpi = 150
)
