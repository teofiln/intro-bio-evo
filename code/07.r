library(ggplot2)
library(ggridges)
library(tibble)
library(rlang)
library(dplyr)

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

simulate_drift <- function(
  pop_size,
  n_generations = 250,
  n_replicates = 10,
  p0 = 0.5,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  n_alleles <- 2 * pop_size
  generations <- 0:n_generations
  out <- vector("list", n_replicates)

  for (rep_id in seq_len(n_replicates)) {
    p <- numeric(length(generations))
    p[1] <- p0

    for (g in 2:length(generations)) {
      n_A <- rbinom(1, size = n_alleles, prob = p[g - 1])
      p[g] <- n_A / n_alleles
    }

    out[[rep_id]] <- tibble(
      generation = generations,
      p_A = p,
      population = factor(rep_id)
    )
  }

  do.call(rbind, out)
}

make_drift_plot <- function(drift_df, pop_size) {
  ggplot(
    drift_df,
    aes(
      x = .data$generation,
      y = .data$p_A,
      group = .data$population,
      color = .data$population
    )
  ) +
    geom_hline(yintercept = 0.5, color = "gray70", linetype = "dashed") +
    geom_line(linewidth = 0.5, alpha = 0.9) +
    labs(
      title = paste0("Deriva genetica (N = ", pop_size, ")"),
      subtitle = "10 poblaciones independientes | frecuencia inicial de A = 0.5",
      x = "Generacion",
      y = "Frecuencia del alelo A"
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    scale_color_viridis_d() +
    theme_diagram()
}

# Build output path from project root.
out_dir <- ensure_asset_dir("07-assets")

sizes <- c(20, 200, 2000)

for (i in seq_along(sizes)) {
  n <- sizes[i]
  drift <- simulate_drift(
    pop_size = n,
    n_generations = 100,
    n_replicates = 10,
    p0 = 0.5,
    seed = 700 + i
  )

  p <- make_drift_plot(drift, n)

  ggsave(
    filename = file.path(out_dir, paste0("drift_N", n, "_10pops.png")),
    plot = p,
    width = 8,
    height = 4.8,
    dpi = 300
  )
}

# Buri-style simulation:
# 100 populations, each with 8 male + 8 female adults (16 total)
# all founders are heterozygous bw/bw75 (initial p_bw = 0.5)
# each generation produces 300 zygotes via random mating
# 16 adults are sampled at random to found the next generation

simulate_buri <- function(
  n_populations = 100,
  n_generations = 10,
  n_adults = 16,
  n_zygotes = 300,
  p0 = 0.5,
  seed = 707
) {
  set.seed(seed)

  out <- vector("list", n_populations)

  for (pop_id in seq_len(n_populations)) {
    p_current <- p0
    pop_rows <- vector("list", n_generations + 1)

    pop_rows[[1]] <- tibble(
      population = pop_id,
      generation = 0,
      bw_freq = p_current
    )

    for (gen in seq_len(n_generations)) {
      # Random mating: each zygote receives two alleles drawn with prob p_current.
      zygote_bw_counts <- rbinom(n_zygotes, size = 2, prob = p_current)

      # Drift bottleneck: sample 16 adults to seed the next generation.
      next_adults <- sample(zygote_bw_counts, size = n_adults, replace = FALSE)
      p_next <- sum(next_adults) / (2 * n_adults)

      pop_rows[[gen + 1]] <- tibble(
        population = pop_id,
        generation = gen,
        bw_freq = p_next
      )

      p_current <- p_next
    }

    out[[pop_id]] <- do.call(rbind, pop_rows)
  }

  do.call(rbind, out)
}

buri_df <- simulate_buri(
  n_populations = 100,
  n_generations = 10,
  n_adults = 8,
  n_zygotes = 100,
  p0 = 0.5,
  seed = 707
)

n_gen <- max(buri_df$generation)
n_pops <- length(unique(buri_df$population))

buri_df$generation_f <- factor(
  paste0("Gen ", buri_df$generation),
  levels = paste0("Gen ", 0:n_gen)
)

p_buri_hist <- ggplot(
  buri_df,
  aes(
    x = .data$bw_freq,
    y = .data$generation_f
  )
) +
  geom_density_ridges(
    stat = "binline",
    binwidth = 1 / 8,
    scale = 0.8,
    fill = "#2B8CBE",
    color = "white",
    linewidth = 0.4,
    alpha = 0.85
  ) +
  labs(
    title = paste0(
      "Experimento tipo Buri: deriva genetica en ",
      n_pops,
      " poblaciones"
    ),
    subtitle = "Distribucion de frecuencia de bw por generacion (N = 16 adultos, 32 cigotos/generacion)",
    x = "Frecuencia del alelo bw",
    y = "Generacion"
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    expand = expansion(mult = c(0, 0.01))
  ) +
  theme_diagram() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  coord_flip()

ggsave(
  filename = file.path(out_dir, "buri_bw_frequency_histograms_stacked.png"),
  plot = p_buri_hist,
  width = 12,
  height = 5,
  dpi = 300
)
