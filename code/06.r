library(ggplot2)
library(tibble)
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
theme_diagram <- get("theme_diagram", envir = environment())

out_dir <- ensure_asset_dir("06-assets")

count_genotypes <- function(pop) {
  lvls <- c("AA", "AS", "SS")
  tab <- table(factor(pop, levels = lvls))
  counts <- as.integer(tab)
  names(counts) <- lvls
  counts
}

count_alleles <- function(pop) {
  genotype_counts <- count_genotypes(pop)
  c(
    A = unname(2 * genotype_counts[["AA"]] + genotype_counts[["AS"]]),
    S = unname(2 * genotype_counts[["SS"]] + genotype_counts[["AS"]])
  )
}

format_allele_summary <- function(pop) {
  allele_counts <- count_alleles(pop)
  total_alleles <- sum(allele_counts)
  allele_freqs <- if (total_alleles > 0) {
    allele_counts / total_alleles
  } else {
    c(A = 0, S = 0)
  }
  sprintf(
    "A = %d (%.3f) | S = %d (%.3f)",
    unname(allele_counts[["A"]]),
    unname(allele_freqs[["A"]]),
    unname(allele_counts[["S"]]),
    unname(allele_freqs[["S"]])
  )
}

relative_fitness <- c(AA = 0.7, AS = 1.0, SS = 0.4)
diversifying_fitness <- c(AA = 1.0, AS = 0.6, SS = 1.0)
directional_fitness <- c(AA = 1.0, AS = 0.8, SS = 0.5)

save_fitness_chart <- function(
  fitness_map,
  file_name,
  title,
  subtitle
) {
  fitness_data <- tibble(
    genotype = factor(c("AA", "AS", "SS"), levels = c("AA", "AS", "SS")),
    fitness = unname(fitness_map[c("AA", "AS", "SS")])
  )
  fitness_data$is_fittest <- fitness_data$fitness == max(fitness_data$fitness)

  p_fitness <- ggplot(
    data = fitness_data,
    aes(x = genotype, y = fitness, fill = is_fittest)
  ) +
    geom_col(
      alpha = 0.75,
      width = 0.6
    ) +
    scale_fill_manual(
      values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"),
      guide = "none"
    ) +
    geom_label(aes(label = fitness)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Genotipo",
      y = "Aptitud relativa (fitness)"
    ) +
    ylim(0, 1.2) +
    theme_diagram()

  ggsave(
    filename = file.path(out_dir, file_name),
    plot = p_fitness,
    width = 8,
    height = 5,
    dpi = 300
  )

  invisible(p_fitness)
}

hw_genotype_probs <- function(q_S) {
  p_A <- 1 - q_S
  c(AA = p_A^2, AS = 2 * p_A * q_S, SS = q_S^2)
}

build_population_df <- function(
  pop,
  stage_label,
  alive = rep(TRUE, length(pop))
) {
  n <- length(pop)
  n_cols <- ceiling(sqrt(n))
  n_rows <- ceiling(n / n_cols)
  grid <- expand.grid(row = 1:n_rows, col = 1:n_cols)
  grid <- grid[seq_len(n), , drop = FALSE]

  x_center <- grid$col
  y_center <- (n_rows + 1) - grid$row

  cells_df <- tibble(
    stage = stage_label,
    id = seq_len(n),
    genotype = pop,
    alive = alive,
    display_group = ifelse(alive, pop, "No sobrevive"),
    x = x_center,
    y = y_center,
    xmin = x_center - 0.45,
    xmax = x_center + 0.45,
    ymin = y_center - 0.45,
    ymax = y_center + 0.45
  )

  list(cells = cells_df, n_cols = n_cols, n_rows = n_rows)
}

plot_population_stage <- function(pop, alive, stage_label, file_name = NULL) {
  built <- build_population_df(pop, stage_label, alive)
  geno_counts <- count_genotypes(pop[alive])
  label_size <- if (length(pop) <= 36) {
    3
  } else if (length(pop) <= 100) {
    2.1
  } else {
    1.6
  }
  subtitle <- sprintf(
    "N = %d | AA = %d | AS = %d | SS = %d\nAlleles: %s",
    sum(alive),
    geno_counts[1],
    geno_counts[2],
    geno_counts[3],
    format_allele_summary(pop[alive])
  )

  p <- ggplot() +
    geom_rect(
      data = built$cells,
      aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin,
        ymax = .data$ymax,
        fill = .data$display_group
      ),
      color = "gray45",
      linewidth = 0.35,
      alpha = 0.95
    ) +
    geom_text(
      data = built$cells,
      aes(x = .data$x, y = .data$y, label = .data$genotype),
      size = label_size,
      color = "black"
    ) +
    scale_fill_manual(
      values = c(
        "AA" = "#4C78A8",
        "AS" = "#54A24B",
        "SS" = "#E45756",
        "No sobrevive" = "#D3D3D3"
      ),
      breaks = c("AA", "AS", "SS", "No sobrevive"),
      name = "Estado"
    ) +
    coord_equal(
      xlim = c(0.5, built$n_cols + 0.5),
      ylim = c(0.5, built$n_rows + 0.5),
      expand = FALSE
    ) +
    labs(
      title = stage_label,
      subtitle = subtitle,
      x = NULL,
      y = NULL
    ) +
    theme_diagram() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom"
    )

  if (!is.null(file_name)) {
    ggsave(
      filename = file.path(out_dir, file_name),
      plot = p,
      width = 7,
      height = 6,
      dpi = 220
    )
  }

  invisible(p)
}

apply_selection <- function(
  pop,
  alive,
  fitness_map = relative_fitness
) {
  survive_prob <- unname(fitness_map[pop])
  keep_draw <- runif(length(pop)) < survive_prob
  alive & keep_draw
}

repopulate <- function(pop, n_target = 25) {
  if (length(pop) == 0) {
    stop("No survivors left to repopulate")
  }
  sample(pop, size = n_target, replace = TRUE)
}

next_gen_hw <- function(pop_survivors, n_target = 25) {
  if (length(pop_survivors) == 0) {
    stop("No survivors left to generate HW offspring")
  }

  # Allele frequencies among survivors (post-selection parents)
  n_A <- sum(substring(pop_survivors, 1, 1) == "A") +
    sum(substring(pop_survivors, 2, 2) == "A")
  n_alleles <- 2 * length(pop_survivors)
  p_A <- n_A / n_alleles
  q_S <- 1 - p_A

  # HW expectations for offspring (rounded to integer counts)
  expected <- c(AA = p_A^2, AS = 2 * p_A * q_S, SS = q_S^2) * n_target
  counts <- floor(expected)
  remainder <- n_target - sum(counts)
  if (remainder > 0) {
    frac <- expected - floor(expected)
    idx <- order(frac, decreasing = TRUE)[seq_len(remainder)]
    counts[idx] <- counts[idx] + 1
  }

  offspring <- c(
    rep("AA", counts["AA"]),
    rep("AS", counts["AS"]),
    rep("SS", counts["SS"])
  )
  sample(offspring, size = length(offspring), replace = FALSE)
}

build_tally_df <- function(stage_order, pops) {
  out <- vector("list", length(stage_order))
  for (i in seq_along(stage_order)) {
    counts <- count_genotypes(pops[[i]])
    out[[i]] <- tibble(
      stage = stage_order[i],
      genotype = c("AA", "AS", "SS"),
      count = counts,
      proportion = counts / sum(counts)
    )
  }
  do.call(rbind, out)
}

allele_frequency_after_selection <- function(q_S, fitness_map) {
  p_A <- 1 - q_S
  wbar <- (p_A^2 * fitness_map[["AA"]]) +
    (2 * p_A * q_S * fitness_map[["AS"]]) +
    (q_S^2 * fitness_map[["SS"]])

  q_after_selection <- ((q_S^2 * fitness_map[["SS"]]) +
    (p_A * q_S * fitness_map[["AS"]])) /
    wbar

  unname(q_after_selection)
}

simulate_allele_frequency_path <- function(
  q_start,
  fitness_map,
  n_generations = 12
) {
  generations <- 0:n_generations
  q_values <- numeric(length(generations))
  q_values[1] <- q_start

  for (i in seq_len(n_generations)) {
    q_values[i + 1] <- allele_frequency_after_selection(
      q_values[i],
      fitness_map
    )
  }

  tibble(
    generation = generations,
    q_S = q_values,
    p_A = 1 - q_values
  )
}

simulate_selection <- function(
  n_individuals = 25,
  q_S = 0.4,
  fitness_map = relative_fitness,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  probs <- hw_genotype_probs(q_S)
  pop_initial <- sample(
    x = c("AA", "AS", "SS"),
    size = n_individuals,
    replace = TRUE,
    prob = unname(probs)
  )
  alive_initial <- rep(TRUE, length(pop_initial))

  alive_after_selection <- apply_selection(
    pop_initial,
    alive_initial,
    fitness_map = fitness_map
  )

  if (!any(alive_after_selection)) {
    stop(
      "No survivors after selection. Try increasing N or adjusting fitness values."
    )
  }

  pop_next_gen <- next_gen_hw(
    pop_initial[alive_after_selection],
    n_target = n_individuals
  )
  alive_next_gen <- rep(TRUE, length(pop_next_gen))

  list(
    n_individuals = n_individuals,
    q_S = q_S,
    fitness_map = fitness_map,
    pop_initial = pop_initial,
    alive_initial = alive_initial,
    alive_after_selection = alive_after_selection,
    pop_next_gen = pop_next_gen,
    alive_next_gen = alive_next_gen
  )
}

save_selection_outputs <- function(
  sim,
  prefix = "",
  panel_width = 16,
  panel_height = 5.5
) {
  file_prefix <- if (is.null(prefix)) "" else prefix

  initial_geno_counts <- count_genotypes(sim$pop_initial)
  initial_allele_counts <- count_alleles(sim$pop_initial)
  initial_allele_freqs <- initial_allele_counts / sum(initial_allele_counts)

  message(sprintf(
    "Initial sample (N=%d, target q(S)=%.3f): AA=%d, AS=%d, SS=%d | A=%d (%.3f), S=%d (%.3f)",
    sim$n_individuals,
    sim$q_S,
    initial_geno_counts["AA"],
    initial_geno_counts["AS"],
    initial_geno_counts["SS"],
    initial_allele_counts["A"],
    initial_allele_freqs["A"],
    initial_allele_counts["S"],
    initial_allele_freqs["S"]
  ))

  parents_post_selection <- sim$pop_initial[sim$alive_after_selection]
  n_A_parent <- sum(substring(parents_post_selection, 1, 1) == "A") +
    sum(substring(parents_post_selection, 2, 2) == "A")
  pA_parent <- n_A_parent / (2 * length(parents_post_selection))
  qS_parent <- 1 - pA_parent
  expected_hw <- c(
    AA = pA_parent^2,
    AS = 2 * pA_parent * qS_parent,
    SS = qS_parent^2
  )
  obs_counts <- count_genotypes(sim$pop_next_gen)
  obs_freq <- obs_counts / sum(obs_counts)

  message(sprintf(
    "Post-selection parent allele freqs: p(A)=%.3f, q(S)=%.3f",
    pA_parent,
    qS_parent
  ))
  message(sprintf(
    "Expected HW offspring freqs: AA=%.3f, AS=%.3f, SS=%.3f",
    expected_hw[1],
    expected_hw[2],
    expected_hw[3]
  ))
  message(sprintf(
    "Observed next-gen freqs: AA=%.3f, AS=%.3f, SS=%.3f",
    obs_freq[1],
    obs_freq[2],
    obs_freq[3]
  ))

  p_stage_0 <- plot_population_stage(
    sim$pop_initial,
    sim$alive_initial,
    "Paso 0: Cigotos (HW)"
  )
  p_stage_1 <- plot_population_stage(
    sim$pop_initial,
    sim$alive_after_selection,
    "Paso 1: Selección natural"
  )
  p_stage_2 <- plot_population_stage(
    sim$pop_next_gen,
    sim$alive_next_gen,
    "Paso 2: Cigotos de la siguiente generación (HW)"
  )

  blank_panel <- ggplot() +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = "gray88"),
      panel.background = element_rect(fill = "white", color = NA)
    )

  panel_1 <- plot_grid(
    p_stage_0,
    blank_panel,
    blank_panel,
    ncol = 3,
    align = "hv"
  )
  panel_2 <- plot_grid(
    p_stage_0,
    p_stage_1,
    blank_panel,
    ncol = 3,
    align = "hv"
  )
  panel_3 <- plot_grid(
    p_stage_0,
    p_stage_1,
    p_stage_2,
    ncol = 3,
    align = "hv"
  )

  ggsave(
    filename = file.path(
      out_dir,
      paste0(file_prefix, "selection-panels-1of3.png")
    ),
    plot = panel_1,
    width = panel_width,
    height = panel_height,
    dpi = 220,
    bg = "white"
  )
  ggsave(
    filename = file.path(
      out_dir,
      paste0(file_prefix, "selection-panels-2of3.png")
    ),
    plot = panel_2,
    width = panel_width,
    height = panel_height,
    dpi = 220,
    bg = "white"
  )
  ggsave(
    filename = file.path(
      out_dir,
      paste0(file_prefix, "selection-panels-3of3.png")
    ),
    plot = panel_3,
    width = panel_width,
    height = panel_height,
    dpi = 220,
    bg = "white"
  )

  stage_names <- c(
    "Cigotos (HW)",
    "Selección natural",
    "Cigotos de la siguiente generación (~HW)"
  )
  pop_after_selection <- sim$pop_initial[sim$alive_after_selection]
  pops <- list(sim$pop_initial, pop_after_selection, sim$pop_next_gen)
  tally_df <- build_tally_df(stage_names, pops)
  tally_df$stage <- factor(tally_df$stage, levels = stage_names)

  p_tally <- ggplot(
    tally_df,
    aes(x = stage, y = count, color = genotype, group = genotype)
  ) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 2.8) +
    geom_text(aes(label = count), vjust = -0.8, size = 3, show.legend = FALSE) +
    scale_color_manual(
      values = c("AA" = "#1f77b4", "AS" = "#2ca02c", "SS" = "#d62728")
    ) +
    labs(
      title = "Cambio en conteos genotipicos durante la seleccion",
      subtitle = "Tally por etapa: inicio, seleccion y siguiente generacion",
      x = NULL,
      y = "Numero de individuos",
      color = "Genotipo"
    ) +
    ylim(0, max(tally_df$count) + 4) +
    theme_diagram() +
    theme(axis.text.x = element_text(angle = 12, hjust = 1))

  ggsave(
    filename = file.path(
      out_dir,
      paste0(file_prefix, "selection-genotype-tally.png")
    ),
    plot = p_tally,
    width = 8,
    height = 4.8,
    dpi = 220
  )

  q_equilibrium <- ((sim$fitness_map[["AA"]] - sim$fitness_map[["AS"]]) /
    (sim$fitness_map[["AA"]] +
      sim$fitness_map[["SS"]] -
      2 * sim$fitness_map[["AS"]]))
  allele_path_df <- simulate_allele_frequency_path(
    q_start = sim$q_S,
    fitness_map = sim$fitness_map,
    n_generations = 12
  )

  p_equilibrium <- ggplot(allele_path_df, aes(x = generation, y = q_S)) +
    geom_hline(
      yintercept = q_equilibrium,
      linetype = "dashed",
      color = "#7f8c8d"
    ) +
    geom_line(linewidth = 1.1, color = "#2c7fb8") +
    geom_point(size = 2.8, color = "#2c7fb8") +
    geom_point(data = allele_path_df[1, ], size = 3.4, color = "#e74c3c") +
    annotate(
      "text",
      x = max(allele_path_df$generation) * 0.68,
      y = q_equilibrium + 0.015,
      label = sprintf("Equilibrio teórico: q(S)=%.3f", q_equilibrium),
      size = 3.5,
      color = "#4d4d4d"
    ) +
    labs(
      title = "Aproximación de la frecuencia del alelo S al equilibrio",
      subtitle = "Ventaja del heterocigoto: la selección desplaza q(S) hacia el equilibrio interno",
      x = "Generación",
      y = "Frecuencia de S, q(S)"
    ) +
    scale_x_continuous(
      breaks = seq(0, max(allele_path_df$generation), by = 2)
    ) +
    scale_y_continuous(limits = c(0.05, 0.45)) +
    theme_diagram()

  ggsave(
    filename = file.path(
      out_dir,
      paste0(file_prefix, "allele-frequency-equilibrium.png")
    ),
    plot = p_equilibrium,
    width = 8.5,
    height = 5,
    dpi = 300
  )

  message("Saved figures in lectures/06-assets:")
  message(paste0(" - ", file_prefix, "selection-panels-1of3.png"))
  message(paste0(" - ", file_prefix, "selection-panels-2of3.png"))
  message(paste0(" - ", file_prefix, "selection-panels-3of3.png"))
  message(paste0(" - ", file_prefix, "selection-genotype-tally.png"))
  message(paste0(" - ", file_prefix, "allele-frequency-equilibrium.png"))

  invisible(list(
    parent_allele_freqs = c(A = pA_parent, S = qS_parent),
    offspring_freqs = obs_freq,
    tally_df = tally_df
  ))
}

# -- Selection happens during a generation; evolution is observed across generations --
sim <- simulate_selection(
  n_individuals = 400,
  q_S = 0.4,
  fitness_map = relative_fitness,
  seed = 106
)

save_selection_outputs(sim)

# simulate when the initial S freq is lower than equilibrium

sim_01 <- simulate_selection(
  n_individuals = 400,
  q_S = 0.1,
  fitness_map = relative_fitness,
  seed = 106
)

save_selection_outputs(sim_01, prefix = "01")

save_fitness_chart(
  fitness_map = relative_fitness,
  file_name = "fitness-chart.png",
  title = "Fitness relativa por genotipo",
  subtitle = "Selección balanceadora (ventaja del heterocigoto)"
)

save_fitness_chart(
  fitness_map = diversifying_fitness,
  file_name = "fitness-chart-diversifying.png",
  title = "Fitness relativa por genotipo",
  subtitle = "Selección diversificadora (desventaja del heterocigoto)"
)

save_fitness_chart(
  fitness_map = directional_fitness,
  file_name = "fitness-chart-directional.png",
  title = "Fitness relativa por genotipo",
  subtitle = "Selección direccional (favorece el genotipo AA)"
)
