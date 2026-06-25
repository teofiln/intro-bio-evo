library(ggplot2)
library(tibble)

# Resolve project root regardless of invocation directory.
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

theme_diagram <- function() {
  theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9)
    )
}

simulate_split_with_barrier <- function(
  pop_size = 120,
  n_generations = 120,
  split_generation = 30,
  p0 = 0.5,
  m_after_split = 0,
  n_replicates = 80,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (split_generation < 1 || split_generation >= n_generations) {
    stop("split_generation must be between 1 and n_generations - 1")
  }

  if (m_after_split < 0 || m_after_split > 0.5) {
    stop("m_after_split must be between 0 and 0.5")
  }

  n_alleles <- 2 * pop_size
  generations <- 0:n_generations
  out <- vector("list", n_replicates)

  for (rep_id in seq_len(n_replicates)) {
    p1 <- numeric(length(generations))
    p2 <- numeric(length(generations))

    p1[1] <- p0
    p2[1] <- p0

    for (idx in 2:length(generations)) {
      g <- generations[idx]

      if (g <= split_generation) {
        p_ancestor <- rbinom(1, size = n_alleles, prob = p1[idx - 1]) /
          n_alleles
        p1[idx] <- p_ancestor
        p2[idx] <- p_ancestor
      } else {
        p1_mig <- (1 - m_after_split) *
          p1[idx - 1] +
          m_after_split * p2[idx - 1]
        p2_mig <- (1 - m_after_split) *
          p2[idx - 1] +
          m_after_split * p1[idx - 1]

        p1[idx] <- rbinom(1, size = n_alleles, prob = p1_mig) / n_alleles
        p2[idx] <- rbinom(1, size = n_alleles, prob = p2_mig) / n_alleles
      }
    }

    out[[rep_id]] <- rbind(
      tibble(
        generation = generations,
        p_a = p1,
        population = "Linaje 1",
        replicate = rep_id,
        m_after_split = m_after_split,
        phase = ifelse(
          generations <= split_generation,
          "Poblacion ancestral",
          "Despues de separacion"
        )
      ),
      tibble(
        generation = generations,
        p_a = p2,
        population = "Linaje 2",
        replicate = rep_id,
        m_after_split = m_after_split,
        phase = ifelse(
          generations <= split_generation,
          "Poblacion ancestral",
          "Despues de separacion"
        )
      )
    )
  }

  do.call(rbind, out)
}

compute_abs_difference <- function(sim_df) {
  pop1 <- sim_df[
    sim_df$population == "Linaje 1",
    c("generation", "replicate", "p_a", "m_after_split")
  ]
  pop2 <- sim_df[
    sim_df$population == "Linaje 2",
    c("generation", "replicate", "p_a", "m_after_split")
  ]

  names(pop1)[names(pop1) == "p_a"] <- "p1"
  names(pop2)[names(pop2) == "p_a"] <- "p2"

  merged <- merge(
    pop1,
    pop2,
    by = c("generation", "replicate", "m_after_split"),
    sort = TRUE
  )
  merged$abs_diff <- abs(merged$p1 - merged$p2)
  merged
}

make_trajectory_plot <- function(sim_df, split_generation, scenario_label) {
  ggplot(
    sim_df,
    aes(
      x = generation,
      y = p_a,
      group = interaction(population, replicate),
      color = population
    )
  ) +
    geom_line(alpha = 0.12, linewidth = 0.3) +
    stat_summary(
      aes(group = population),
      fun = mean,
      geom = "line",
      linewidth = 1.1
    ) +
    geom_vline(
      xintercept = split_generation,
      linetype = "dashed",
      color = "black",
      linewidth = 0.6
    ) +
    annotate(
      "text",
      x = split_generation,
      y = 0.98,
      label = "Separacion",
      vjust = 1,
      hjust = -0.05,
      size = 3
    ) +
    labs(
      title = "Un linaje ancestral que se divide en dos",
      subtitle = scenario_label,
      x = "Generacion",
      y = "Frecuencia del alelo a",
      color = "Linaje"
    ) +
    scale_color_manual(
      values = c("Linaje 1" = "#1f77b4", "Linaje 2" = "#d62728")
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    theme_diagram()
}

make_difference_plot <- function(diff_df, split_generation, scenario_label) {
  ggplot(diff_df, aes(x = generation, y = abs_diff, group = replicate)) +
    geom_line(color = "gray55", alpha = 0.14, linewidth = 0.3) +
    stat_summary(
      fun = mean,
      geom = "line",
      color = "#2ca02c",
      linewidth = 1.2
    ) +
    geom_vline(
      xintercept = split_generation,
      linetype = "dashed",
      color = "black",
      linewidth = 0.6
    ) +
    labs(
      title = "Divergencia entre linajes despues de la separacion",
      subtitle = scenario_label,
      x = "Generacion",
      y = "|p1 - p2|"
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    theme_diagram()
}

make_scenario_comparison_plot <- function(scenario_df, split_generation) {
  scenario_df$scenario <- factor(
    scenario_df$scenario,
    levels = c("Con barrera (m = 0)", "Sin barrera (m = 0.02)")
  )

  ggplot(
    scenario_df,
    aes(x = generation, y = mean_abs_diff, color = scenario)
  ) +
    geom_line(linewidth = 1.2) +
    geom_vline(
      xintercept = split_generation,
      linetype = "dashed",
      color = "black",
      linewidth = 0.6
    ) +
    labs(
      title = "Efecto de cortar el flujo genico",
      subtitle = "Misma poblacion ancestral; solo cambia el flujo despues de la separacion",
      x = "Generacion",
      y = "Promedio de |p1 - p2|",
      color = "Escenario"
    ) +
    scale_color_manual(
      values = c(
        "Con barrera (m = 0)" = "#d62728",
        "Sin barrera (m = 0.02)" = "#1f77b4"
      )
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    theme_diagram()
}

script_dir <- get_script_dir()
project_root <- find_project_root(c(script_dir, getwd()))
if (is.null(project_root)) {
  stop("Could not find project root (_quarto.yml)")
}

out_dir <- file.path(project_root, "lectures", "09-assets")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

pop_size <- 120
n_generations <- 120
split_generation <- 30
n_replicates <- 80

# Scenario 1: complete barrier after split (no migration).
sim_barrier <- simulate_split_with_barrier(
  pop_size = pop_size,
  n_generations = n_generations,
  split_generation = split_generation,
  p0 = 0.5,
  m_after_split = 0,
  n_replicates = n_replicates,
  seed = 901
)

# Scenario 2: continued migration after split (for contrast).
sim_flow <- simulate_split_with_barrier(
  pop_size = pop_size,
  n_generations = n_generations,
  split_generation = split_generation,
  p0 = 0.5,
  m_after_split = 0.02,
  n_replicates = n_replicates,
  seed = 902
)

diff_barrier <- compute_abs_difference(sim_barrier)
diff_flow <- compute_abs_difference(sim_flow)

p_barrier_traj <- make_trajectory_plot(
  sim_barrier,
  split_generation = split_generation,
  scenario_label = "Con barrera: m = 0 despues de la separacion"
)

p_barrier_diff <- make_difference_plot(
  diff_barrier,
  split_generation = split_generation,
  scenario_label = "Sin flujo genico despues de la separacion"
)

scenario_barrier <- aggregate(
  abs_diff ~ generation,
  data = diff_barrier,
  FUN = mean
)
scenario_barrier$scenario <- "Con barrera (m = 0)"

scenario_flow <- aggregate(abs_diff ~ generation, data = diff_flow, FUN = mean)
scenario_flow$scenario <- "Sin barrera (m = 0.02)"

scenario_df <- rbind(scenario_barrier, scenario_flow)
names(scenario_df)[names(scenario_df) == "abs_diff"] <- "mean_abs_diff"

p_compare <- make_scenario_comparison_plot(
  scenario_df,
  split_generation = split_generation
)

ggsave(
  filename = file.path(out_dir, "lineage_split_barrier_trajectories.png"),
  plot = p_barrier_traj,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(out_dir, "lineage_split_barrier_divergence.png"),
  plot = p_barrier_diff,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(out_dir, "lineage_split_barrier_vs_flow.png"),
  plot = p_compare,
  width = 9,
  height = 5,
  dpi = 300
)
