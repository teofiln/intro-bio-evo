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

rate_to_token <- function(migration_rate, digits = 4) {
  formatted <- formatC(migration_rate, format = "f", digits = digits)
  paste0("m", gsub("\\.", "p", formatted))
}

rate_pair_token <- function(m12, m21, digits = 4) {
  paste0(
    "m12_",
    rate_to_token(m12, digits = digits),
    "_m21_",
    rate_to_token(m21, digits = digits)
  )
}

# Two-population Wright-Fisher simulation with symmetric migration and drift.
# migration_rate is the fraction of each population replaced by migrants per generation.
simulate_gene_flow <- function(
  pop_size = 100,
  n_generations = 80,
  p1_0 = 0.9,
  p2_0 = 0.1,
  migration_rate = 0.01,
  n_replicates = 60,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (migration_rate < 0 || migration_rate > 0.5) {
    stop("migration_rate must be between 0 and 0.5")
  }

  n_alleles <- 2 * pop_size
  generations <- 0:n_generations
  out <- vector("list", n_replicates)

  for (rep_id in seq_len(n_replicates)) {
    p1 <- numeric(length(generations))
    p2 <- numeric(length(generations))

    p1[1] <- p1_0
    p2[1] <- p2_0

    for (g in 2:length(generations)) {
      p1_mig <- (1 - migration_rate) * p1[g - 1] + migration_rate * p2[g - 1]
      p2_mig <- (1 - migration_rate) * p2[g - 1] + migration_rate * p1[g - 1]

      p1[g] <- rbinom(1, size = n_alleles, prob = p1_mig) / n_alleles
      p2[g] <- rbinom(1, size = n_alleles, prob = p2_mig) / n_alleles
    }

    out[[rep_id]] <- rbind(
      tibble(
        generation = generations,
        p_a = p1,
        population = "Poblacion 1",
        replicate = rep_id,
        migration_rate = migration_rate
      ),
      tibble(
        generation = generations,
        p_a = p2,
        population = "Poblacion 2",
        replicate = rep_id,
        migration_rate = migration_rate
      )
    )
  }

  do.call(rbind, out)
}

# Asymmetric migration: m12 is migration from pop1 -> pop2, m21 is pop2 -> pop1.
simulate_gene_flow_asymmetric <- function(
  pop_size1 = 200,
  pop_size2 = 50,
  n_generations = 80,
  p1_0 = 0.9,
  p2_0 = 0.1,
  m12 = 0.05,
  m21 = 0.005,
  n_replicates = 60,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (m12 < 0 || m12 > 1 || m21 < 0 || m21 > 1) {
    stop("m12 and m21 must be between 0 and 1")
  }

  n_alleles1 <- 2 * pop_size1
  n_alleles2 <- 2 * pop_size2
  generations <- 0:n_generations
  out <- vector("list", n_replicates)

  for (rep_id in seq_len(n_replicates)) {
    p1 <- numeric(length(generations))
    p2 <- numeric(length(generations))

    p1[1] <- p1_0
    p2[1] <- p2_0

    for (g in 2:length(generations)) {
      p1_mig <- (1 - m21) * p1[g - 1] + m21 * p2[g - 1]
      p2_mig <- (1 - m12) * p2[g - 1] + m12 * p1[g - 1]

      p1[g] <- rbinom(1, size = n_alleles1, prob = p1_mig) / n_alleles1
      p2[g] <- rbinom(1, size = n_alleles2, prob = p2_mig) / n_alleles2
    }

    out[[rep_id]] <- rbind(
      tibble(
        generation = generations,
        p_a = p1,
        population = "Poblacion 1",
        replicate = rep_id,
        migration_rate = NA_real_
      ),
      tibble(
        generation = generations,
        p_a = p2,
        population = "Poblacion 2",
        replicate = rep_id,
        migration_rate = NA_real_
      )
    )
  }

  do.call(rbind, out)
}

compute_abs_difference <- function(sim_df) {
  pop1 <- sim_df[
    sim_df$population == "Poblacion 1",
    c("generation", "replicate", "p_a")
  ]
  pop2 <- sim_df[
    sim_df$population == "Poblacion 2",
    c("generation", "replicate", "p_a")
  ]

  names(pop1)[names(pop1) == "p_a"] <- "p1"
  names(pop2)[names(pop2) == "p_a"] <- "p2"

  merged <- merge(pop1, pop2, by = c("generation", "replicate"), sort = TRUE)
  merged$abs_diff <- abs(merged$p1 - merged$p2)
  merged
}

make_trajectory_plot <- function(sim_df, title_tag, subtitle_text) {
  ggplot(
    sim_df,
    aes(
      x = generation,
      y = p_a,
      group = interaction(population, replicate),
      color = population
    )
  ) +
    geom_line(alpha = 0.13, linewidth = 0.3) +
    stat_summary(
      aes(group = population),
      fun = mean,
      geom = "line",
      linewidth = 1.1
    ) +
    labs(
      title = paste0(
        "Flujo genico + deriva (",
        title_tag,
        ")"
      ),
      subtitle = subtitle_text,
      x = "Generacion",
      y = "Frecuencia del alelo a",
      color = "Poblacion"
    ) +
    scale_color_manual(
      values = c("Poblacion 1" = "#1f77b4", "Poblacion 2" = "#d62728")
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    theme_diagram()
}

make_similarity_plot <- function(diff_df, title_tag, subtitle_text) {
  ggplot(
    diff_df,
    aes(x = generation, y = abs_diff, group = replicate)
  ) +
    geom_line(color = "gray55", alpha = 0.15, linewidth = 0.3) +
    stat_summary(
      fun = mean,
      geom = "line",
      color = "#2ca02c",
      linewidth = 1.2
    ) +
    labs(
      title = paste0(
        "Convergencia entre poblaciones (",
        title_tag,
        ")"
      ),
      subtitle = subtitle_text,
      x = "Generacion",
      y = "|p1 - p2|"
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    theme_diagram()
}

make_scenario_plot <- function(scenario_df) {
  scenario_df$scenario <- factor(
    scenario_df$scenario,
    levels = unique(scenario_df$scenario)
  )

  ggplot(
    scenario_df,
    aes(x = generation, y = mean_abs_diff, color = scenario)
  ) +
    geom_line(linewidth = 1) +
    labs(
      title = "Comparacion de escenarios de flujo genico",
      subtitle = "Menor |p1 - p2| implica mayor similitud entre poblaciones",
      x = "Generacion",
      y = "Promedio de |p1 - p2|",
      color = "Escenario"
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    theme_diagram()
}

make_scenario_plot_asymmetric <- function(scenario_df) {
  scenario_df$scenario <- factor(
    scenario_df$scenario,
    levels = unique(scenario_df$scenario)
  )

  ggplot(
    scenario_df,
    aes(x = generation, y = mean_abs_diff, color = scenario)
  ) +
    geom_line(linewidth = 1) +
    labs(
      title = "Comparacion de escenarios asimetricos",
      subtitle = "Asimetria en flujo genico: m12 (P1->P2) y m21 (P2->P1)",
      x = "Generacion",
      y = "Promedio de |p1 - p2|",
      color = "Escenario"
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    theme_diagram()
}

script_dir <- get_script_dir()
project_root <- find_project_root(c(script_dir, getwd()))
if (is.null(project_root)) {
  stop("Could not find project root (_quarto.yml)")
}

out_dir <- file.path(project_root, "lectures", "08-assets")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Default setup: with N = 100, migration_rate = 0.01 is ~1 migrant per generation.
pop_size <- 100
n_generations <- 80
n_replicates <- 60
migration_rates <- c(0, 0.005, 0.01, 0.02, 0.05)

scenario_rows <- vector("list", length(migration_rates))

for (i in seq_along(migration_rates)) {
  m <- migration_rates[i]
  m_token <- rate_to_token(m)

  sim_df <- simulate_gene_flow(
    pop_size = pop_size,
    n_generations = n_generations,
    p1_0 = 0.9,
    p2_0 = 0.1,
    migration_rate = m,
    n_replicates = n_replicates,
    seed = 800 + i
  )

  diff_df <- compute_abs_difference(sim_df)

  p_traj <- make_trajectory_plot(
    sim_df,
    title_tag = m_token,
    subtitle_text = paste0(
      "Dos poblaciones | N = ",
      pop_size,
      " | p_a inicial: 0.9 vs 0.1"
    )
  )
  p_diff <- make_similarity_plot(
    diff_df,
    title_tag = m_token,
    subtitle_text = paste0(
      "Diferencia absoluta |p1 - p2| con N = ",
      pop_size,
      " por poblacion"
    )
  )

  ggsave(
    filename = file.path(
      out_dir,
      paste0("gene_flow_trajectories_", m_token, ".png")
    ),
    plot = p_traj,
    width = 9,
    height = 5,
    dpi = 300
  )

  ggsave(
    filename = file.path(
      out_dir,
      paste0("gene_flow_similarity_", m_token, ".png")
    ),
    plot = p_diff,
    width = 9,
    height = 5,
    dpi = 300
  )

  mean_diff <- aggregate(abs_diff ~ generation, data = diff_df, FUN = mean)
  mean_diff$scenario <- paste0(
    m_token,
    " (~",
    round(m * pop_size, 2),
    " migrantes/pob/gen)"
  )
  names(mean_diff)[names(mean_diff) == "abs_diff"] <- "mean_abs_diff"

  scenario_rows[[i]] <- mean_diff
}

scenario_df <- do.call(rbind, scenario_rows)
p_scenarios <- make_scenario_plot(scenario_df)

ggsave(
  filename = file.path(out_dir, "gene_flow_similarity_scenarios.png"),
  plot = p_scenarios,
  width = 9,
  height = 5,
  dpi = 300
)

# Asymmetric migration scenarios (e.g., larger source into smaller recipient).
pop_size1_asym <- 200
pop_size2_asym <- 50
asym_scenarios <- data.frame(
  m12 = c(0.01, 0.05, 0.10),
  m21 = c(0.01, 0.005, 0.001)
)

asym_rows <- vector("list", nrow(asym_scenarios))

for (i in seq_len(nrow(asym_scenarios))) {
  m12 <- asym_scenarios$m12[i]
  m21 <- asym_scenarios$m21[i]
  asym_token <- rate_pair_token(m12, m21)

  sim_asym <- simulate_gene_flow_asymmetric(
    pop_size1 = pop_size1_asym,
    pop_size2 = pop_size2_asym,
    n_generations = n_generations,
    p1_0 = 0.9,
    p2_0 = 0.1,
    m12 = m12,
    m21 = m21,
    n_replicates = n_replicates,
    seed = 900 + i
  )

  diff_asym <- compute_abs_difference(sim_asym)

  p_asym_traj <- make_trajectory_plot(
    sim_asym,
    title_tag = asym_token,
    subtitle_text = paste0(
      "Asimetrico | N1 = ",
      pop_size1_asym,
      ", N2 = ",
      pop_size2_asym,
      " | m12(P1->P2)=",
      m12,
      ", m21(P2->P1)=",
      m21
    )
  )

  p_asym_diff <- make_similarity_plot(
    diff_asym,
    title_tag = asym_token,
    subtitle_text = paste0(
      "Convergencia bajo migracion asimetrica | N1 = ",
      pop_size1_asym,
      ", N2 = ",
      pop_size2_asym
    )
  )

  ggsave(
    filename = file.path(
      out_dir,
      paste0("gene_flow_trajectories_asymmetric_", asym_token, ".png")
    ),
    plot = p_asym_traj,
    width = 9,
    height = 5,
    dpi = 300
  )

  ggsave(
    filename = file.path(
      out_dir,
      paste0("gene_flow_similarity_asymmetric_", asym_token, ".png")
    ),
    plot = p_asym_diff,
    width = 9,
    height = 5,
    dpi = 300
  )

  mean_diff_asym <- aggregate(
    abs_diff ~ generation,
    data = diff_asym,
    FUN = mean
  )
  mean_diff_asym$scenario <- paste0(
    asym_token,
    " | N1=",
    pop_size1_asym,
    ", N2=",
    pop_size2_asym
  )
  names(mean_diff_asym)[names(mean_diff_asym) == "abs_diff"] <- "mean_abs_diff"

  asym_rows[[i]] <- mean_diff_asym
}

asym_df <- do.call(rbind, asym_rows)
p_asym_scenarios <- make_scenario_plot_asymmetric(asym_df)

ggsave(
  filename = file.path(
    out_dir,
    "gene_flow_similarity_scenarios_asymmetric.png"
  ),
  plot = p_asym_scenarios,
  width = 9,
  height = 5,
  dpi = 300
)
