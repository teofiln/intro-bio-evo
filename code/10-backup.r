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
      axis.text = element_text(size = 9),
      strip.text = element_text(size = 9)
    )
}

# Simulate species-through-time dynamics using a birth-death process.
simulate_ltt <- function(
  scenario,
  lambda_fun,
  mu_fun,
  t_max = 30,
  dt = 0.1,
  n0 = 2,
  n_replicates = 80,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  times <- seq(0, t_max, by = dt)
  out <- vector("list", n_replicates)

  for (rep_id in seq_len(n_replicates)) {
    n_species <- numeric(length(times))
    n_species[1] <- n0

    for (i in 2:length(times)) {
      current_n <- n_species[i - 1]

      if (current_n <= 0) {
        n_species[i] <- 0
        next
      }

      t_prev <- times[i - 1]
      lambda_t <- max(0, lambda_fun(t_prev, rep_id))
      mu_t <- max(0, mu_fun(t_prev, rep_id))

      births <- rpois(1, lambda = current_n * lambda_t * dt)
      deaths <- rpois(1, lambda = current_n * mu_t * dt)

      n_species[i] <- max(0, current_n + births - deaths)
    }

    out[[rep_id]] <- tibble(
      time = times,
      n_species = n_species,
      replicate = rep_id,
      scenario = scenario
    )
  }

  do.call(rbind, out)
}

make_scenario_plot <- function(sim_df, scenario_label) {
  scenario_df <- sim_df[sim_df$scenario == scenario_label, ]

  ggplot(scenario_df, aes(x = time, y = n_species)) +
    geom_line(
      aes(group = replicate),
      linewidth = 0.25,
      alpha = 0.12,
      color = "gray50"
    ) +
    stat_summary(
      aes(group = 1),
      fun = mean,
      geom = "line",
      color = "#1f77b4",
      linewidth = 1.2
    ) +
    labs(
      title = "Species Through Time",
      subtitle = paste0(scenario_label, " | 80 simulaciones + promedio"),
      x = "Tiempo (millones de anos)",
      y = "Numero de especies"
    ) +
    theme_diagram() +
    theme(legend.position = "none")
}

make_simple_accumulation_panel <- function(
  speciation_rate = 1,
  t_max = 5,
  initial_species = 1
) {
  didactic_df <- tibble(
    time = 0:t_max,
    n_species = initial_species + speciation_rate * (0:t_max)
  )

  ggplot(didactic_df, aes(x = time, y = n_species)) +
    geom_step(linewidth = 1.1, color = "#d62728") +
    geom_point(size = 2.2, color = "#d62728") +
    labs(
      title = "Acumulacion simple de especies",
      subtitle = "Tasa de especiacion = 1 especie por millon de anos | Sin extincion",
      x = "Tiempo (millones de anos)",
      y = "Numero de especies"
    ) +
    scale_x_continuous(breaks = 0:t_max) +
    scale_y_continuous(breaks = didactic_df$n_species) +
    annotate(
      "text",
      x = 2.5,
      y = max(didactic_df$n_species) + 0.45,
      label = "En 5 millones de anos se agregan 5 especies",
      size = 3.3
    ) +
    coord_cartesian(
      ylim = c(min(didactic_df$n_species), max(didactic_df$n_species) + 0.9)
    ) +
    theme_diagram()
}

build_toy_fossil_sequence <- function() {
  tibble(
    lineage = c("A", "B", "C", "B1", "B2"),
    start = c(1, 1, 1, 3, 3),
    end = c(5, 3, 2, 5, 5),
    y = c(5, 4, 3, 2, 1)
  )
}

make_first_last_appearance_plot <- function() {
  toy_df <- build_toy_fossil_sequence()

  extant_df <- toy_df[toy_df$lineage %in% c("A", "B1", "B2"), ]
  extinct_df <- toy_df[toy_df$lineage == "C", ]
  ancestor_df <- toy_df[toy_df$lineage == "B", ]

  ggplot(toy_df) +
    geom_segment(
      aes(x = start, xend = end, y = y, yend = y),
      linewidth = 2.4,
      color = "#1f4e79",
      lineend = "round"
    ) +
    geom_point(
      data = extant_df,
      aes(x = end, y = y),
      size = 3.2,
      color = "#2ca02c"
    ) +
    geom_point(
      data = extinct_df,
      aes(x = end, y = y),
      size = 3.2,
      color = "#d62728"
    ) +
    geom_point(
      data = ancestor_df,
      aes(x = end, y = y),
      size = 3.2,
      shape = 21,
      stroke = 0.8,
      color = "#444444",
      fill = "white"
    ) +
    geom_text(
      aes(x = start - 0.10, y = y, label = lineage),
      hjust = 1,
      size = 3.6
    ) +
    annotate(
      "text",
      x = 4.95,
      y = 5.45,
      label = "Verde: linajes que llegan al presente",
      hjust = 1,
      size = 3,
      color = "#2ca02c"
    ) +
    annotate(
      "text",
      x = 4.95,
      y = 5.05,
      label = "Rojo: linaje extinto (C)",
      hjust = 1,
      size = 3,
      color = "#d62728"
    ) +
    annotate(
      "text",
      x = 4.95,
      y = 4.65,
      label = "Circulo blanco: B representa un ancestro",
      hjust = 1,
      size = 3,
      color = "#444444"
    ) +
    labs(
      title = "La misma historia, pero como primeras y ultimas apariciones",
      subtitle = "Cada barra muestra el intervalo observado para un linaje",
      x = "Tiempo (millones de anos)",
      y = NULL
    ) +
    scale_x_continuous(breaks = 1:5, limits = c(0.75, 5.15)) +
    scale_y_continuous(breaks = NULL, limits = c(0.5, 5.7)) +
    theme_diagram() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
}

make_phylogeny_plot <- function(
  filename,
  width = 8.8,
  height = 5.4,
  dpi = 300
) {
  if (!requireNamespace("ape", quietly = TRUE)) {
    stop("Package 'ape' is required to render the phylogeny figure.")
  }

  phy <- ape::read.tree(text = "(A:4,(C:1,(B1:2,B2:2):2):0);")
  split_node <- ape::getMRCA(phy, c("B1", "B2"))

  grDevices::png(
    filename = filename,
    width = width,
    height = height,
    units = "in",
    res = dpi
  )

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(
    {
      graphics::par(old_par)
      grDevices::dev.off()
    },
    add = TRUE
  )

  graphics::par(mar = c(6.2, 1.2, 5.2, 1.0), xpd = NA)

  ape::plot.phylo(
    phy,
    type = "phylogram",
    direction = "rightwards",
    show.tip.label = FALSE,
    no.margin = FALSE,
    edge.color = "#1f4e79",
    edge.width = 5,
    x.lim = c(-0.15, 5.35),
    font = 1,
    cex = 1.05
  )

  plot_state <- get("last_plot.phylo", envir = ape::.PlotPhyloEnv)
  tip_ids <- seq_len(ape::Ntip(phy))
  tip_x <- plot_state$xx[tip_ids]
  tip_y <- plot_state$yy[tip_ids]
  names(tip_x) <- phy$tip.label
  names(tip_y) <- phy$tip.label

  extant_tips <- c("A", "B1", "B2")
  extinct_tip <- "C"

  graphics::points(
    x = tip_x[extant_tips],
    y = tip_y[extant_tips],
    pch = 16,
    cex = 1.3,
    col = "#2ca02c"
  )
  graphics::points(
    x = tip_x[extinct_tip],
    y = tip_y[extinct_tip],
    pch = 16,
    cex = 1.3,
    col = "#d62728"
  )

  graphics::text(
    x = tip_x + 0.14,
    y = tip_y,
    labels = phy$tip.label,
    adj = c(0, 0.5),
    cex = 1.0
  )

  parent_node <- phy$edge[phy$edge[, 2] == split_node, 1]
  branch_mid_x <- mean(plot_state$xx[c(parent_node, split_node)])
  branch_mid_y <- mean(plot_state$yy[c(parent_node, split_node)])

  graphics::text(
    x = branch_mid_x,
    y = branch_mid_y + 0.22,
    labels = "B",
    cex = 0.95
  )

  graphics::title(
    main = "Una filogenia compatible con la misma historia",
    xlab = "Tiempo (millones de anos)"
  )
  graphics::axis(1, at = 0:4, labels = 1:5)

  graphics::text(
    x = 4.75,
    y = max(tip_y) + 0.45,
    labels = "Verde: linajes que llegan al presente",
    adj = 1,
    cex = 0.9,
    col = "#2ca02c"
  )
  graphics::text(
    x = 4.75,
    y = max(tip_y) + 0.12,
    labels = "Rojo: linaje extinto (C)",
    adj = 1,
    cex = 0.9,
    col = "#d62728"
  )
  graphics::text(
    x = 4.75,
    y = max(tip_y) - 0.21,
    labels = "B marca el segmento ancestral del clado B1-B2",
    adj = 1,
    cex = 0.9,
    col = "#444444"
  )

  invisible(filename)
}

make_event_count_rate_plot <- function() {
  toy_df <- build_toy_fossil_sequence()

  speciation_events <- 1
  extinction_events <- 1
  lineage_time <- sum(toy_df$end - toy_df$start)

  lambda_hat <- speciation_events / lineage_time
  mu_hat <- extinction_events / lineage_time
  r_hat <- lambda_hat - mu_hat

  rate_text <- paste0(
    "Conteo en esta ventana temporal\n",
    "Especiaciones = ",
    speciation_events,
    " | Extinciones = ",
    extinction_events,
    "\nLineage-time total = ",
    lineage_time,
    " linaje-Myr",
    "\n\n",
    "lambda_hat = ",
    speciation_events,
    "/",
    lineage_time,
    " = ",
    sprintf("%.3f", lambda_hat),
    " eventos/linaje/Myr",
    "\nmu_hat = ",
    extinction_events,
    "/",
    lineage_time,
    " = ",
    sprintf("%.3f", mu_hat),
    " eventos/linaje/Myr",
    "\nr_hat = lambda_hat - mu_hat = ",
    sprintf("%.3f", r_hat)
  )

  ggplot(toy_df) +
    geom_segment(
      aes(x = start, xend = end, y = y, yend = y),
      linewidth = 2.2,
      color = "#1f4e79",
      lineend = "round"
    ) +
    geom_vline(xintercept = 2, linetype = "dashed", color = "#d62728") +
    geom_vline(xintercept = 3, linetype = "dashed", color = "#2ca02c") +
    annotate(
      "text",
      x = 2,
      y = 4.45,
      label = "extincion",
      color = "#d62728",
      size = 3
    ) +
    annotate(
      "text",
      x = 3,
      y = 4.45,
      label = "especiacion",
      color = "#2ca02c",
      size = 3
    ) +
    annotate(
      "label",
      x = 4.05,
      y = 3.0,
      label = rate_text,
      hjust = 0,
      vjust = 1,
      size = 3,
      fill = "white"
    ) +
    labs(
      title = "Del registro fosil al calculo de tasas",
      subtitle = "Contamos eventos y los dividimos entre tiempo total de linajes expuestos",
      x = "Tiempo (millones de anos)",
      y = "Linaje"
    ) +
    scale_x_continuous(breaks = 1:5, limits = c(0.9, 5.95)) +
    scale_y_continuous(
      breaks = toy_df$y,
      labels = toy_df$lineage,
      limits = c(0.5, 5.5)
    ) +
    theme_diagram()
}

make_fossil_sequence_plot <- function(seed = 250) {
  set.seed(seed)

  lineage_df <- tibble(
    lineage = c("A", "B", "C", "D", "E", "F"),
    start = c(0, 3, 3, 8, 8, 10),
    end = c(3, 8, 10, 12, 12, 12),
    y = c(3.2, 4.2, 2.4, 5.0, 3.7, 2.0)
  )

  branch_df <- tibble(
    x = c(3, 8, 10),
    y_from = c(3.2, 4.2, 2.4),
    y_to = c(4.2, 5.0, 2.0),
    y_to_2 = c(2.4, 3.7, NA_real_)
  )

  fossils_df <- do.call(
    rbind,
    lapply(seq_len(nrow(lineage_df)), function(i) {
      k <- sample(2:4, size = 1)
      tibble(
        lineage = lineage_df$lineage[i],
        time = sort(runif(
          k,
          lineage_df$start[i] + 0.15,
          lineage_df$end[i] - 0.15
        )),
        y = lineage_df$y[i]
      )
    })
  )

  ggplot() +
    geom_segment(
      data = lineage_df,
      aes(x = start, xend = end, y = y, yend = y),
      linewidth = 1.4,
      color = "#1f4e79"
    ) +
    geom_segment(
      data = branch_df,
      aes(x = x, xend = x, y = y_from, yend = y_to),
      linewidth = 1.1,
      color = "#1f4e79"
    ) +
    geom_segment(
      data = branch_df[!is.na(branch_df$y_to_2), ],
      aes(x = x, xend = x, y = y_from, yend = y_to_2),
      linewidth = 1.1,
      color = "#1f4e79"
    ) +
    geom_point(
      data = fossils_df,
      aes(x = time, y = y),
      size = 2,
      color = "#d95f02"
    ) +
    geom_text(
      data = lineage_df,
      aes(x = start + 0.15, y = y + 0.18, label = lineage),
      size = 3,
      hjust = 0,
      color = "#1f4e79"
    ) +
    annotate(
      "text",
      x = 1.8,
      y = 5.55,
      label = "Puntos = ocurrencias fosiles",
      size = 3.1,
      color = "#d95f02"
    ) +
    labs(
      title = "Secuencia fosil simplificada",
      subtitle = "Las lineas son linajes; las ramificaciones son eventos de especiacion",
      x = "Tiempo (millones de anos)",
      y = "Linajes"
    ) +
    scale_x_continuous(breaks = seq(0, 12, by = 2)) +
    scale_y_continuous(
      breaks = lineage_df$y,
      labels = lineage_df$lineage
    ) +
    coord_cartesian(ylim = c(1.5, 5.8)) +
    theme_diagram()
}

simulate_event_path <- function(
  lambda = 0.25,
  mu = 0.15,
  t_max = 20,
  n0 = 3,
  seed = 1
) {
  set.seed(seed)

  if (n0 <= 0) {
    stop("n0 must be > 0")
  }

  events <- tibble(
    time = 0,
    event = "start",
    n_before = n0,
    n_after = n0
  )

  current_time <- 0
  current_n <- n0

  while (current_time < t_max && current_n > 0) {
    total_rate <- current_n * (lambda + mu)
    if (total_rate <= 0) {
      break
    }

    wait_time <- rexp(1, rate = total_rate)
    next_time <- current_time + wait_time

    if (next_time > t_max) {
      break
    }

    is_speciation <- runif(1) < (lambda / (lambda + mu))
    new_n <- if (is_speciation) current_n + 1 else max(0, current_n - 1)

    events <- rbind(
      events,
      tibble(
        time = next_time,
        event = if (is_speciation) "speciation" else "extinction",
        n_before = current_n,
        n_after = new_n
      )
    )

    current_time <- next_time
    current_n <- new_n
  }

  events
}

events_to_step_df <- function(events, t_max) {
  step_rows <- vector("list", nrow(events) + 1)

  for (i in seq_len(nrow(events))) {
    t_start <- events$time[i]
    t_end <- if (i < nrow(events)) events$time[i + 1] else t_max
    step_rows[[i]] <- tibble(
      time = t_start,
      time_end = t_end,
      n_species = events$n_after[i]
    )
  }

  do.call(rbind, step_rows)
}

make_event_marks_plot <- function(events, t_max, lambda, mu) {
  step_df <- events_to_step_df(events, t_max)
  event_df <- events[events$event != "start", ]

  ggplot(step_df) +
    geom_segment(
      aes(x = time, xend = time_end, y = n_species, yend = n_species),
      linewidth = 1.25,
      color = "#1f77b4"
    ) +
    geom_segment(
      data = event_df,
      aes(x = time, xend = time, y = n_before, yend = n_after),
      linewidth = 1,
      color = "gray45"
    ) +
    geom_point(
      data = event_df[event_df$event == "speciation", ],
      aes(x = time, y = n_after),
      shape = 24,
      size = 3,
      fill = "#2ca02c",
      color = "#2ca02c"
    ) +
    geom_point(
      data = event_df[event_df$event == "extinction", ],
      aes(x = time, y = n_after),
      shape = 25,
      size = 3,
      fill = "#d62728",
      color = "#d62728"
    ) +
    labs(
      title = "Una realizacion estocastica del proceso nacimiento-muerte",
      subtitle = paste0(
        "Mismas tasas todo el tiempo: lambda=",
        lambda,
        ", mu=",
        mu
      ),
      x = "Tiempo (millones de anos)",
      y = "Numero de especies"
    ) +
    scale_x_continuous(breaks = seq(0, t_max, by = 2)) +
    theme_diagram()
}

make_same_rates_many_paths_plot <- function(
  lambda = 0.25,
  mu = 0.15,
  t_max = 20,
  n0 = 3,
  n_paths = 8,
  seed = 500
) {
  set.seed(seed)
  seeds <- sample(1:100000, n_paths)

  path_df <- do.call(
    rbind,
    lapply(seq_len(n_paths), function(i) {
      ev <- simulate_event_path(
        lambda = lambda,
        mu = mu,
        t_max = t_max,
        n0 = n0,
        seed = seeds[i]
      )
      st <- events_to_step_df(ev, t_max)
      tibble(
        replicate = paste0("run_", i),
        time = st$time,
        n_species = st$n_species
      )
    })
  )

  ggplot(path_df, aes(x = time, y = n_species, group = replicate)) +
    geom_step(alpha = 0.7, linewidth = 0.8, color = "#1f4e79") +
    labs(
      title = "Mismas tasas, trayectorias diferentes",
      subtitle = paste0(
        "Todas empiezan con ",
        n0,
        " especies y usan lambda=",
        lambda,
        ", mu=",
        mu
      ),
      x = "Tiempo (millones de anos)",
      y = "Numero de especies"
    ) +
    scale_x_continuous(breaks = seq(0, t_max, by = 2)) +
    theme_diagram()
}

# Build output path from project root.
script_dir <- get_script_dir()
project_root <- find_project_root(c(script_dir, getwd()))
if (is.null(project_root)) {
  stop("Could not find project root (_quarto.yml)")
}

out_dir <- file.path(project_root, "lectures", "10-assets")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Scenario 1: constant speciation, no extinction.
scenario_1 <- "1) Especiacion constante | extincion = 0"
lambda_1 <- function(t, rep_id) 0.20
mu_1 <- function(t, rep_id) 0.00
sim_1 <- simulate_ltt(
  scenario = scenario_1,
  lambda_fun = lambda_1,
  mu_fun = mu_1,
  n0 = 1,
  seed = 101
)

# Scenario 2: varying speciation, no extinction.
rep_effect_2 <- rlnorm(80, meanlog = -0.5 * 0.25^2, sdlog = 0.25)
scenario_2 <- "2) Especiacion variable | extincion = 0"
lambda_2 <- function(t, rep_id) {
  max(0.01, (0.18 + 0.08 * sin(2 * pi * t / 8)) * rep_effect_2[rep_id])
}
mu_2 <- function(t, rep_id) 0.00
sim_2 <- simulate_ltt(
  scenario = scenario_2,
  lambda_fun = lambda_2,
  mu_fun = mu_2,
  n0 = 1,
  seed = 102
)

# Scenario 3: constant speciation with extinction.
scenario_3 <- "3) Especiacion constante + extincion"
lambda_3 <- function(t, rep_id) 0.20
mu_3 <- function(t, rep_id) 0.10
sim_3 <- simulate_ltt(
  scenario = scenario_3,
  lambda_fun = lambda_3,
  mu_fun = mu_3,
  n0 = 1,
  seed = 103
)

# Scenario 4: varying speciation and varying extinction.
rep_effect_4 <- rlnorm(80, meanlog = -0.5 * 0.3^2, sdlog = 0.3)
scenario_4 <- "4) Especiacion y extincion variables"
lambda_4 <- function(t, rep_id) {
  max(0.01, (0.22 + 0.10 * sin(2 * pi * t / 10)) * rep_effect_4[rep_id])
}
mu_4 <- function(t, rep_id) {
  max(
    0.00,
    (0.08 + 0.07 * (cos(2 * pi * t / 6) + 1) / 2) * rep_effect_4[rep_id]
  )
}
sim_4 <- simulate_ltt(
  scenario = scenario_4,
  lambda_fun = lambda_4,
  mu_fun = mu_4,
  n0 = 1,
  seed = 104
)

sim_all <- rbind(sim_1, sim_2, sim_3, sim_4)
scenario_order <- c(scenario_1, scenario_2, scenario_3, scenario_4)
sim_all$scenario <- factor(sim_all$scenario, levels = scenario_order)

p_s1 <- make_scenario_plot(sim_all, scenario_1)
p_s2 <- make_scenario_plot(sim_all, scenario_2)
p_s3 <- make_scenario_plot(sim_all, scenario_3)
p_s4 <- make_scenario_plot(sim_all, scenario_4)
p_simple <- make_simple_accumulation_panel(
  speciation_rate = 1,
  t_max = 5,
  initial_species = 1
)
p_fad_lad <- make_first_last_appearance_plot()
p_count_rates <- make_event_count_rate_plot()
p_fossil <- make_fossil_sequence_plot(seed = 250)
events_one <- simulate_event_path(
  lambda = 0.25,
  mu = 0.15,
  t_max = 20,
  n0 = 3,
  seed = 714
)
p_events_one <- make_event_marks_plot(
  events = events_one,
  t_max = 20,
  lambda = 0.25,
  mu = 0.15
)
p_many_paths <- make_same_rates_many_paths_plot(
  lambda = 0.25,
  mu = 0.15,
  t_max = 20,
  n0 = 3,
  n_paths = 8,
  seed = 501
)

ggsave(
  filename = file.path(out_dir, "species_scenario_1_constant_speciation.png"),
  plot = p_s1,
  width = 8.8,
  height = 5.4,
  dpi = 300
)

ggsave(
  filename = file.path(out_dir, "species_scenario_2_variable_speciation.png"),
  plot = p_s2,
  width = 8.8,
  height = 5.4,
  dpi = 300
)

ggsave(
  filename = file.path(out_dir, "species_scenario_3_with_extinction.png"),
  plot = p_s3,
  width = 8.8,
  height = 5.4,
  dpi = 300
)

ggsave(
  filename = file.path(out_dir, "species_scenario_4_variable_both.png"),
  plot = p_s4,
  width = 8.8,
  height = 5.4,
  dpi = 300
)

ggsave(
  filename = file.path(
    out_dir,
    "species_simple_accumulation_1_per_myr_5myr.png"
  ),
  plot = p_simple,
  width = 8.8,
  height = 5.4,
  dpi = 300
)

ggsave(
  filename = file.path(out_dir, "fossil_first_last_appearance_toy.png"),
  plot = p_fad_lad,
  width = 8.8,
  height = 5.4,
  dpi = 300
)

make_phylogeny_plot(
  filename = file.path(out_dir, "fossil_phylogeny_toy.png"),
  width = 8.8,
  height = 5.4,
  dpi = 300
)

ggsave(
  filename = file.path(out_dir, "fossil_event_counts_and_rates_toy.png"),
  plot = p_count_rates,
  width = 8.8,
  height = 5.4,
  dpi = 300
)

ggsave(
  filename = file.path(out_dir, "fossil_sequence_simplified_timeline.png"),
  plot = p_fossil,
  width = 8.8,
  height = 5.4,
  dpi = 300
)

ggsave(
  filename = file.path(out_dir, "birth_death_single_realization_marks.png"),
  plot = p_events_one,
  width = 8.8,
  height = 5.4,
  dpi = 300
)

ggsave(
  filename = file.path(out_dir, "birth_death_same_rates_many_paths.png"),
  plot = p_many_paths,
  width = 8.8,
  height = 5.4,
  dpi = 300
)
