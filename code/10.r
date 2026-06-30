library(ggplot2)
library(tibble)

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
  step_rows <- vector("list", nrow(events))

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

make_scale_sampling_contrast_plot <- function(seed = 812) {
  set.seed(seed)

  true_time <- seq(0, 320, by = 1)
  true_trait <-
    7.8 +
    0.0038 * true_time +
    0.62 * plogis((true_time - 185) / 11) +
    cumsum(rnorm(length(true_time), mean = 0, sd = 0.008))

  yearly_df <- tibble(
    time = true_time,
    trait = true_trait,
    panel = "Muestreo anual (cada 1 ano)"
  )

  sparse_idx <- seq(1, length(true_time), by = 20)
  every20_df <- tibble(
    time = true_time[sparse_idx],
    trait = true_trait[sparse_idx],
    panel = "Muestreo escaso (cada 20 anos)"
  )

  very_sparse_idx <- seq(1, length(true_time), by = 40)
  every40_df <- tibble(
    time = true_time[very_sparse_idx],
    trait = true_trait[very_sparse_idx],
    panel = "Muestreo muy escaso (cada 40 anos)"
  )

  preservation_panel <- "Muestreo denso, preservacion fosil pobre"
  dense_poor_pres_df <- tibble(
    time = true_time,
    trait = true_trait,
    panel = preservation_panel
  )

  fossil_idx <- sort(sample(seq_along(true_time), size = 18))
  fossil_df <- tibble(
    time = true_time[fossil_idx],
    trait = true_trait[fossil_idx],
    panel = preservation_panel
  )

  plot_df <- rbind(yearly_df, every20_df, every40_df, dense_poor_pres_df)
  standard_point_df <- plot_df[plot_df$panel != preservation_panel, ]

  ggplot(plot_df, aes(x = trait, y = time)) +
    geom_step(
      linewidth = 2.1,
      alpha = 0.28,
      color = "#1f4e79",
      direction = "hv"
    ) +
    geom_line(linewidth = 0.55, color = "#1f4e79") +
    geom_point(data = standard_point_df, size = 1.6, color = "#1f4e79") +
    geom_point(
      data = dense_poor_pres_df,
      shape = 21,
      size = 1.55,
      stroke = 0.5,
      color = "#6b7280",
      fill = "white"
    ) +
    geom_point(data = fossil_df, size = 1.8, color = "#1f4e79") +
    facet_wrap(~panel, nrow = 1, ncol = 4) +
    labs(
      title = "La misma trayectoria puede verse gradual o abrupta",
      subtitle = "Misma dinamica subyacente; cambia la resolucion temporal del muestreo o que fraccion del registro se preserva",
      x = "Rasgo (tamano relativo)",
      y = "Tiempo (anos)"
    ) +
    scale_y_continuous(limits = c(min(true_time), max(true_time))) +
    theme_diagram() +
    theme(strip.text = element_text(face = "bold", size = 10))
}

out_dir <- ensure_asset_dir("10-assets")

p_fad_lad <- make_first_last_appearance_plot()
p_count_rates <- make_event_count_rate_plot()

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
p_scale_sampling <- make_scale_sampling_contrast_plot()

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

ggsave(
  filename = file.path(out_dir, "trait_sampling_scale_millennia.png"),
  plot = p_scale_sampling,
  width = 8.8,
  height = 6.2,
  dpi = 300
)
