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

resolve_project_root <- function(fallback_to_script_parent = FALSE) {
  script_dir <- get_script_dir()
  project_root <- find_project_root(c(script_dir, getwd()))

  if (is.null(project_root) && fallback_to_script_parent) {
    project_root <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)
  }

  project_root
}

ensure_asset_dir <- function(
  asset_dir_name,
  fallback_to_script_parent = FALSE
) {
  project_root <- resolve_project_root(
    fallback_to_script_parent = fallback_to_script_parent
  )

  if (is.null(project_root)) {
    stop("Could not find project root (_quarto.yml)")
  }

  out_dir <- file.path(project_root, "lectures", asset_dir_name)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_dir
}

theme_diagram_base <- function(
  base_size = 11,
  title_size = 12,
  subtitle_size = 10,
  subtitle_lineheight = NULL,
  axis_title_size = 10,
  axis_text_size = 9,
  strip_text_size = 9
) {
  theme_args <- list(
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      face = "bold",
      size = title_size
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    axis.title = ggplot2::element_text(size = axis_title_size),
    axis.text = ggplot2::element_text(size = axis_text_size)
  )

  if (!is.null(subtitle_size)) {
    theme_args$plot.subtitle <- ggplot2::element_text(
      hjust = 0.5,
      size = subtitle_size,
      lineheight = if (is.null(subtitle_lineheight)) {
        0.9
      } else {
        subtitle_lineheight
      }
    )
  }

  if (!is.null(strip_text_size)) {
    theme_args$strip.text <- ggplot2::element_text(size = strip_text_size)
  }

  ggplot2::theme_bw(base_size = base_size) +
    do.call(ggplot2::theme, theme_args)
}

theme_diagram <- function() {
  theme_diagram_base()
}
