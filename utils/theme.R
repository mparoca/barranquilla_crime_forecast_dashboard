library(fresh)
# create the theme with national police colors
theme <- create_theme(
  adminlte_color(
    green = "#179C94",
    blue = "#002F46",
    red = " #ff2b2b",
    yellow = "#f9b414",
    fuchsia = "#ff5bf8",
    navy = "#374c92",
    purple = "#615cbf",
    maroon = "#FF5C26",
    light_blue = "#002F46"
  ),
  adminlte_sidebar(
    dark_bg = "#179C94",
    dark_hover_bg = "#106d67",
    dark_color = "#b9e1de", 
    #light_hover_bg  = "#002F46"
  ),
  adminlte_global(
    content_bg = "#ecf0f5"
  )
)

# create tribble for box global config
box_config <- tibble::tribble(
  ~background, ~labelStatus,
  "red", "warning",
  "purple", "success",
  "green", "primary",
  "yellow", "danger",
  "fuchsia", "info"
)

# box factory function
box_factory <- function(background, labelStatus) {
  box(
    title = "Cyberpunk Box", 
    collapsible = TRUE, 
    background = background,
    height = "200px",
    label = boxLabel(1, labelStatus)
  )
}

# pmap magic
boxes <- purrr::pmap(box_config, box_factory)


# Reactable theme
options(reactable.theme = reactableTheme(
  color = "#2c3e50",
  backgroundColor = "#FFFFFF",
  borderColor = "#002F46",
  stripedColor = "hsl(233, 12%, 22%)",
  highlightColor = "#ff8861",
  inputStyle = list(backgroundColor = "#ecf0f5"),
  selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
))
