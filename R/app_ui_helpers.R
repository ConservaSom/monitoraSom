# Shared UI helpers for the two Shiny apps (UIX-02 / UIX-03)
#
# Julia port: not applicable (R-only Shiny app layer).
#
# R packages replaced: none — thin constructors over shiny::actionButton.
#
# Extracted from launch_segmentation_app() and launch_validation_app() so the
# repeated confirm/danger buttons (colour, border, fixed width) and their Font
# Awesome icons live in one place. Both apps call these instead of hand-writing
# inline `style =` strings. Behaviour and size are unchanged from the original
# inline buttons (360 px confirm / 370 px danger, matching the 400 px sidebar).

#' Green confirmation button shared by both apps
#'
#' # R equivalent
#'   shiny::actionButton(id, label, icon = shiny::icon("check"),
#'     style = "color:#000;background:#33b733;border-color:#288d28;width:360px")
#' # Arguments
#'   input_id  character — Shiny input id.
#'   label     character — visible button label.
#' # Returns
#'   A shiny::actionButton tag.
#' @noRd
.btn_confirm <- function(input_id, label) {
  shiny::actionButton(
    input_id, label, icon = shiny::icon("check"),
    style = paste0(
      "color:#000;background:#33b733;border-color:#288d28;width:360px;"
    )
  )
}

#' Red danger button shared by both apps
#'
#' # R equivalent
#'   shiny::actionButton(id, label, icon = shiny::icon("right-from-bracket"),
#'     style = "color:#fff;background:#b73333;border-color:#8d2c2c;width:370px")
#' # Arguments
#'   input_id  character — Shiny input id.
#'   label     character — visible button label.
#' # Returns
#'   A shiny::actionButton tag.
#' @noRd
.btn_danger_wide <- function(input_id, label) {
  shiny::actionButton(
    input_id, label, icon = shiny::icon("right-from-bracket"),
    style = paste0(
      "color:#fff;background:#b73333;border-color:#8d2c2c;width:370px;"
    )
  )
}
