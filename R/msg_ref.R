# Message serial helper for the two Shiny apps (UIX-25)
#
# Julia port: not applicable (R-only Shiny app layer).
# R packages replaced: none.
#
# Every user-facing warning / error in launch_segmentation_app() (LSA) and
# launch_validation_app() (LVA) carries a stable serial of the form
# `MSG-###` so the user can look the message up in docs/messages-reference.md.
# The serial is a suffix, never a replacement for the human sentence.

#' Append a message serial suffix
#'
#' # R equivalent
#'   paste0(" [MSG-", code, "]")
#' # Arguments
#'   code  character — 3-digit serial, e.g. "001".
#' # Returns
#'   character — the bracketed serial suffix.
#' @noRd
.msg_ref <- function(code) paste0(" [MSG-", code, "]")
