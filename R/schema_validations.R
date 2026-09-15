#' Standard validations schema (VBO-05; push plan §4)
#'
#' @description Single source of truth for the `validations` table written by
#'   [validate_by_overlap()]. It is the standard detections schema
#'   ([.detection_schema_spec()]) **plus** the four validation columns, so a
#'   validated TP/FP detection persists with its full identity (and durable
#'   `detection_id` primary key) alongside the verdict. Mirrors
#'   [_schema_detections.R] and reuses the generic `.typed_na()` / `.coerce_type()`
#'   helpers.
#'
#'   **FN rows are not stored here** (they have no `detection_id`): they live only
#'   in the wide returned frame and the deprecated CSV (VBO-05). Folding
#'   validation into the detections table itself is deferred (VBO-10).
#'
#' @keywords internal
#' @noRd

.validation_extra_cols <- function() {
  c(
    validation_user  = "character",
    validation_time  = "character",
    validation       = "character",  # TP | FP (FN never persisted here)
    validation_note  = "character",
    # LVA-113: the order in which the user validated each detection (monotonic,
    # persisted across sessions) + a fingerprint of the validation-subset settings
    # active at that time. Feed the LVA-110 score-stabilization plot.
    validation_order = "integer",
    validation_subset = "character"
  )
}

.validation_schema_spec <- function() {
  c(.detection_schema_spec(), .validation_extra_cols())
}

#' Empty (or n-row NA) validations frame in standard schema/order.
#' @param n number of NA-filled rows to create (default 0).
.schema_validations <- function(n = 0L) {
  spec <- .validation_schema_spec()
  cols <- lapply(spec, function(type) rep(.typed_na(type), n))
  df <- as.data.frame(cols, stringsAsFactors = FALSE)
  names(df) <- names(spec)
  df
}

#' Coerce an arbitrary frame to the standard validations schema/types.
#' Missing standard columns are added as typed NA; extras are dropped.
#' @param df a data.frame to coerce.
.coerce_validations <- function(df) {
  spec <- .validation_schema_spec()
  if (is.null(df) || nrow(df) == 0L) return(.schema_validations(0L))
  out <- .schema_validations(nrow(df))
  for (col in names(spec)) {
    if (col %in% names(df)) out[[col]] <- .coerce_type(df[[col]], spec[[col]])
  }
  out
}
