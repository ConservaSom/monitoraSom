#' Default-path write guard shared by every writing function (FEAT-07)
#'
#' @description Warn-only gate implementing the contract fixed by the user on
#'   2026-08-05: never refuse automatically; the warning must be clear; the user
#'   confirms before proceeding. It stops the package from silently scattering
#'   stores (DuckDB, CSVs, exports) over R's working directory when no explicit
#'   workspace was given.
#'
#'   Reuses the workspace-marker infrastructure of [set_workspace()]
#'   (`monitoraSom.proj`, written by `.write_project_marker()`): a write target
#'   is silent when the marker sits at the target directory or any ancestor —
#'   i.e. anywhere inside a monitoraSom workspace.
#'
#'   Behaviour matrix (per resolved target directory):
#'
#'   | Case | Behaviour |
#'   |---|---|
#'   | Inside the R session `tempdir()` subtree | silent (deliberate scratch) |
#'   | Marker found at the directory or an ancestor | silent |
#'   | Target resolved by a caller **default** | confirm-grade warning `[MSG-021]` |
#'   | Explicit target that **is** the working/home/temp directory | confirm-grade `[MSG-021]` |
#'   | Any other unmarked explicit target | one `[MSG-022]` warning per directory per session |
#'
#'   Confirm-grade: interactive sessions must answer the prompt — declining
#'   stops **before anything is written** (this is the user refusing, not the
#'   gate). Non-interactive sessions (Rscript, testthat) warn and proceed.
#'   `options(monitoraSom.path_guard = FALSE)` silences the gate entirely.
#'
#'   Messages carry stable serials documented in `docs/messages-reference.md`
#'   (UIX-25); the serials are inlined (not via `_msg_ref.R`) so this helper
#'   stays sourceable with no other dependency.
#'
#' @return The gate itself returns nothing; see [.require_explicit_workspace()].
#' @keywords internal
#' @noRd
NULL

# Warned-target registry: one warning per directory per R session.
.ws_guard_state <- new.env(parent = emptyenv())

#' TRUE when `dir` or any ancestor carries the monitoraSom.proj marker.
#'
#' Single upward walk from the resolved directory; stops at the filesystem
#' root. Shared by the FEAT-07 gate so a write anywhere inside a workspace
#' (nested tier directory included) stays silent.
#'
#' # R equivalent
#'   any(file.exists(file.path(ancestor_dirs(dir), "monitoraSom.proj")))
#' # Arguments
#'   dir  character — absolute directory path.
#' # Returns
#'   logical — TRUE when a marker was found.
#' @noRd
.ws_has_marker <- function(dir) {
  d <- normalizePath(dir, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(d, "monitoraSom.proj"))) return(TRUE)
    parent <- dirname(d)
    if (parent == d) return(FALSE)
    d <- parent
  }
}

#' Warn-only gate before writing outside a marked workspace (FEAT-07).
#'
#' # R equivalent
#'   none — new shared guard called by every writing function.
#'
#' @param path Single character: write target (file or directory).
#' @param default_target Logical: TRUE when `path` came from a caller default
#'   (the user passed nothing).
#' @param label Single character: argument name shown in the message.
#' @param caller Single character: function name shown in the message.
#' @return Invisible logical: TRUE when a warning was emitted.
#' # Example
#'   .require_explicit_workspace(tempfile(), caller = "my_writer")
#'   # silent: the tempdir() subtree is exempt
.require_explicit_workspace <- function(path,
                                        default_target = FALSE,
                                        label = "path",
                                        caller = "this function") {
  if (identical(getOption("monitoraSom.path_guard"), FALSE)) {
    return(invisible(FALSE))
  }
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    return(invisible(FALSE))  # non-path targets are validated by their callers
  }
  p <- path.expand(path)
  # A directory target (existing or trailing-slash) is guarded as itself; a
  # file target is guarded by its directory. A not-yet-existing bare name is
  # treated as a file, so the (existing) parent directory is checked.
  target_dir <- if (dir.exists(p) || grepl("/$", p)) p else dirname(p)
  target <- normalizePath(target_dir, winslash = "/", mustWork = FALSE)
  temp_root <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
  if (target == temp_root || startsWith(target, paste0(temp_root, "/"))) {
    return(invisible(FALSE))  # session scratch is always deliberate
  }
  if (.ws_has_marker(target)) {
    return(invisible(FALSE))  # inside a monitoraSom workspace
  }
  zones <- c(
    "working directory" = normalizePath(getwd(), winslash = "/",
                                        mustWork = FALSE),
    "home directory"    = normalizePath(path.expand("~"), winslash = "/",
                                        mustWork = FALSE),
    "temp directory"    = temp_root
  )
  zone_hit <- match(target, zones)
  confirm_grade <- isTRUE(default_target) || !is.na(zone_hit)
  key <- paste0(caller, "|", target)
  if (exists(key, envir = .ws_guard_state, inherits = FALSE)) {
    return(invisible(TRUE))  # one warning per directory per session
  }
  if (confirm_grade) {
    zone_nm <- if (is.na(zone_hit)) {
      "an unmarked default location"
    } else {
      names(zones)[[zone_hit]]
    }
    warning(caller, ": the '", label, "' target resolves to ", zone_nm,
            " ('", target, "'). That is not a monitoraSom workspace: no ",
            "monitoraSom.proj marker was found there. Writing can scatter ",
            "files outside a project. [MSG-021]", call. = FALSE)
    if (interactive()) {
      ans <- readline(paste0(caller, ": proceed and write under '",
                             target, "'? [y/N] "))
      if (!identical(tolower(ans), "y")) {
        stop("Aborted before any write. Pass an explicit workspace path or ",
             "run set_workspace() first. [MSG-021]", call. = FALSE)
      }
    }
  } else {
    warning(caller, ": writing to '", target, "'. The directory carries no ",
            "monitoraSom.proj marker: it is not a monitoraSom workspace. ",
            "This warning is issued once per directory per session. ",
            "[MSG-022]", call. = FALSE)
  }
  assign(key, TRUE, envir = .ws_guard_state)
  invisible(TRUE)
}
