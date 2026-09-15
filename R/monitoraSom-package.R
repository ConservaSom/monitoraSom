#' monitoraSom: automated sound detection in passive acoustic monitoring
#'
#' @description
#' monitoraSom matches animal-vocalization templates against long soundscape
#' recordings, segments regions of interest (ROIs), validates detections, and
#' diagnoses template performance. The workflow is: set up a workspace
#' ([set_workspace()]), segment ROIs ([launch_segmentation_app()]), cut
#' templates ([export_templates()]), read inputs
#' ([fetch_soundscape_metadata()], [fetch_template_metadata()]), build the
#' search grid ([fetch_match_grid()]), match ([run_matching()] /
#' [template_matching()]), validate ([validate_by_overlap()]) and diagnose
#' ([diagnostic_validations()]).
#'
#' @section What changed in the refactored version (before vs after 1.2.0):
#' The refactored package (the analysis flow from before monitoraSom 1.2.0
#' onward) keeps the same steps but reorganized the internals. Compatibility
#' shims keep old scripts running where it matters: [export_roi_cuts()] is a
#' deprecated alias of [export_templates()] (the `roi_cuts/` folder became
#' `templates/`, CRAN item 3); old folder names (`match_grid_metadata`,
#' `validation_outputs`, `validation_diagnostics`) became `grids`,
#' `validations` and `diagnostics` ([set_workspace()] still documents the
#' mapping); legacy CSV detections are still read, with a deprecation warning,
#' and the migration path to the signals store is documented in
#' [launch_validation_app()]. ROIs and detections now live together in one
#' `signals` database; the promotion of a validated detection to a ROI is
#' automatic in [validate_by_overlap()] (opt-out with `promote_to_roi =
#' FALSE`).
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL

## usethis namespace: start
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL
