# Register the data-masked names used by dplyr/ggplot2 non-standard
# evaluation, silencing the "no visible binding for global variable" NOTE
# from codetools ("checking R code for possible problems").
# Name list frozen from the win-builder check logs (2026-08-31; identical
# on R 4.6.1 release and R-devel). Base functions flagged by the same NOTE
# (ave, head, tail, setNames) are registered here too -- bulk alternative
# to per-file importFrom() tags.
utils::globalVariables(c(
  ".", ".ch", ".dir", ".files", ".fls", ".ovl", ".ps", ".wav", ".wl",
  "F1_score", "ID", "ave", "cum_mean_score", "detection_id", "fn", "fp",
  "head", "n_detections", "n_validated", "peak_score", "precision",
  "recall", "roi_end", "roi_label_lists", "roi_max_freq", "roi_min_freq",
  "roi_start", "score_vec", "sensitivity", "setNames", "soundscape",
  "soundscape_file", "soundscape_path", "specificity", "tail", "template",
  "template_name", "template_path", "time_vec", "tn", "tp", "validation",
  "validation_bin", "validation_note", "validation_order",
  "validation_subset", "validation_time", "x", "xmax", "xmin", "y",
  "ymax", "ymin"
))
