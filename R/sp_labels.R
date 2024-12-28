#' Spectrogram Labels
#'
#' A data frame containing spectrogram labels and acoustic measurements for
#' soundscape recordings. The lists in the `sp_labels` data frame are used to
#' label the species in the spectrograms within the segmentation shiny app. When
#' lauching the app, a xlsx spreadsheet is created within the working directory,
#' in which users can add custom species labels as new columns, with the first
#' cell as the title of that respective list.
#'
#' @format ## `sp_labels`
#' A data frame with the following columns:
#' \describe{
#'   \item{`CBRO-2021 (Birds - Brazil)`}{List of bird species from the CBRO-2021
#'   dataset, from Brazil.}
#'   \item{Aves Argentinas (Birds - Argentina)}{List of bird species from the
#'   Aves Argentinas dataset, from Argentina.}
#'   \item{SBH-2021 (Amphibians - Brazil)}{List of amphibian species from the
#'   SBH-2021 dataset, from Brazil.}
#'   \item{CLMB-2020 (Bats - Brazil)}{List of bat species from the CLMB-2020
#'   dataset, from Brazil.}
#' }
"sp_labels"
