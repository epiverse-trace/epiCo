#' Get ISCO-88 occupation labels from codes
#'
#' Function that translates a vector of ISCO-88 occupation codes into a vector of labels
#' @param isco_codes A numeric vector of ISCO-88 occupation codes (major, submajor, minor or unit)
#' @param output_level A string parameter that defines the level of the desired label (major, submajor, minor or unit)
#'
#' @return A string vector of ISCO-88 labels
#' @examples
#' estimate_incidenceRate(incidence, level = 1)
#' @export
get_occupationLabels <- function(isco_codes, output_level) {
  data(ISCO88_table)
  input_level <- ifelse(isco_codes == 0 | isco_codes == 110, "Armed Forces",
    ifelse(nchar(isco_codes) == 1, "major",
      ifelse(nchar(isco_codes) == 2, "sub_major",
        ifelse(nchar(isco_codes) == 3, "minor",
          ifelse(nchar(isco_codes) == 4, "unit",
            NA
          )
        )
      )
    )
  )
  tryCatch(
    {
      output_level_index <- as.numeric(sapply(output_level, switch,
        "major" = 1,
        "major_label" = 1,
        "sub_major" = 2,
        "sub_major_label" = 2,
        "minor" = 3,
        "minor_label" = 3,
        "unit" = 4,
        "unit_label" = 4,
        simplify = "array"
      ))
    },
    error = function(e) {
      stop(paste0("Output level does not exist, please check your input"), call. = FALSE)
    }
  )


  input_level_index <- sapply(input_level, switch,
    "major" = 1,
    "sub_major" = 2,
    "minor" = 3,
    "unit" = 4,
    "Armed Forces" = 0,
    simplify = "array"
  )

  ISCO88_labels <- as.list(input_level)
  for (i in seq(1, length(isco_codes)))
  {
    tryCatch(
      {
        isco_code <- isco_codes[i]
        if (isco_code == 0 | isco_code == 110) {
          ISCO88_labels[[i]] <- as.array(ISCO88_table[ISCO88_table$major == 0, output_level])
        } else if (input_level_index[i] < output_level_index) {
          index_start <- match(isco_code, ISCO88_table[, input_level[i]])
          n_match <- sum(ISCO88_table[, input_level[i]] == isco_code)
          index_end <- index_start + n_match - 1
          ISCO88_labels[[i]] <- as.array(ISCO88_table[index_start:index_end, output_level])
        } else {
          ISCO88_labels[i] <- ISCO88_table[which(ISCO88_table[input_level[i]] == isco_code)[1], output_level]
        }
      },
      error = function(e) {
        stop(paste0("Code ", isco_code, " does not exist, please check your input"), call. = FALSE)
      }
    )
  }
  return(ISCO88_labels)
}
