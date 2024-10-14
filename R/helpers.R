#' Helper function to convert attributes to list
#' @description Little helper function that converts tibble into a list with vectors, which is the
#' expected attributes input for get_census_summary()
#' @param df tibble/data.frame. First column is the original value, the second the new label
#' @param original name of original attribute
#' @param new new naming
#' @importFrom dplyr distinct filter pull select any_of if_any
#' @return list object
#' @export
#' @keywords helpers
#' @examples \dontrun{
#' attributes <- tribble(~Census_stat, ~ Group,
#'         "Age_years_60_males","60 year old male",
#'          "Age (Years): 60_males","60 year old male",
#'          "Age_years_60_males","60 year old female",
#'          "Age (Years): 60_females","60 year old female")

#'          attribute_tibble_to_list(attributes)
#'
#' }
attribute_tibble_to_list <- function(df,
                                     original = colnames(df)[1],
                                     new     = colnames(df)[2]){

  levels <- df  |>
            select(any_of(new)) |>
            distinct() |>
            pull()

  results <- list()
  i <-1
  for(level in levels){

    results[[i]]      <-  df |>
                          filter(if_any(any_of(new), ~ .x==level))  |>
                          select(any_of(original)) |>
                          distinct() |>
                          pull()

    names(results)[i] <-  level
    i <- i +1


  }
  return(results)
}

#' Helper function to convert attributes to list
#' @description Little helper function that converts tibble into a list with vectors, which is the
#' expected attributes input for get_census_summary()
#' @param df data frame
#' @param key_col name of the column containing the "Total Label"
#' @param value_col   name of the column containing values
#' @param key_value total label
#' @param percentage_scale 1 if percentage to be presented in scale 0-1, or 100 to be shown as 0%-100%
#' @importFrom dplyr filter if_any any_of select rename left_join mutate case_when
#' @importFrom naniar replace_with_na
#' @importFrom rlang .data
#' @return list object
#' @keywords helpers
#' @export
calculate_percentage <- function(df,key_col,value_col,key_value="Total",percentage_scale=1){

  if(!(percentage_scale %in% c(1,100))) stop("percentage scale must by 1 or 100 (as in 100%)")

  totals <- df |>
    filter(if_any(any_of(c(key_col)), ~ .x == key_value)) |>
    select(-any_of(c(key_col))) |>
    rename("Total"=any_of(value_col))

  other_cols <- colnames(df)
  other_cols <- other_cols[!(other_cols %in% c(key_col,value_col))]

  df <- df |>
    filter(if_any(any_of(c(key_col)), ~ .x != key_value))  |>
    left_join(totals,by=other_cols)


  df <- df |>
    mutate(Percentage = case_when(
      is.na(.data$Value) ~ NA,
      is.na(.data$Total) ~ -1,
      .data$Total == 0   ~ -1,
      TRUE ~    percentage_scale*.data$Value/.data$Total
    )) |>
    replace_with_na(list("Percentage"=-1))


  return(df)

}



