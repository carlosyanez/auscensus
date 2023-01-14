test_that("test helpers", {

  attributes <- tribble(~Census_stat, ~ Group,
           "Age_years_60_males","60 year old male",
            "Age (Years): 60_males","60 year old male",
            "Age_years_60_males","60 year old female",
            "Age (Years): 60_females","60 year old female")

  attribute_tibble_to_list(attributes)



})
