test_that("get data", {

  data_2021 <- data_census_info() |>
               dplyr::select(dplyr::any_of("path")) |>
               dplyr::pull()

  data_2021 <- any(stringr::str_detect(data_2021,"2021.zip"))

  ## Tests won't run without data, so they will be skip if there is no data

  if(data_2021){
  get_census_data(census_table = list_census_tables("04"),
                  geo_structure = "LGA")

  data <- get_census_data(census_table = list_census_tables("04"),
                          geo_structure = "LGA",
                          collect_data = TRUE)


  #summaries

  get_census_summary(table_number = "04",
                     attribute = list("60 year old male"=c("Age_years_60_males","Age (Years): 60_males"),
                                            "60 year old female"=c("Age_years_60_males","Age (Years): 60_females")),
                          geo_unit_names = c("Melbourne","Stonnington","Yarra"),
                          reference_total = list("Total"=c("Total_persons")),
                          geo_structure = "LGA")


  get_census_summary(table_number = "04",
                     attribute = list("60 year old male"=c("Age_years_60_males","Age (Years): 60_males"),
                                      "60 year old female"=c("Age_years_60_males","Age (Years): 60_females")),
                     geo_structure = "LGA")

  get_census_summary(table_number = "04",
                     attribute = list("60 year old male"=c("Age_years_60_males","Age (Years): 60_males"),
                                      "60 year old female"=c("Age_years_60_males","Age (Years): 60_females")),
                     geo_unit_names  = c("Melbourne","Stonnington","Yarra"),
                     reference_total = list("Total"=c("Total_persons")),
                     geo_structure = "LGA")
  }

})
