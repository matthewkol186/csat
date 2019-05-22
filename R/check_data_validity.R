# Check whether the disease matches the drug administered.
checkDiseaseDrugMatch <- function(disease, drug) {
  match = switch(disease,
                 "Lymphatic Filariasis" = drug=="Ivermectin" | drug=="Albendazole" | drug=="ALB & IVM" | drug=="ALB & DEC" | drug=="ALB, DEC, & IVM",
                 "Onchocerciasis" = drug == "Ivermectin",
                 "Trachoma" = drug == "Azithromycin",
                 "STH" = drug=="Albendazole" | drug=="Mebendazole",
                 "Schistosomiasis" = drug=="Praziquantel"
  )
  return(match)
}




# Check whether the header exists in the data for each header name specified in the web tool
checkHeaderMatch <- function(column_header, data) {

  # we make all data column names and the header to lowercase for easy comparison
  data <- rename_all(data, tolower)
  column_header <- tolower(column_header)

  # Compare
  if (column_header %in% colnames(data)){
    TRUE
  }else{
    FALSE
  }
}



# Check whether all relevant header names are included using checkHeaderMatch function. If so, nothing will happen.
# If any header names are not included, we stop the program and return the error message with all missing headers.
checkUnmatchedHeaders <- function(data, implementation_unit_header, cluster_header, sex_header, age_header, offered_drug_header, swallowed_drug_header,
                                   reported_coverage_header) {


 # first, we create a tribble (equivalent to a tibble or dataframe) that contains each required header and the result from the
 # checkHeaderMatch test. This allows us to manipulate the data for future use.
  headerMatchResults <-
    tribble(
      ~header, ~result,
      "implementation unit", checkHeaderMatch(implementation_unit_header, data),
      "cluster", checkHeaderMatch(cluster_header, data),
      "sex", checkHeaderMatch(sex_header, data),
      "swallowed drug", checkHeaderMatch(swallowed_drug_header, data),
    )

  # if the user specifies one or more of the following data column in the web tool, we check to make sure the column exists in
  # the uploaded data
  if(!is.null(age_header)) {
    headerMatchResults <-
      headerMatchResults %>%
      add_row(header = "age", result = checkHeaderMatch(age_header, data))
  }

  if(!is.null(offered_drug_header)) {
    headerMatchResults <-
      headerMatchResults %>%
      add_row(header = "offered drug", result = checkHeaderMatch(offered_drug_header, data))
  }

  if(!is.null(reported_coverage_header)) {
    headerMatchResults <-
      headerMatchResults %>%
      add_row(header = "reported coverage", result = checkHeaderMatch(reported_coverage_header, data))
  }

  if(all(headerMatchResults$result == TRUE)) {
    TRUE#maybe we should do nothing here, idk
  }
  else  {

    # filter the tibble for those headers that were not found
    headerNotFound <-
      headerMatchResults %>%
      filter(!result)

    #pull the vector of header names
    vectorNoHeader <- pull(headerNotFound, header)

    #stop the program, and share the vector of header names without a match in the data with the user
    # stop("Please make sure that the column headers of the file match the names of the headers that
    #         you entered into the web tool. The following headers were not found: " + vectorNoHeader)
    return(vectorNoHeader)
  }
}

findColsPresent <- function(all_cols, cols_to_check, data) {
  cols_to_remove <- vector()
  for(cname in cols_to_check){
    if(!exists(cname, where = data)) {
      cols_to_remove <- append(cols_to_remove, cname)
    }
  }
  # remove the columns not present
  cols_present <- all_cols[! all_cols %in% cols_to_remove]
}
