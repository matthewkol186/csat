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
