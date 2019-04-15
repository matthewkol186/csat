

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

#set threshold value (desired level of population coverage) based on WHO recommendation
setDiseaseThreshold <- function(disease) {
  if (disease %in% c("Onchocerciasis", "Lymphatic Filariasis")) {
    thresh = 65
  } else if (disease == "Trachoma") {
    thresh = 80
  } else thresh = 75 #Value for STH and Schistosomiasis
}
