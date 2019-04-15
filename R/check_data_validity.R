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
