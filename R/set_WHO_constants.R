#set threshold value (desired level of population coverage) based on WHO recommendation

setDiseaseThreshold <- function(disease) {
  if (disease %in% c("Onchocerciasis", "Lymphatic Filariasis")) {
    thresh = 65
  } else if (disease == "Trachoma") {
    thresh = 80
  } else thresh = 75 #Value for STH and Schistosomiasis
}