resilience_scores <- function(mh_items, stress_items){
  # Description:  Calculates pca-based resilient functioning scores adapted from van Harmelen et al. (2017), Psychological Medicine
  # Output:       for each participant: resilience score, mental health pca score (1st component), stress pca score (1st component), both pca and lm models 
  # Params:       mh_items: a matrix of mental health variables (e.g. questionnaire items)
  #               stress_items: a matrix of stress variables (e.g. questionnaire items)
  # Author:       Katja Schueler (kschueler@uni-mainz.de)
  # Last update:  2019-08-06
  # DOI:          DOI 10.17605/OSF.IO/S7U23
  
  # pca score of mental health indicators
  mh.pca <- princomp(mh_items, cor = TRUE, scores = TRUE)
  mh_pca_score <- data.frame(mh.pca$scores[,1]) # get scores of first component
  colnames(mh_pca_score) <- c("mh_pca_score")
  
  # pca score of stress indicators
  stress.pca <- princomp(stress_items, cor = TRUE, scores = TRUE)
  stress_pca_score <- data.frame(stress.pca$scores[,1]) # get scores of first component
  colnames(stress_pca_score) <- c("stress_pca_score")
  
  # linear regression:  stress score -->  mh scores
  resilience_data <- cbind(mh_pca_score, stress_pca_score)
  res.lm <- lm(mh_pca_score ~ stress_pca_score, data=resilience_data)
  rscore <- res.lm$residuals # save residuals as resilience
  #results <- cbind(rscore, mh_pca_score, stress_pca_score)
  results <- list("RScores"=rscore,
                  "Stress_PCAScores"=stress_pca_score,
                  "MH_PCAScores"=mh_pca_score,
                  "Stress_PCA"=stress.pca ,
                  "MH_PCA"=mh.pca ,
                  "LM"=res.lm
                  )
    
  return(results)
}
