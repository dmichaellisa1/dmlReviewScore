#' ReviewScore
#'
#' This function takes reviews with star ratings and creates a score based on the frequency and positivity of reviews for phrases. Phrases that are commonly found in positive reviews will receive high scores, while phrases that frequently appear in negative reviews will have low scores. This can be useful to gain insight on important information that reviewers are happy or dissatisfied with.
#' @param Reviews List of review text. You will likely want to remove punctuation, put everything in lowercase and other text cleaning procedures before using this function. This should usually be a column in a data frame.
#' @param Stars List of Star ratings associated with review. Should be on a scale of 1 to 5. This should usually be a column in a data frame.
#' @param PhraseSize Number stating how many words long you want the analyzed phrases to be. Defaults to 3.
#' @param nthread Number stating how many cores you want to run on. Defaults to 1. You may want to use more for speed. Recommended (parallel::detectCores() - 1)
#' @return Data frame containing score for phrase and how often the phrase occurred in reviews.
#' @import dplyr
#' @import parallel
#' @import doParallel
#' @import foreach
#' @import tidytext
#' @export
#' @examples
#' ReviewScore(df$ReviewText, df$StarRating, PhraseSize = 3, nthread = (parallel::detectCores() - 1))
ReviewScore <- function(Reviews, Stars, PhraseSize = 3, nthread = 1) {
  
  #n-thread
  cl <- parallel::makeCluster(nthread)
  doParallel::registerDoParallel(cl)
  
  #split into tokens and score it
  totalnvals <- foreach::foreach(i = 1:length(Reviews), .combine = "rbind", .packages=c('dplyr','tidytext')) %dopar% {
    
    tokens <- tibble(text = Reviews[i]) %>% 
      tidytext::unnest_tokens(tbl = ., output = word, input = text, token = "ngrams", n = PhraseSize)
    
    Value <- Stars[i] - 3
    
    Count <- 1
    
    RowNum <- i
    
    tokenvals <- cbind(tokens, Value, RowNum, Count)
    
    tokenvals <- unique(tokenvals)
    
    tokenvals
    
  }
  
  parallel::stopCluster(cl)
 
  #organize into df 
  NgramData <- totalnvals %>% 
    dplyr::group_by(word) %>% 
    dplyr::summarise(Score = sum(Value), Occurences = sum(Count)) %>% 
    dplyr::arrange(Score)
  
  NgramData
}