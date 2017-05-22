#' Function trt4_inteiro_teor
#'
#' This function returns a character text with the 4th Region High Labor Court decision.
#' The argument url can be retrieved using the function trt4_meta.
#' @param url url 
#' @keywords judicial decisions, high courts, labor courts
#' @import httr
#' @import boilerpipeR
#' 
#' @return A data.frame with 7 variables with the information available.
#' @examples
#' a<-trt4_inteiro_teor(url) 
#' @export
trt4_inteiro_teor<-function(url){
  a<-NULL
  for (i in seq(url)){
    b<-url[i] %>% 
      httr::GET() %>% 
      content("text") %>% 
      boilerpipeR::DefaultExtractor()
    a<-c(a,b)
  }
  return(a)
}