#' Function trt4_meta
#'
#' This function returns a data frame with metadata about the 4th Region High Labor Court
#' decisions
#' @param livre word or expression to search for.
#' @param quote logical. If TRUE, default, teh expression will be searched between quotations.
#' @param inicio initial date of the decisions' period to be searched. 
#' @param fim  final date of the decisions' period to be searched.
#' @keywords judicial decisions, high courts, labor courts
#' @import httr
#' @import xml2
#' @import stringr
#' 
#' @return A data.frame with 7 variables with the information available.
#' @examples
#' df<-trt4_meta(livre="dano moral coletivo",inicio="19/05/1016", fim="19/05/2017) 

#' @export
trt4_meta<-function(livre,quote=T,inicio,fim){
  if(quote==TRUE){livre<-deparse(livre)}
  url1<-"http://www.trt4.jus.br/portal/portal/trt4/consultas/jurisprudencia/acordaos"
  handle(url1)
  url2<-"http://gsa5.trt4.jus.br/search?"
  l<-list(client = "jurisp",
          site = "jurisp_sp", 
          output = "xml_no_dtd", 
          proxystylesheet = "jurisp",
          ie = "UTF-8",
          oe = "UTF-8",
          requiredfields = "TIPO_DOCUMENTO:ACORDAO", 
          q = "",
          as_q = "inmeta:DATA_DOCUMENTO:2016-05-19..2017-05-19", 
          lr = "lang_pt",
          getfields = "*",
          proxyreload = "1",
          filter = "0", 
          partialfields = "action:add",
          sort = "date:D:L:d1",
          trt_q = "", 
          as_oq = "",
          as_eq = "",
          as_epq = "",
          trt_processo = "",
          trt_tipo = "ACORDAO", 
          trt_classe = "",
          trt_emissor = "",
          trt_sistema = "",
          trt_juiz = "", 
          trt_data_in = "",
          trt_data_fim = "")
  
  
  l[[9]]<-paste0("inmeta:DATA_DOCUMENTO:",as.Date(inicio,"%d/%m/%Y"),"..",as.Date(fim,"%d/%m/%Y"))
  l[[8]]<-livre
  l[[16]]<-livre
  l[[26]]<-inicio
  l[[27]]<-fim
  
  l<-paste0(names(l),"=",l)
  l<-toString(l)
  l<-str_replace_all(l,",[\\s+]","&")
  u<-paste0(url2,l)
  u<-str_replace_all(u,"\\s","+")
  
  a<- u %>% 
    GET() %>% 
    content("parsed")
  
  val<-a %>% 
    xml_find_first(xpath="//*[@class='resultsstatsdiv']/b[3]") %>% 
    xml_text() %>% 
    as.numeric() %>% 
    seq(0,.,10)
  
  paginas<-xml_find_first(a,"//td[@class='navigationbartd']/a/@href") %>%
    xml_text()
  
  h<-str_replace(paginas,"search\\?","")
  h<-paste0("http://gsa5.trt4.jus.br/search?",h)
  
  
  df<-data.frame(stringsAsFactors = F)
  for ( i in seq(val)){
    tryCatch({
      
      k<-str_replace(h,"(\\=\\d+)$",paste0("=",val[i]))
      k<-GET(k)
      k<-content(k,"parsed")
      
      links<-k %>% 
        xml_find_all(xpath="//*/a[@class='resultlink']/@href") %>% 
        xml_text() %>% 
        str_trim() %>% 
        paste0("http://gsa5.trt4.jus.br/search",.)
      
      p<-k %>%
        xml_find_all(xpath=".//*/a[@class='resultlink']") %>% 
        xml_text() 
      processo<-  str_extract(p,"\\d.*(?=\\s)")
      classe<-str_extract(p,"(?<=\\d\\s\\()\\w*")
      
      data<- k %>% 
        xml_find_all(xpath="//*[@class='blanktable']") %>% 
        xml_text() %>% 
        str_extract("\\d.{9}")
      
      j<-k %>% 
        xml_find_all(xpath="/html/body/div[5]/div") %>% 
        xml_text()
      
      orgao<-j[seq(1,30,3)]
      relator<-j[seq(2,30,3)]
      ementa<-j[seq(3,30,3)]
      
      df1<-data.frame(processo,classe,data, orgao,relator,ementa,links, stringsAsFactors = F)
      df<-rbind(df,df1)
    }, error=function(m){
      m
    }, finally={
      next
    })
    
  }
  return(df)
}