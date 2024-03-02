# Codigo para extrair as infos das patentes do cultivarWeb.
# Paginas baixadas localmente (eram 5, mais facil do que explorar la cruzando captcha)


# https://sistemas.agricultura.gov.br/snpc/cultivarweb/cultivares_registradas.php?txt_ordem=&postado=1&acao=pesquisar


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
library(plyr)

library(dplyr)
library(ggplot2)
library(reshape2)
library(data.table)
library(xml2)
library(XML)
library(RCurl)
library(rlist)
library(rvest)
library(writexl)
library(RSelenium)



# function to get attribute on_click ####

get_on_click <- function(x){
  return(x$getElementAttribute("onclick"))
}

get_proxima_pagina <- function(x){
  return(x$getElementAttribute("title"))
}

# Open the server ####

rD <- rsDriver(port = 4567L,
               browser = c("chrome"),
               chromever = NULL
)
remDr <- rD[["client"]] 

remDr$navigate("https://sistemas.agricultura.gov.br/snpc/cultivarweb/cultivares_registradas.php?txt_ordem=&postado=1&acao=pesquisar")


# prepare the pages to be scraped ####
pre_links <- c()


# enter in the loop ####

for(i in 1:350){
  print(i)
elementos <- remDr$findElements(using = "xpath", value = "//a[@onclick]")
pre_links <- c(pre_links, unlist(sapply(elementos,get_on_click)))


botoes_naveg <- remDr$findElements(using = "class" , value = "botao_paginacao") %>% sapply(.,get_proxima_pagina) %>% unlist()

next_page <- remDr$findElements(using = "class" , value = "botao_paginacao")[[which(botoes_naveg == "Próxima Página")]]
next_page$clickElement()
Sys.sleep(20)

}


# close the server ####
remDr$close()
rD$server$stop()
save.image("image_links.RData")

# Connect to the remote driver

pre_links <- gsub("newWindow\\('","",pre_links)
pre_links <- gsub("',''\\)","",pre_links)

# This list presents every link to every variety #
links <- paste0("https://sistemas.agricultura.gov.br/snpc/cultivarweb/",pre_links)


# Now we have the links to every variety, we can start scraping the data from each one of them ####

output <- data.frame(links)
output$unstructured_table <- list(NA)

for(i in 1:nrow(output)){
  cat(i,"\r")
  link <- links[i]
  theurl <- getURL(link,.opts = list(ssl.verifypeer = FALSE) )
  tables <- readHTMLTable(theurl)
  tables <- list.clean(tables, fun = is.null, recursive = FALSE)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  main_table <- tables[[which.max(n.rows)]] %>% unlist() %>%  data.frame()
  output$unstructured_table[[i]] <- (main_table)
}

# We have a dataframe that has the links and a list of structured data frames with the info #


from_unstructured_to_data_frame_line <- function(x){
  
  x <- unlist(x)
  x <- x[3:length(x)]
  
  # Categorias basicas:
  # Cultivar, nome comum, nome cientifico,situacao, no registro, data do registro, mantenedor (requerente), descritores (isso é para jogar fora)
  
  # some cleaning
  x <- gsub("ENDEREÇO"," ENDEREÇO",x)
  x <- gsub("CEP: "," CEP: ",x)
  x <- gsub("FONE: "," FONE: ",x)
  
  
  # getting rid of the descritores ####
  x <- x[!grepl("DESCRITORES",x)]
  
  # making some alternative headings work 
  x[grepl("REGIÃO DE ADAPTAÇÃO",x)] <- "REGIÃO DE ADAPTAÇÃO:"   
  x[grepl("ARQUIVOS DIGITAIS",x)] <- "ARQUIVOS DIGITAIS:"
  x[grepl("NOVA DENOMINAÇÃO",x)] <- "NOVA DENOMINAÇÃO:"
  x[grepl("DATA DE ALTERAÇÃO",x)] <- "DATA DE ALTERAÇÃO:"
  
  x <- x[!is.na(x)]
  x <- gsub("\\t","",x)
  # line headings
  index_line_headings <- which(grepl("\\:$",x))
  index_line_headings <- c(1,index_line_headings,length(x)+1)
  out_table <- matrix(nrow = 2, ncol = length(index_line_headings)-1)
  for(i in 1:(length(index_line_headings)-1)){
    out_table[1,i] <- x[index_line_headings[i]]
    
    indexes_content <- (index_line_headings[i]+1):(index_line_headings[i+1]-1)
    out_table[2,i] <- paste0(x[indexes_content],collapse = " - ")
  }
  
  
  wide_m <- out_table %>% as.data.frame()
  colnames(wide_m) <- wide_m[1,]
  wide_m <- wide_m[2,]
  
  return(wide_m)
}

list_of_output <- lapply(output$unstructured_table,from_unstructured_to_data_frame_line)
output_cultivares <- do.call(rbind.fill,list_of_output)

write_xlsx(output_cultivares,"cultivares_cultivarWeb.xlsx" )

# oi ariel. vamos aprender git.



