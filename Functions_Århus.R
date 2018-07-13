#######################################################################################################################
###         1) webSCRAPER # Århus
###
### 
#######################################################################################################################

###---------------------------------------------
###        1.1) define variables
###---------------------------------------------

# Definer de resterende css-selectors
css.addr      = ".text-md-left a"           #1 
# Ændret således at det passer til nyt css - element på hjemmeside
css.buysum    = "td:nth-child(2) h5"        #2
css.date      = "td:nth-child(3) h5"        #3
css.sqm_price = "td:nth-child(4) h5"        #4
css.rooms     = "td:nth-child(5) h5"        #5
css.type      = "td.qtipped"                #6
css.m2        = ".qtipped+ td h5"           #7
css.build     = "td:nth-child(8) h5"        #8
css.deduction = "td:nth-child(9)"           #9

# sammensætning af css selectors i en vector
css.list  = c(css.addr, css.buysum, css.date, css.sqm_price, css.rooms, css.type, css.m2, css.build, css.deduction)         
# Navnelisten og denne skal matche 1:1
#Definer en tilsvarende vektor med variabelnavne
N         = c(NA, "link", "address",  "buysum", "date", "sqm_price", "n_rooms", "type", "m2","build_year", "deduction")  




#Addresse data kan hentes fra følgende link
url = "http://dawa.aws.dk/adresser?format=csv&kommunekode=0751"



read.url <- function(url, ...){
  tmpFile <- tempfile()
  download.file(url, destfile = tmpFile, method = "auto", mode = "wb")
  url.data <- fread(tmpFile, ...)
  return(url.data)
}

addr <- read.url(url)




#Jeg er pricipielt ligeglad med hvilken etage at enheden ligger på, også om den ligger til højre eller venstre. 
#Derfor fjerne jeg disse oplysninger, sammen med en hel del andre. Det er addressepunktet, der er interessant

subaddr = unique(addr[c("vejnavn","husnr","wgs84koordinat_bredde", "wgs84koordinat_længde", "postnr",
                        "postnrnavn","nøjagtighed", "højde")])  


# Bind link part med et (alle) tal i sekvensen 1:M
list.updater = function(M){
  num         = 1:M
  link.list   = paste(link.part,num, sep="")
  return(link.list)
}


scraper.singlepage = function(link){
  #open html and read link  attribute from column 1
  data <- link %>% 
    read_html() 
  
  
  
  for(i in css.list){
    data_text = data  %>% 
      html_nodes(i)   %>% 
      html_text()
    if (i == css.addr) {
      data_att = data %>% 
        html_nodes(i) %>% 
        html_attr("href")
      frame = cbind(frame, as.list(data_att)) 
      
    }
    
    frame = cbind(frame, as.list(data_text)) 
  }
  
  colnames(frame) = N
  return(out = frame[,2:ncol(frame)])
  
}





### ********** 1.3.2) pagelooper ************

# pagelooper kører scraper.singlepage() på hver side fra S til M
pagelooper = function(S,m){
  s.page = as.list(NULL)
  for(i in S:m){
    t = as.numeric(Sys.time())
    pagedump = scraper.singlepage(link.list[i])
    s.page   = rbind(s.page, pagedump)
    print(paste("getting page",i,"took",as.numeric(Sys.time())-t , "seconds. Sleeping for 5 seconds before getting next page"))
    Sys.sleep(5)
  }
  print(paste("DONE with pages",S,"to",m))
  return(as.data.frame(s.page))
}


store.payload = function(data, n, inc){
  page = floor(nrow(data)/40)
  stop = floor(nrow(data)/40) + n
  print(paste("start page:",page))
  print(paste("stop page:", stop))
  
  while(page < stop){
    t = as.numeric(Sys.time())
    
    data = tryCatch(rbind(data, pagelooper(page + 1,  page + inc)),
                    error = function(e) {
                      message(paste('Error:',e))
                      return(data)
                    })
    
    page = floor(nrow(data)/40)
    if(stop - page < inc) {inc = stop - page} else{inc = inc}
    
    for(i in 1:(NROW(N)-1)){
      data[,i] = unlist(data[,i])
    }
    
    write.csv2(data, 'Århus.csv')
    Sys.sleep(10)
    
    print(paste("Finishing page", page, "of", stop, "total"))
    print(paste("estimated",(stop-page)*((as.numeric(Sys.time())-t)/inc) , "seconds remaining"))
  }
  print("FINALLY FINISHED")
  return(data)
}







cleaner = function(data){
  # unlist every column
  
  for(i in 1:(NROW(N)-1)){
    data[,i] = unlist(data[,i])
  }
  
  # replace - with NA's
  data[data == "-"] <- NA
  
  # fjern 1000-sepeartorer
  data$buysum = gsub("\\.","", data$buysum) %>% as.numeric
  data$sqm_price = gsub("\\.","", data$sqm_price) %>% as.numeric
  
  # konverter til numeric
  data[c("n_rooms","m2","build_year")] <- as.numeric(as.matrix(data[c("n_rooms","m2","build_year")]))
  
  # Opdel date i faktisk dato og handelstype
  data$tradetype = substr(data$date, 11,20)
  data$date = substr(data$date, 1,10)
  
  
  #convert date to actual date
  data$date = as.Date(data$date, "%d-%m-%Y")
  
  # clean all character columns of whitespace in begining and end of string
  data[,sapply(data, class) == 'character'] <- lapply(data[,sapply(data, class) == 'character'], trimws)
  
  data = data %>% separate(address, c("roadname", "area"),",")
  data$floor = with(data, ifelse(grepl(". ", data$area, fixed = TRUE) == TRUE, substr(data$area,2,2), NA))
  data$floor[data$floor == "S"] = 0
  data$floor = as.numeric(data$floor)
  
  #gen vejnavn & husnr vars
  data$vejnavn = gsub(" [^ ]*$", "", data$roadname)
  data$husnr = gsub(".* ", "", data$roadname)
  
  return(clean_data = data)
}


###---------------------------------------------
###        3.2) geodata.offline
###---------------------------------------------


# left join the scraped data info from danish address registries  
geodata.offline = function(data){
  out = left_join(data, subaddr, by = c("vejnavn", "husnr"))
  #rename columns in output
  names(out)[names(out) == "wgs84koordinat_bredde"] = "lat" 
  names(out)[names(out) == "wgs84koordinat_længde"] = "lon" 
  names(out)[names(out) == "højde"] = "height"
  
  return(out)
  
}




