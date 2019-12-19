# Functions for mutating data

comprss <- function(tx) { 
  div <- findInterval(as.numeric(gsub("\\,", "", tx)), 
                      c(0, 1e3, 1e6, 1e9, 1e12) )
  paste(round( as.numeric(gsub("\\,","",tx))/10^(3*(div-1)), 2), 
        c("","K","M","G","T")[div] )}

get_shape_info <- function(id, df, df_descriptors){
  
  df <- df@data
  
  cols_occupations <- names(df) %>% .[grep("^(?=P_)(?=.*_Tot)(?!.*_\\d)(?!.*_NS)",., perl = TRUE)]
  cols_base <- c("sa2_maincode_2016", "Census_Name_2016", "Area sqkm")
  my_cols <- c(cols_base, cols_occupations)

  df <- df %>% 
    select(my_cols) %>%
    filter(sa2_maincode_2016 == id)
  
  dft <- t(df) %>% as.data.frame()
  dft$short <- rownames(dft)
  dft <- dft %>% left_join(df_descriptors %>% select(short, long) %>% unique())
  dft$long[is.na(dft$long)] <- dft$short[is.na(dft$long)]
  
  dft <- dft %>% 
    select(name = long, value = V1) %>%
    mutate(name = str_replace_all(name, "_", " ")) %>%
    mutate(name = str_replace_all(name, "Persons | Total", ""))  

  dft
}

iplookup <- function(x){
  cbind(x, rgeolocate::ip2location(ips = x$dest_ip, file = iplookup_db_file, fields = c('country_code','city','lat','long')))
}


subset_colclasses <- function(x, colclasses = c("numeric","character","factor", "integer","datetime")) {
  x[,sapply(x, function(vec, test) class(vec) %in% test, test=colclasses)]
}


simple_cap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}