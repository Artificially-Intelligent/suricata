# Functions for mutating data

predict_weather <- function(){
  temperature <- ts(frequency = 12, start = c(2020, 1),
                    data = c(7.0, 6.9, 9.5, 14.5, 18.2, 21.5, 
                             25.2, 26.5, 23.3, 18.3, 13.9, 9.6))
  rainfall <- ts(frequency = 12, start = c(2020, 1),
                 data = c(49.9, 71.5, 106.4, 129.2, 144.0, 176.0, 
                          135.6, 148.5, 216.4, 194.1, 95.6, 54.4))
  df <- cbind(rainfall, temperature)
}

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