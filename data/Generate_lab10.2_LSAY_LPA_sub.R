
# create new LSAY subset with 4 test scores

lsay_data <- read_spss(here("data", "LSAY_labs.sav")) %>%                    #  
  select(RURAL, GENDER, FATHED, MOTHED,
         AMTHIMP, ASCIIMP, APHYIMP, ABIOIMP) %>%                                    
  clean_names() %>%                                                          #  
  rename(mth_scor = amthimp ,                                                #
         sci_scor = asciimp ,                                                #
         phy_scor = aphyimp ,                                                #
         bio_scor = abioimp ) %>%                                            #     
  replace_with_na_all(condition = ~.x == 9999.00)                            #   

########################

# View metadeta 
sjPlot::view_df(lsay_data)


# Write a `CSV` file
write_csv(lsay_data, here("data", "lsay_lab10.2_lpa.csv"))
