

library(tidyods)
library(tidyverse)
library(readxl)
library(fs)
library(xlsx)
library(lubridate)

setwd("C:/Users/fintan.smith/Desktop/Prisons/raw")

folder_path <- 'C:/Users/fintan.smith/Desktop/Prisons/raw'
file_paths_ods <- dir_ls(folder_path, regexp = ".ods")
file_paths_xl <- dir_ls(folder_path, regexp = ".xls")


dataFiles_ODS <- lapply(file_paths_ods, read_ods_sheet)

dataFiles<- lapply(file_paths_xl,read_xls)




##### 2012 to mid 2018 - xls format

merged_to_mid_18<-bind_rows(dataFiles,.id = "source")%>%
  filter(!str_detect(source,"june10"))%>%
  filter(!is.na(...2))


# CLEAN SOURCE STRINGS
merged_to_mid_18$source <- gsub("C:/Users/fintan.smith/Desktop/Prisons/raw/prison-pop-", "", merged_to_mid_18$source)
merged_to_mid_18$source <- gsub("ulation", "", merged_to_mid_18$source)
merged_to_mid_18$source <- gsub(".xls", "", merged_to_mid_18$source) 
merged_to_mid_18$source <- gsub("C:/Users/fintan.smith/Desktop/Prisons/raw/prison-pop-", "", merged_to_mid_18$source) 
merged_to_mid_18$source <- gsub("C:/Users/fintan.smith/Desktop/Prisons/raw/prison_pop_", "", merged_to_mid_18$source)
merged_to_mid_18$source <- gsub("C:/Users/fintan.smith/Desktop/Prisons/raw/pop-figures-", "", merged_to_mid_18$source)
merged_to_mid_18$source <- gsub("C:/Users/fintan.smith/Desktop/Prisons/raw/pop-bulletin-", "", merged_to_mid_18$source)
merged_to_mid_18$source <- gsub("C:/Users/fintan.smith/Desktop/Prisons/raw/prison-popo-", "", merged_to_mid_18$source)
merged_to_mid_18$source <- gsub("C:/Users/fintan.smith/Desktop/Prisons/raw/prison-popualtion-figures-", "", merged_to_mid_18$source)
merged_to_mid_18$source <- gsub("C:/Users/fintan.smith/Desktop/Prisons/raw/weekly-bulletin-", "", merged_to_mid_18$source)
merged_to_mid_18$source <- gsub("C:/Users/fintan.smith/Desktop/Prisons/raw/prison-report-", "", merged_to_mid_18$source)
merged_to_mid_18$source <- gsub("figures-", "", merged_to_mid_18$source)

# REMOVE EVERY OTHER GROUP OF 6 ROWS TO CLEAR OUT LAST WEEK VALUES
group_size <- 6
merged_to_mid_18$...2 <- ifelse((1:nrow(merged_to_mid_18) %/% group_size) %% 2 == 1, NA, merged_to_mid_18$...2)


merged_to_mid_18<- merged_to_mid_18%>%
  mutate(value=str_extract(...2,"[0-9]*$"))%>%
  mutate(value=as.numeric(value))%>%
  mutate(metric=merged_to_mid_18$...1)%>%
  filter(!is.na(value))
  
merged_to_mid_18 <- dplyr::select(merged_to_mid_18,source,value,metric)






#################### MID 2018 - mid 2021...

merged_mid_2018_2021<- bind_rows(dataFiles_ODS,.id = "source")%>%
  filter(str_detect(source,"2017")|str_detect(source, "2019")| str_detect(source,"2018")|str_detect(source,"2020")|str_detect(source,"july-2021")|
         str_detect(source,"jun-2021")|str_detect(source,"may-2021")|
         str_detect(source,"apr-2021")|str_detect(source,"mar-2021")|str_detect(source,"feb-2021")| str_detect(source,"jan-2021"))%>%
  filter(!is.na(x4))


merged_mid_2018_2021$source <- gsub("C:/Users/fintan.smith/Desktop/Prisons/raw/prison-pop-", "", merged_mid_2018_2021$source)
merged_mid_2018_2021$source <- gsub("ulation", "", merged_mid_2018_2021$source)
merged_mid_2018_2021$source <- gsub(".ods", "", merged_mid_2018_2021$source) 
merged_mid_2018_2021$source <- gsub("C:/Users/fintan.smith/Desktop/Prisons/raw/prison-pop-", "", merged_mid_2018_2021$source) 
merged_mid_2018_2021$source <- gsub("C:/Users/fintan.smith/Desktop/Prisons/raw/prison_pop_", "", merged_mid_2018_2021$source)
merged_mid_2018_2021$source <- gsub("C:/Users/fintan.smith/Desktop/Prisons/raw/pop-figures-", "", merged_mid_2018_2021$source)
merged_mid_2018_2021$source <- gsub("C:/Users/fintan.smith/Desktop/Prisons/raw/pop-bulletin-", "", merged_mid_2018_2021$source)
merged_mid_2018_2021$source <- gsub("C:/Users/fintan.smith/Desktop/Prisons/raw/prison-popo-", "", merged_mid_2018_2021$source)
merged_mid_2018_2021$source <- gsub("C:/Users/fintan.smith/Desktop/Prisons/raw/prison-popualtion-figures-", "", merged_mid_2018_2021$source)
merged_mid_2018_2021$source <- gsub("C:/Users/fintan.smith/Desktop/Prisons/raw/weekly-bulletin-", "", merged_mid_2018_2021$source)
merged_mid_2018_2021$source <- gsub("C:/Users/fintan.smith/Desktop/Prisons/raw/prison-report-", "", merged_mid_2018_2021$source)
merged_mid_2018_2021$source <- gsub("figures-", "", merged_mid_2018_2021$source)


group_size <- 6
merged_mid_2018_2021$x4 <- ifelse((1:nrow(merged_mid_2018_2021) %/% group_size) %% 2 == 1, NA, merged_mid_2018_2021$x4)


merged_mid_2018_2021<- merged_mid_2018_2021%>%
  mutate(value=str_extract(x4,"[0-9]*$"))%>%
  mutate(value=as.numeric(value))%>%
  mutate(metric=merged_mid_2018_2021$x3)%>%
  filter(!is.na(value))

merged_mid_2018_2021 <- dplyr::select(merged_mid_2018_2021,source,value,metric)

################## MID 2021 - 2023


merged_mid_2021_2023<- bind_rows(dataFiles_ODS,.id = "source")%>%
filter(str_detect(source,"aug-2021") |str_detect(source,"sept-2021")|
                       str_detect(source,"oct-2021")|str_detect(source,"nov-2021")|
                         str_detect(source,"dec-2021")|str_detect(source,"2022")|str_detect(source,"2023"))



merged_mid_2021_2023$source <- gsub("C:/Users/fintan.smith/Desktop/Prisons/raw/prison-pop-", "", merged_mid_2021_2023$source)
merged_mid_2021_2023$source <- gsub("ulation", "", merged_mid_2021_2023$source)
merged_mid_2021_2023$source <- gsub(".ods", "", merged_mid_2021_2023$source) 
merged_mid_2021_2023$source <- gsub("C:/Users/fintan.smith/Desktop/Prisons/raw/prison-pop-", "", merged_mid_2021_2023$source) 
merged_mid_2021_2023$source <- gsub("C:/Users/fintan.smith/Desktop/Prisons/raw/prison_pop_", "", merged_mid_2021_2023$source) 

merged_mid_2021_2023<- merged_mid_2021_2023%>%
  filter(is.na(x4))%>%
  mutate(value=x6)%>%
  mutate(metric=x3)%>%
  filter(!is.na(value))%>%
  filter(!is.na(metric))%>%
  select(source,value, metric)


  


###################################### BINDED DATA FRAMES

complete_df<- rbind(merged_to_mid_18,merged_mid_2018_2021,merged_mid_2021_2023)



complete_df$source <- gsub("prison-popoulation-", "", complete_df$source)
complete_df$source <- gsub("prison popualtion", "", complete_df$source)

complete_df_cleaning <- complete_df%>%
  mutate(source=trimws(source))%>%
  mutate(source=textclean::replace_non_ascii(source))%>%
  mutate(date= tryCatch(as.Date(source, format = c("%d-%B-%Y", "%d-%m-%Y", "%d-%b-%Y"," %d-%b-%Y", "%d%m%y","%d %m %y"), error = function(e) NA)))%>%
  mutate(invalid_date = is.na(date))

write.csv(complete_df,"complete_CSV.csv")

# AS.DATE WAS STRUGGLING TO READ DATES FOR MANY OF THESE... I THINK BECAUSE OF HIDDEN CHARACTERS IN SOURCE COLUMN
# I MESSED ABOUT FOR A WHILE TRYING TO FIX THIS, BUT WRITING TO A CSV SOLVED A LOT OF THE ISSUES THE REST OF WHICH
# I JUST SORTED MANUALLY.




