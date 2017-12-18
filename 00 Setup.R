dir.create("data")
dir.create("plot")
dir.create("images")

custom_packages = c("tidyverse", "stringr", "ggthemr", "ggrepel", "hrbrthemes", "readxl")
new_packages = custom_packages[!(custom_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)>0){
  install.packages(new_packages, repos='https://cran.rstudio.com/')
}
