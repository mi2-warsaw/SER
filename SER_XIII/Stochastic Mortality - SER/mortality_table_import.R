

# Polskie Tablice Trwania Życia (PTTŻ) 1990-2014 (GUS)
PTTZ <- data.frame()
for(i in 1990:2014){
  given_year <- read_excel('lifetables1990-2014.xlsx',sheet=as.character(i), skip = if(i %in% c(1995:2001)){2}else{3}) %>% mutate(year = as.character(i))
  colnames(given_year)[c(1,2)] <- c('sex','age')
  PTTZ <- rbind(PTTZ,given_year)
}
PTTZ <- PTTZ %>% filter(is.na(sex)==F)
rm(given_year)