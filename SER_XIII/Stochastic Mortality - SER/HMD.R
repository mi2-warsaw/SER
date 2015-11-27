
# deaths data
deaths <- read.table("Deaths_1x1.txt",skip=1, header = T)
deaths <- rbind(deaths %>% select(-Male,-Total) %>% rename(dh = Female) %>% mutate(sex='F'),deaths %>% select(-Female,-Total) %>% rename(dh = Male) %>% mutate(sex='M')) %>% rbind(deaths %>% select(-Male,-Female)%>% rename(dh = Total)%>% mutate(sex='T'))
deaths$Age <- as.numeric(substr(as.character(deaths$Age),1,3))
deaths$Year <- as.character(deaths$Year)

colnames(PTTZ)[c(2,5,6,9)] <- c("Age","dh",'exp',"Year")
PTTZ <- PTTZ %>% mutate(sex = ifelse(sex == 1, "M",'F'))
deaths_all <- rbind_list(deaths %>% filter(Year < 2010),PTTZ %>% filter(sex == gender,Year >= 2010)) %>% filter(Year >= starting_year)

Dxt <- deaths_all %>% filter(sex == gender, Age <= 100) %>% select(-sex) %>% dcast(Age ~ Year,value.var='dh')
rownames(Dxt) <- Dxt$Age
Dxt <- Dxt %>% select(-Age)

# exposure data
exp_HMD <- read.table("Population.txt",skip=1, header = T,stringsAsFactors =F)
exp_HMD$Year <- as.character(exp_HMD$Year)
exp_HMD <- rbind(exp_HMD %>% select(-Male,-Total) %>% rename(exp = Female)%>% mutate(sex='F'),exp_HMD %>% select(-Female,-Total) %>% rename(exp = Male) %>% mutate(sex='M')) %>% rbind(exp_HMD %>% select(-Male,-Female)%>% rename(exp = Total)%>% mutate(sex='T'))
exp_HMD$Age <- as.numeric(as.character(exp_HMD$Age))

Ext_all <- rbind_list(exp_HMD %>% filter(sex == gender,Year < 2010),PTTZ %>% filter(sex == gender,Year >= 2010)) %>% filter(Year >= starting_year)

Ext <- Ext_all %>% filter(sex == gender, Age <= 100) %>% select(-sex) %>% dcast(Age ~ Year,value.var='exp',fun.aggregate= sum) 
rownames(Ext) <- Ext$Age
Ext <- Ext %>% select(-Age)
Ext <- Ext + 0.5 * Dxt

full_tables <- Ext_all %>% select(Year,Age,sex,exp) %>% left_join(deaths_all %>% select(Year,Age,sex,dh)) %>% 
  mutate(qx = dh/exp) %>% filter(sex != 'T') %>% filter(is.na(Age) == F, Age %in% c(0:100))


rm(deaths,deaths_all,exp_HMD,Ext_all)