# Summarize the age of fish in the hake fleets for Sean Matson 8-24-2022


rmis.base.dir <- "/Users/ole.shelton/GitHub/rmis-data"
base.dir <- "/Users/ole.shelton/GitHub"

### CWT releases
# go to rmis git repo to get release data.
all.release.dat	<-	read.csv(paste0(rmis.base.dir,"/data/chinook/all_releases.csv"))

# Read in all CWT fish recovered from the ashop fleets

# These are needed for the end of this script to match up with ASHOP and SHORESIDE DATA
shoreside_chinook = read.csv(paste0(base.dir,"/Orca_Salmon_DATA/Recoveries/Shoreside_CWT/Final_Dataset/shoreside_chinook_tagcode_FTID.csv"))
ashop_all_salmon  = read.csv(paste0(base.dir,"/Orca_Salmon_DATA/Hake Trawl/ASHOP/final_datasets/ashop_chinook.csv"))
ashop_chinook_CWT1 = read.csv(paste0(base.dir,"/Orca_Salmon_DATA/Recoveries/ASHOP/snoutbase/A-SHOP_Snoutbase_122118.csv"))
ashop_chinook_CWT2 = read.csv(paste0(base.dir,"/Orca_Salmon_DATA/Recoveries/ASHOP/snoutbase/A-SHOP_ChinookBio_2012-2021_Matson_061622 for Ole.csv"))
ashop_sample_frac = read.csv(paste0(base.dir,"/Orca_Salmon_DATA/Hake Trawl/ASHOP/final_datasets/ashop_sample_fraction.csv"))

# pad front zero in front of cwt that are only 5 characters long.
shoreside_chinook <- shoreside_chinook %>% 
                      mutate(Tag.Code = as.character(Tag.Code)) %>%
                      mutate(tag_code = ifelse(nchar(Tag.Code)==5,paste0("0",Tag.Code),Tag.Code))

# Merge the two ashop data frames
ashop_chinook_CWT1 <- ashop_chinook_CWT1 %>% dplyr::select(SalmonSnoutNumber,
                                                           length.cm = Ln ,
                                                           weight.kg = Wt,
                                                           sex = Sex,
                                                           CWTCode,
                                                           Date,
                                                           year = Year) %>%
                                            mutate(SalmonSnoutNumber = as.character(SalmonSnoutNumber),
                                                              length.cm = as.numeric(length.cm),
                                                              weight.kg = as.numeric(weight.kg))

ashop_chinook_CWT2 <- ashop_chinook_CWT2 %>% dplyr::select(SalmonSnoutNumber,
                                                         length.cm=Length..cm.,
                                                         weight.kg = Weight..kg.,
                                                         sex =Sex,
                                                         CWTCode,
                                                         Date,
                                                         year=Year) %>%
                                        mutate(SalmonSnoutNumber = as.character(SalmonSnoutNumber),
                                               length.cm = as.numeric(length.cm),
                                               CWTCode = as.character(CWTCode))

ashop_chinook_CWT = bind_rows(ashop_chinook_CWT1 %>% filter(year <=2011),ashop_chinook_CWT2)

ashop_chinook_CWT <- ashop_chinook_CWT %>% 
                      mutate(CWTCode = as.character(CWTCode)) %>%
                      mutate(tag_code = ifelse(nchar(CWTCode)==5,paste0("0",CWTCode),CWTCode))


# Trim the releases to make a smaller dataset to deal with.
trim.rel <- all.release.dat %>% 
              dplyr::select(tag_code=tag_code_or_release_id,
                            species,
                            run,
                            brood_year,
                            first_release_date,
                            last_release_date,
                            stock_location_code)


shoreside_cwt <- shoreside_chinook %>% dplyr::select(ID_UNIQUE,sector,
                                                     rec.month =LANDING_MONTH,
                                                     sex=SampleSex,
                                                     fork.length.cm = ForkLength..cm.,
                                                     weight.lbs = SampleWeight..lbs.,
                                                     rec.year = LANDING_YEAR,
                                                     tag_code) %>%
                      distinct(ID_UNIQUE,
                                sector,
                               rec.month ,
                               sex,
                               fork.length.cm ,
                               weight.lbs ,
                               rec.year ,
                               tag_code) %>%
                      left_join(.,trim.rel) %>%
                      mutate(rel.year = substr(first_release_date,1,4),
                             rel.month = substr(first_release_date,5,6))

shoreside_cwt <- shoreside_cwt %>% mutate(brood.year.age = rec.year - brood_year - 1) %>%
                                  filter(brood.year.age < 10)  

shoreside_cwt <- shoreside_cwt %>% mutate(run.type = case_when(run == 1 ~"spring",
                                                               run == 2 ~"summer",
                                                               run == 3 ~"fall",
                                                               run == 4 ~"winter",
                                                               run == 5 ~"hybrid",
                                                               run == 7 ~"late.fall",
                                                               run == 8 ~"late.fall"),
                                          origin_state = case_when(substr(stock_location_code,1,1)==1~"AK",
                                                                   substr(stock_location_code,1,1)==2~"BC",
                                                                   substr(stock_location_code,1,1)==3~"WA",
                                                                   substr(stock_location_code,1,1)==4~"ID",
                                                                   substr(stock_location_code,1,1)==5~"OR",
                                                                   substr(stock_location_code,1,1)==6~"CA") )

# shoreside_cwt <- shoreside_cwt %>% mutate(brood.year.age = rec.year - brood_year - 1,
#                                           month.fraction = as.numeric(rec.month/12)) %>% 
#   filter(brood.year.age < 10)  


######## ASHOP  
ashop_cwt <- ashop_chinook_CWT %>% dplyr::select(SalmonSnoutNumber,
                                                     Date,
                                                     sex,
                                                     length.cm,
                                                     weight.kg,
                                                     rec.year = year,
                                                     tag_code) %>%
  mutate(date=as.Date(Date,"%m/%d/%y"),rec.month = month(date))%>%
  filter(!tag_code %in% c("-"," ")) %>%
  left_join(.,trim.rel) %>%
  mutate(rel.year = substr(first_release_date,1,4),
         rel.month = substr(first_release_date,5,6)) %>% 
  dplyr::select(-Date,-date)

ashop_cwt <- ashop_cwt %>% mutate(brood.year.age = rec.year - brood_year - 1) %>%
                                          filter(brood.year.age < 10)  

ashop_cwt <- ashop_cwt %>% mutate(run.type = case_when(run == 1 ~"spring",
                                                               run == 2 ~"summer",
                                                               run == 3 ~"fall",
                                                               run == 4 ~"winter",
                                                               run == 5 ~"hybrid",
                                                               run == 7 ~"late.fall",
                                                               run == 8 ~"late.fall"),
                                          origin_state = case_when(substr(stock_location_code,1,1)==1~"AK",
                                                                   substr(stock_location_code,1,1)==2~"BC",
                                                                   substr(stock_location_code,1,1)==3~"WA",
                                                                   substr(stock_location_code,1,1)==4~"ID",
                                                                   substr(stock_location_code,1,1)==5~"OR",
                                                                   substr(stock_location_code,1,1)==6~"CA") )

ggplot(shoreside_cwt) +
  geom_histogram(aes(brood.year.age,fill=run.type)) +
  facet_wrap(~rec.year) +
  theme_bw() +
  scale_x_continuous("Age = Recovery year - Brood year - 1",breaks=0:5,limits=c(-0.1,5.5)) +
  ggtitle("Shoreside, Chinook CWT ages")
  
ggplot(ashop_cwt) +
    geom_histogram(aes(brood.year.age,fill=run.type)) +
    facet_wrap(~rec.year) +
  scale_x_continuous("Age = Recovery year - Brood year - 1",breaks=0:5,limits=c(-0.1,5.5)) +
  ggtitle("ASHOP, Chinook CWT ages") +
    theme_bw()

write.csv(file="Shoreside_CWT.csv",shoreside_cwt,row.names = FALSE)
write.csv(file="ASHOP_CWT.csv",ashop_cwt,row.names = FALSE)

