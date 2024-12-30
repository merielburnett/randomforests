library(tidyverse)
library(easystats)
library(janitor)
library(hablar)
library(careless)
library(sjmisc)
library(psych)
library(freqtables)
library(isotree)


agglang<-read_csv("Data/Raw Prep/fulldata.csv", col_names = TRUE) %>%
  slice(-c(1:2)) %>% 
  hablar::retype() %>%
  janitor::clean_names()

# this object has prolific id's for anyone rejected in prolific
# these include straightlining, other things caught in early screening
# don't include DRI items because those are caught later
prolific_rejected <- c("670c58c1bba676dd72f89d9d", "5bdf8d7eee652a0001f0999c", "663e47f0c517bda322a94fe8",
                       "671abe1dccaf30c200eca904", "65dd0f812be8afe8d9539c7c", "65dd0f812be8afe8d9539c7c",
                       "6713fd29f92e00cd4d9d165a", "66c3e9226e53a47150ada20e", "66ef865205edfd0f5bed6a93",
                       "66b1b31b3b1f217ce617b41e", "6679751777a339a589213d20", "6695205887bb69e745179e09",
                       "66675c887a593976a91c4b68", "6647c117352b708167ccfd48", "670ade3621dd9160b59fd56c",
                       "66f267387fb5eaa6419752e4", "6438e5ddc8f49802145ad685", "66c36fe6d22ac6b19f1ad2f7",
                       "664f934b501266e8401ada48", "66d759d40786f72ee34ba77a", "66293a1955a99ad01eb673d3",
                       "66f3515ea0ea5ba3a2a05e7c", "65db7262504a6a75a4d6e3f8", "66f5c81fa771db6db1bc2382",
                       "66ecae7c719f9db1fdc9c708", "65d6482bc7c0f830c49a7622", "66f841ccf9b37bb1c54f83b9",
                       "65d7618924ced69f831161c8", "67185627848fa95be142de8a", "67178054a368314e6afa5ed4", 
                       "66c42da12cca15dd8d446e9a", "5dc2fce0031ed621054ab1f2", "5c93b1e8ed48490014619f21",
                       "66b4c4c7e246ed808308e969", "63615888b5606b24a845f73e", "6597ff8087f61724316b4df6",
                       "5f6388bf9ae5ad07ba1a0173", "66da0028189369965f4b91ce", "657dbb30678be2d216a81912",
                       "654a5b5e1a5a91fecd090714", "5be8353e33f40a0001031d7a", "66e8d4c69a74ea5ce75873ec",
                       "66a91c9bb82807bae0e4ff19", "670692b29cd73b506df2f3dc", "668549903e73f0aed0651cbf",
                       "667036fd1ce66ec3e9b66169", "6716a42e16da0c8846644f33", "6684d07784aece78a44ea2da",
                       "671109989456948c884480b0", "5f84743db0bcc50c63cdb55c", "66b93ba9ccbb002158c59355",
                       "5c35293501f27e0001094ba5", "66c396ad1fd21de8d95c3921")

# people to see if they get flagged
# e.g., look like straightliners, quick responders, IntraSD, but not obvious rejections
prolific_suspected <- c("5955c739cbe7f60001f64a5e", "5751f576a9de4b0006e557b9")


#
# basic filtering and prepping----

# omit incomplete rows
# recode DRI items them to 0=okay, 1=failed the item

agglang <- agglang %>%
  filter(finished==1) %>% # only completers
  mutate(minutes = duration_in_seconds/60) %>% # sets duration to minutes
  mutate(dri1 = rec(dri_hum, rec="1=0; 2:5,NA=1")) %>%
  mutate(dri2 = rec(dri_sd4, rec="5=0; 1:4,NA=1")) %>%
  mutate(any_help = rec(any_help, rec="0=0; 1,NA=1"))

freq_table(agglang, dri1)
freq_table(agglang, dri2)
freq_table(agglang, any_help)
describe(agglang$minutes)


#
# careless----

##LONGSTRING----

#getting longstring index for hexaco
hxco_longstring <- longstring(select(agglang, starts_with("hxco_")), avg = TRUE)
plot(hxco_longstring)
describe(hxco_longstring$longstr)

#getting longstring index for HEISS
heiss_longstring <- longstring(select(agglang, starts_with("humef"), starts_with("humid"), starts_with("hum_trap")), avg = TRUE)
plot(heiss_longstring)
describe(heiss_longstring$longstr)

#boxplots for each
boxplot(hxco_longstring, main = "Boxplot of Avg Longstring Index for HEXACO")
boxplot(heiss_longstring, main = "Boxplot of Avg Longstring Index for HEISS")

#there are definitely some outliers. someone had 50 straight responses to the hxco


## MAHAD DISTANCE----

#TriPM
tripm_items <- select(agglang, starts_with("tripm_"))

#calc mahalanobis distance for tripm items
mahad_tripm <- mahad(tripm_items)

#looks like it starts deviating at around the 20th quantile?

# HSQ
hsq_items <- select(agglang, starts_with("hsq_"))

#calc mahalanobis distance for hsq items
mahad_hsq <- mahad(hsq_items)

#deviates at below 80, and above about 110?

##INTRA INDIVIDUAL SD----

#HEXACO
hxco_items <- select(agglang, starts_with("hxco_"))
irv_hexaco <- irv(hxco_items, split = TRUE, num.split = 2)
describe(irv_hexaco)

#HEISS
heiss_items <- agglang %>% select(starts_with("humef"), starts_with("humid"), contains("hum_trap")) 

irv_heiss <- irv(heiss_items, split = FALSE)
hist(irv_heiss)

#HSQ
hsq_items <- agglang %>% select(starts_with("hsq_"))

irv_hsq <- irv(hsq_items, split = FALSE)
hist(irv_hsq)


## bind into dataset
inattents <- data.frame("longstr_hex" = hxco_longstring$longstr,
                        "longstr_heiss" = heiss_longstring$longstr,
                        "irv_hex" = irv_hexaco$irvTotal,
                        "mahal_tripm" = mahad_tripm,
                        "mahal_hsq" = mahad_hsq)

agglang1 <- data.frame(inattents, agglang)

outlier_data <- agglang1 %>%
  select(1:5, dri1, dri2, any_help, minutes)


## isolation forests----
iso1 <- isotree::isolation.forest(data=outlier_data,
                                  ntrees=5000, 
                                  max_depth = NULL,  # grows to true isolation
                                  ndim = 1,
                                  scoring_metric = "depth")

iso2 <- isotree::isolation.forest(data=outlier_data,
                                  ntrees=5000, 
                                  prob_pick_pooled_gain = .65,
                                  ndim = 1,
                                  penalize_range = T,
                                  scoring_metric = "adj_depth")

iso3 <- isotree::isolation.forest(data=outlier_data,
                                  ntrees=5000, 
                                  max_depth = NULL,
                                  ndim = 2,  # note 2 dims
                                  scoring_metric = "density")  # density

iso_out <- data.frame("iso_depth" = predict.isolation_forest(iso1, outlier_data),
                      "iso_tuned" = predict.isolation_forest(iso2, outlier_data),
                      "iso_extended" = predict.isolation_forest(iso3, outlier_data),
                      outlier_data,
                      "prof_id" = agglang1$prof_id)

describe(iso_out)
lowerCor(iso_out[1:12])

hist(iso_out$iso_extended)

# view the data, so some sorting for intuition
View(iso_out)


# how many people do we have with basic exclusions?
remaining <- iso_out %>%
  filter(dri1 == 0) %>%
  filter(dri2 == 0) %>%
  filter(any_help == 0) %>%
  filter(! prof_id %in% prolific_rejected)


#mushy here.....
# let's drop anyone > 1.6 in the isoforest
remaining1 <- remaining %>%
  filter(iso_extended < 1.6)


# now look at a few things....

# drop HEXACO longstring=1 (66e19a338ca7c8737ac78d51); they alternated 4 and 5 the whole time
# and drop longstrings greater than 14...smh...

remaining2 <- remaining1 %>%
  filter(longstr_hex != 1) %>%
  filter(longstr_hex < 15)

# HEISS trap-item
# drop people with straight 5s
heissdat<-agglang1 %>% select(longstr_heiss, hum_trap, humid1:humef4, prof_id) %>%
  arrange(-longstr_heiss, -hum_trap)

heissdrop<-heissdat %>% slice(1:3) %>% select(prof_id)

remaining3 <- anti_join(remaining2, heissdrop, 
                        by = "prof_id")


# any thoughts about duration at this point?
# must have taken at least 7 minutes
plot(remaining3$iso_extended, remaining3$minutes)

describe(remaining3$minutes)

remaining4 <- remaining3 %>%
  filter(minutes > 7)

#did the suspects get flagged?
usual_suspects <- remaining4 %>%
  filter(prof_id %in% prolific_suspected)


#
# make a complete self-report file----

agglang2 <- semi_join(agglang1, remaining4, by = "prof_id")


#
# any text-based exclusions?
text_screen <- agglang2 %>%
  select(prof_id, starts_with("hum_")) %>% select(-hum_trap)

# write_csv(text_screen, "Data/Text_Screen.csv")

# drops based on hand screening the text, plus extensive HEXACO missingness (4th case)
textdrop <- c("66b4dd94546a0fa9f204621d", "671819681e82990c9080f28d", 
              "671819681e82990c9080f28d", "669e4b24ebec5092cdef64c4")

#getting rid of rows that match the textdrop IDs
agglang2 <- agglang2 %>%
  filter(!prof_id %in% textdrop)



#
# some quick frequencies-----

# gender coding
# 0 = male, 1 = female, -2 = self-describe, -1 = prefer not to state
freq_table(agglang2, gender)

# education coding
# 0 = less than HS, 1 = HS, 2 = some college, 
# 3 = associates, 4 = bachelors, 5 = grad degree, -1 = not saying
freq_table(agglang2, ed_level)


#
# reverse scoring-----

## hexaco----
hex_to_reverse <- agglang2 %>%
  select(contains(grep("^hxco_.*r$", colnames(agglang2), value = TRUE)))

hex_key <- c(rep(-1,50)) # make 50 "-1" values (indicating reverse)
hex_reversed <- reverse.code(keys = hex_key,
                             items = hex_to_reverse,
                             mini = 1, maxi = 5) %>% 
                as.data.frame()

## tripm----
tripm_to_reverse <- agglang2 %>%
  select(contains(grep("^tripm_.*r$", colnames(agglang2), value = TRUE)))

tripm_key <- c(rep(-1,4)) # make 4 "-1" values (indicating reverse)

# note this is a 1 to 4 scale, not 1 to 5
tripm_reversed <- reverse.code(keys = tripm_key,
                               items = tripm_to_reverse,
                               mini = 1, maxi = 4) %>% 
  as.data.frame()

## humor styles----
hsq_to_reverse <- agglang2 %>%
  select(contains(grep("^hsq.*r$", colnames(agglang2), value = TRUE)))

hsq_key <- c(rep(-1,5)) # make 5 "-1" values (indicating reverse)
hsq_reversed <- reverse.code(keys = hsq_key,
                             items = hsq_to_reverse,
                             mini = 1, maxi = 5) %>% # a nice option to ensure 1 to 5 (even if empirical range is 2 to 5)
  as.data.frame()


## add back reversed variables----

agglang3 <- agglang2 %>%
  # drops the old columns
  select(!contains(colnames(hex_to_reverse))) %>%
  select(!contains(colnames(tripm_to_reverse))) %>%
  select(!contains(colnames(hsq_to_reverse))) %>%
  # binds new, reversed ones
  bind_cols(hex_reversed, tripm_reversed, hsq_reversed)


#
# averaging scales----

## HEXACO Global Trait Averages----
h_scale_averages <- data.frame(
  row_means(select(agglang3, starts_with("hxco_h_")), var = "h_hexaco", n=.50, append=F),
  row_means(select(agglang3, starts_with("hxco_e_")), var = "e_hexaco", n=.50, append=F),
  row_means(select(agglang3, starts_with("hxco_x_")), var = "x_hexaco", n=.50, append=F),
  row_means(select(agglang3, starts_with("hxco_a_")), var = "a_hexaco", n=.50, append=F),
  row_means(select(agglang3, starts_with("hxco_c_")), var = "c_hexaco", n=.50, append=F),
  row_means(select(agglang3, starts_with("hxco_o_")), var = "o_hexaco", n=.50, append=F)
)

## HEXACO Facets----
hex_facets <- data.frame(
  # H
  row_means(select(agglang3, starts_with("hxco_h_sin")), var="h_sincere", n=1, append=F),
  row_means(select(agglang3, starts_with("hxco_h_fair")), var="h_fairness", n=1, append=F),
  row_means(select(agglang3, starts_with("hxco_h_gre")), var="h_greed", n=1, append=F),
  row_means(select(agglang3, starts_with("hxco_h_mod")), var="h_modesty", n=1, append=F),
  # E  
  row_means(select(agglang3, starts_with("hxco_e_fear")), var="e_fearful", n=1, append=F),
  row_means(select(agglang3, starts_with("hxco_e_anx")), var="e_anxiety", n=1, append=F),
  row_means(select(agglang3, starts_with("hxco_e_dep")), var="e_dependence", n=1, append=F),
  row_means(select(agglang3, starts_with("hxco_e_sen")), var="e_sentimental", n=1, append=F),
  # X
  row_means(select(agglang3, starts_with("hxco_x_sse")), var="x_socialesteem", n=1, append=F),
  row_means(select(agglang3, starts_with("hxco_x_sbo")), var="x_socialboldness", n=1, append=F),
  row_means(select(agglang3, starts_with("hxco_x_soc")), var="x_sociability", n=1, append=F),
  row_means(select(agglang3, starts_with("hxco_x_liv")), var="x_liveliness", n=1, append=F),
  # A
  row_means(select(agglang3, starts_with("hxco_a_for")), var="a_forgiveness", n=1, append=F),
  row_means(select(agglang3, starts_with("hxco_a_gen")), var="a_gentleness", n=1, append=F),
  row_means(select(agglang3, starts_with("hxco_a_flex")), var="a_flexibility", n=1, append=F),
  row_means(select(agglang3, starts_with("hxco_a_pat")), var="a_patience", n=1, append=F),
  # C
  row_means(select(agglang3, starts_with("hxco_c_org")), var="c_organized", n=1, append=F),
  row_means(select(agglang3, starts_with("hxco_c_dil")), var="c_diligence", n=1, append=F),
  row_means(select(agglang3, starts_with("hxco_c_perf")), var="c_perfection", n=1, append=F),
  row_means(select(agglang3, starts_with("hxco_c_pru")), var="c_prudence", n=1, append=F),
  # O
  row_means(select(agglang3, starts_with("hxco_o_aes")), var="o_aesthetics", n=1, append=F),
  row_means(select(agglang3, starts_with("hxco_o_inq")), var="o_inquisitive", n=1, append=F),
  row_means(select(agglang3, starts_with("hxco_o_crea")), var="o_creativity", n=1, append=F),
  row_means(select(agglang3, starts_with("hxco_o_unc")), var="o_unconventional", n=1, append=F),
  # altruism
  row_means(select(agglang3, starts_with("hxco_trui")), var="altruism", n=1, append=F)
)
  
## HSQ----
hsq_scales <- data.frame(
  row_means(select(agglang3, starts_with("hsq_affil")), var="affiliative_hsq", n=2, append=F),
  row_means(select(agglang3, starts_with("hsq_enhan")), var="enhancing_hsq", n=2, append=F),
  row_means(select(agglang3, starts_with("hsq_aggr")), var="aggressive_hsq", n=2, append=F),
  row_means(select(agglang3, starts_with("hsq_defeat")), var="defeating_hsq", n=2, append=F)
)
  

## SD4----
sd4_scales <- data.frame(
  row_means(select(agglang3, starts_with("sd4_m")), var="machiavellian_sd4", n=3, append=F),
  row_means(select(agglang3, starts_with("sd4_n")), var="narcissism_sd4", n=3, append=F),
  row_means(select(agglang3, starts_with("sd4_p")), var="psychopathy_sd4", n=3, append=F),
  row_means(select(agglang3, starts_with("sd4_s")), var="sadism_sd4", n=3, append=F)
)

## TriPM scales----
tripm_scales <- data.frame(
  row_means(select(agglang3, starts_with("tripm_b")), var="boldness_tripm", n=3, append=F),
  row_means(select(agglang3, starts_with("tripm_d")), var="disinhibition_tripm", n=3, append=F),
  row_means(select(agglang3, starts_with("tripm_m")), var="meanness_tripm", n=3, append=F)
)

## HEISS and humor background scales----
heiss_scales <- data.frame(
  row_means(select(agglang3, starts_with("humef")), var="humor_efficacy", n=3, append=F),
  row_means(select(agglang3, starts_with("humid")), var="humor_identity", n=3, append=F),
  row_means(select(agglang3, starts_with("hb_")), var="humor_background", n=4, append=F)
)


## binding back together----

# quick look
temptext<- bind_cols(h_scale_averages, hex_facets, sd4_scales, 
                     tripm_scales, hsq_scales, heiss_scales)
describe(temptext)

agglang4 <- bind_cols(agglang3, h_scale_averages, hex_facets, sd4_scales, 
                      tripm_scales, hsq_scales, heiss_scales)


#
# sanity checks----

# here, see if the scales have the right number of items, reverse minus flags, and good alphas

## internal consistency----

# hexaco 6 factors
agglang4 %>% select(starts_with("hxco_h_")) %>% alpha()  # H
agglang4 %>% select(starts_with("hxco_e_")) %>% alpha()  # E
agglang4 %>% select(starts_with("hxco_x_")) %>% alpha()  # X
agglang4 %>% select(starts_with("hxco_a_")) %>% alpha()  # A        
agglang4 %>% select(starts_with("hxco_c_")) %>% alpha()  # C
agglang4 %>% select(starts_with("hxco_o_")) %>% alpha()  # O

#tripm psychopathy
agglang4 %>% select(starts_with("tripm_bold")) %>% alpha()  
agglang4 %>% select(starts_with("tripm_mean")) %>% alpha()  
agglang4 %>% select(starts_with("tripm_dis")) %>% alpha()    

# hsq
agglang4 %>% select(starts_with("hsq_af")) %>% alpha() 
agglang4 %>% select(starts_with("hsq_ag")) %>% alpha() 
agglang4 %>% select(starts_with("hsq_en")) %>% alpha() 
agglang4 %>% select(starts_with("hsq_de")) %>% alpha() 

#alpha for HEISS 
agglang4 %>% select(starts_with("humef")) %>% alpha()  # humor efficacy
agglang4 %>% select(starts_with("humid")) %>% alpha()  # humor identity
agglang4 %>% select(starts_with("hb_")) %>% alpha() 

#alpha for sd4 dark tetrad
agglang4 %>% select(starts_with("sd4_m")) %>% alpha()  
agglang4 %>% select(starts_with("sd4_n")) %>% alpha()  
agglang4 %>% select(starts_with("sd4_p")) %>% alpha()   
agglang4 %>% select(starts_with("sd4_s")) %>% alpha()   


########### really, you could omit the ones you won't use at this point///

#defining tasks
tasks <- c("coffee", "singing", "food", "outfit", "beach", "car")

agglang5 <- agglang4

#apply rule: #if the text is missing, OR if the self-rating is missing, 
#they don't get a rt or sr rt for each task
for (task in tasks) {
  hum_col <- paste0("hum_", task)  # e.g., hum_food
  sr_col <- paste0("sr_", task)  # e.g., sr_food
  time_col <- paste0("time_", task, "_last_click")  # e.g., time_food_last_click
  time_sr_col <- paste0("time_sr_", task, "_last_click")  # e.g., time_sr_food_last_click
  
  #rule 1: If hum_[task] is empty, make both time_[task]_last_click and time_sr_[task]_last_click empty
  agglang5[[time_col]][is.na(agglang5[[hum_col]])] <- NA
  agglang5[[time_sr_col]][is.na(agglang5[[hum_col]])] <- NA
  
  #rule 2: If sr_[task] is empty, make time_sr_[task]_last_click empty
  agglang5[[time_sr_col]][is.na(agglang5[[sr_col]])] <- NA
}

#renaming response time
agglang5 <- agglang5 %>%
  rename(
    beach_sr_rt = time_sr_beach_last_click,
    singing_sr_rt = time_sr_singing_last_click,
    car_sr_rt = time_sr_car_last_click,
    outfit_sr_rt = time_sr_outfit_last_click,
    coffee_sr_rt = time_sr_coffee_last_click,
    food_sr_rt = time_sr_food_last_click,
    beach_rt = time_beach_last_click,
    singing_rt = time_singing_last_click,
    car_rt = time_car_last_click,
    outfit_rt = time_outfit_last_click,
    coffee_rt = time_coffee_last_click,
    food_rt = time_food_last_click
  )


#winsorizing humor response time and self rated humor response time
# it is set to omit 5% from each end, but this is flexible
agglang6 <- agglang5 %>%
  mutate(
    across(
      .cols = c(beach_sr_rt, singing_sr_rt, car_sr_rt, outfit_sr_rt, coffee_sr_rt, food_sr_rt,
                beach_rt, singing_rt, car_rt, outfit_rt, coffee_rt, food_rt),
      .fns = ~ datawizard::winsorize(., threshold = .05, method = "percentile"),
      .names = "{.col}_win"
    )
  )
describe(agglang6[,c(308:319)])
hist(agglang6$car_rt_win)


#creating self-rated humor avg
agglang6 <- agglang6 %>%
  mutate(
    sr_avg = row_means(
      select(., sr_food, sr_outfit, sr_beach, sr_singing, sr_coffee, sr_car),
      n = 4,
      append = FALSE
    )
  )

agglang6$sr_avg <-  agglang6$sr_avg$rowmeans



#
# Prolific demographics----

# read in CSV files

prolific_raw_files <- list.files(path = "Data/Prolific Demographics",
                                 recursive = TRUE,
                                 pattern = "\\.csv$",
                                 full.names = TRUE)

prol0 <- prolific_raw_files %>% map_dfr(read_csv) %>%
  clean_names()

prol1 <- prol0 %>%
  rename(prof_id = participant_id) %>%
  rename(gender_prolific = sex) %>%
  relocate(prof_id, gender_prolific)

prol2 <- prol1 %>% select(1:2)

############ semi-join prol2 into the main data file
agglang7 <- left_join(agglang6, prol2, by = "prof_id")


#exporting to test text analysis
write_csv(agglang7, "Data/MA_Raw_Prepped.csv")


# END #



