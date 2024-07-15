## ---------------------------------------------------------------------------------------------------------------
library(tableone)
library(readxl)


## ---------------------------------------------------------------------------------------------------------------
library(knitr)
purl(input = "recipient_data.Rmd", output = "recipient_data.R")


## ---------------------------------------------------------------------------------------------------------------
coltype <- c("text","guess","text","guess")
individual <- read_excel("CLEAN_MERGE_new4.xlsx", sheet = "individual",na = "NA",col_types = coltype)

coltypes <- c("text","text","text","text","guess","guess","guess","text","text","guess","guess","guess","guess","guess","text","guess","numeric","guess","guess","guess","guess","guess","guess","guess","guess","guess","guess","guess","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text")
graft_base <- read_excel("CLEAN_MERGE_new4.xlsx", sheet = "graft_base",na = "NA",col_types = coltypes)

individual_r <- individual[individual$type_ind == "R",]
merge1 <- merge.data.frame(individual_r,graft_base,by.x = "id_ind", by.y = "id_r",all.y = TRUE)
merge2 <- unique(merge1)

antecedent_r <- read_excel("CLEAN_MERGE_new4.xlsx", sheet = "antecedent_r",na = "NA",col_types = c(rep("numeric",13),"text","numeric"))
#antecedent_r_Nantes <- antecedent_r[complete.cases(antecedent_r$id_g),]
merge3 <- merge.data.frame(merge2,antecedent_r, by.x = "id_g",by.y = "id_g",all.x = TRUE)
merge4 <- unique(merge3)

coltype <- c("text",rep("numeric",88))
suppressWarnings(complications_graft <- read_excel("CLEAN_MERGE_new4.xlsx", sheet = "complications_graft",na = "NA",col_types = coltype))

merge5 <- merge.data.frame(merge4,complications_graft, by.x = "id_g",by.y = "id_g",all.x = TRUE)
merge6 <- unique(merge5)

coltype <- c("numeric","numeric","numeric",rep("numeric",11))
immune_compatibility <- read_excel("CLEAN_MERGE_new4.xlsx", sheet = "immune_compatibility",na = "NA",col_types = coltype)
immune_compatibility_N <- immune_compatibility[complete.cases(immune_compatibility$id_g),]

merge7 <- merge.data.frame(merge6,immune_compatibility_N, by.x = "id_g",by.y = "id_g",all.x = TRUE)
merge8 <- unique(merge7)

coltype <- c(rep("text",1),"guess",rep("text",5),rep("numeric",2),rep("text",12),rep("numeric",4),"text","text")
donor <- read_excel("CLEAN_MERGE_new4.xlsx", sheet = "donor",na = "NA",col_types = coltype)

#j=0
#k = 0
#for (i in donor$id_g){
#  if (table(donor$id_g[donor$id_g == i])==1){
#    j = j+1
#    donor_clean[j,] <- donor[donor$id_g== i,]
#  }else{
#    k= k+1
    #print(i)
#  }
#}
donor_clean <- unique(donor)

merge9 <- merge.data.frame(merge8,donor_clean, by.x = "id_d",by.y = "id_ind_d",all.x = TRUE)
merge10 <- unique(merge9)

#length(donor$id_ind_d)
#length(donor_clean$id_ind_d)
#table(donor$id_g[donor$id_g == -176])


## ---------------------------------------------------------------------------------------------------------------
merge10$sexe_ind <- as.factor(merge10$sexe_ind)
merge10$date_g <- as.Date(merge10$date_g)
merge10$year_g <- as.numeric(merge10$year_g)
merge10$type_g <- as.factor(merge10$type_g)
merge10$rank_g <- as.numeric(merge10$rank_g)
merge10$code_link_rec_don_g <- as.factor(merge10$code_link_rec_don_g)
merge10$age_predon_r <- as.numeric(merge10$age_predon_r)
merge10$height_r <- as.numeric(merge10$height_r)
merge10$weight_r <- as.numeric(merge10$weight_r)
merge10$bmi_r <- as.numeric(merge10$bmi_r)
merge10$code_disease <- as.factor(merge10$code_disease)
merge10$delay_abm_g <- as.numeric(merge10$delay_abm_g)
merge10$delay_dial_g <- as.numeric(merge10$delay_dial_g)
merge10$code_purif <- as.factor(merge10$code_purif)
merge10$mach_perf <- as.factor(merge10$mach_perf)
merge10$nb_dial_post_g <- as.numeric(merge10$nb_dial_post_g)
merge10$day_resemption_fct_g <- as.numeric(merge10$day_resemption_fct_g)
merge10$cold_ischemia_g <- as.numeric(merge10$cold_ischemia_g)
#merge10$luk_ischemia_g <- as.numeric(merge10$luk_ischemia_g)
#merge10$day_hospit_g <- as.numeric(merge10$day_hospit_g)
merge10$cmv_r <- as.factor(merge10$cmv_r)
merge10$hcv_r <- as.factor(merge10$hcv_r)
merge10$ebv_r <- as.factor(merge10$ebv_r)
merge10$ag_hbs_r <- as.factor(merge10$ag_hbs_r)
merge10$hiv_r <- as.factor(merge10$hiv_r)
merge10$transfusion_r <- as.factor(merge10$transfusion_r)
merge10$code_ttt_ind <- as.factor(merge10$code_ttt_ind)
merge10$day_ttt_ind <- as.factor(merge10$day_ttt_ind)
merge10$mach_perf <- as.factor(merge10$mach_perf)
merge10$cni_g <- as.factor(merge10$cni_g)
merge10$csa_g <- as.factor(merge10$csa_g)
merge10$tacro_g <- as.factor(merge10$tacro_g)
merge10$mTor_g <- as.factor(merge10$mTor_g)
merge10$sirolimus_g <- as.factor(merge10$sirolimus_g)
merge10$everolimus_g <- as.factor(merge10$everolimus_g)
merge10$apparent_cni_g <- as.factor(merge10$apparent_cni_g)
merge10$antiprolif_g <- as.factor(merge10$antiprolif_g)
merge10$mmf_g <- as.factor(merge10$mmf_g)
merge10$mpa_g <- as.factor(merge10$mpa_g)
merge10$aza_g <- as.factor(merge10$aza_g)
merge10$ttt_cortic_g <- as.factor(merge10$ttt_cortic_g)
merge10$ttt_other_g <- as.factor(merge10$ttt_other_g)
merge10$prophyl_pnc_g <- as.factor(merge10$prophyl_pnc_g)
merge10$prophyl_cmv_g <- as.factor(merge10$prophyl_cmv_g)
merge10$atcdt_diab_g <- as.factor(merge10$atcdt_diab_g)
merge10$atcdt_dyslip_g <- as.factor(merge10$atcdt_dyslip_g)
merge10$atcdt_vasc_g <- as.factor(merge10$atcdt_vasc_g)
merge10$atcdt_card_g <- as.factor(merge10$atcdt_card_g)
merge10$atcdt_cardvasc_g <- as.factor(merge10$atcdt_cardvasc_g)
merge10$atcdt_neoplasia_g <- as.factor(merge10$atcdt_neoplasia_g)
merge10$atcdt_Ksolide_g <- as.factor(merge10$atcdt_Ksolide_g)
merge10$atcdt_Kspino_g <- as.factor(merge10$atcdt_Kspino_g)
merge10$atcdt_Kbaso_g <- as.factor(merge10$atcdt_Kbaso_g)
merge10$atcd_Kmelanome_g <- as.factor(merge10$atcd_Kmelanome_g)
merge10$atcdt_Khemato_g <- as.factor(merge10$atcdt_Khemato_g)
merge10$atcdt_nephroBKV_g <- as.factor(merge10$atcdt_nephroBKV_g)
merge10$atcdt_pregnancy_g <- as.factor(merge10$atcdt_pregnancy_g)
merge10$atcdt_diab_g <- as.factor(merge10$atcdt_diab_g)
merge10$atcdt_dyslip_g <- as.factor(merge10$atcdt_dyslip_g)
merge10$atcdt_vasc_g <- as.factor(merge10$atcdt_vasc_g)
merge10$atcdt_card_g <- as.factor(merge10$atcdt_card_g)
merge10$atcdt_cardvasc_g <- as.factor(merge10$atcdt_cardvasc_g)
merge10$atcdt_neoplasia_g <- as.factor(merge10$atcdt_neoplasia_g)
merge10$atcdt_Ksolide_g <- as.factor(merge10$atcdt_Ksolide_g)
merge10$atcdt_Kspino_g <- as.factor(merge10$atcdt_Kspino_g)
merge10$atcdt_Kbaso_g <- as.factor(merge10$atcdt_Kbaso_g)
merge10$atcd_Kmelanome_g <- as.factor(merge10$atcd_Kmelanome_g)
merge10$atcdt_Khemato_g <- as.factor(merge10$atcdt_Khemato_g)
merge10$atcdt_nephroBKV_g <- as.factor(merge10$atcdt_nephroBKV_g)
merge10$atcdt_pregnancy_g <- as.factor(merge10$atcdt_pregnancy_g)

#merge10$type_d <- as.factor(merge10$type_d)
merge10$bmi_r <- as.numeric(merge10$bmi_r)
merge10$rejection <- as.factor(merge10$rejection)
merge10$return_dial<- as.factor(merge10$return_dial)
merge10$incomp_abo <- as.factor(merge10$incomp_abo)
merge10$incomp_a <- as.factor(merge10$incomp_a)
merge10$incomp_b <- as.factor(merge10$incomp_b)

merge10$compli_arterio <- as.factor(merge10$compli_arterio)
merge10$compli_rhumato <- as.factor(merge10$compli_rhumato)
merge10$compli_pulmo <- as.factor(merge10$compli_pulmo)
merge10$compli_card <- as.factor(merge10$compli_card)
merge10$compli_coronaro <- as.factor(merge10$compli_coronaro)
merge10$compli_InsufCard <- as.factor(merge10$compli_InsufCard)
merge10$compli_vasc...12 <- as.factor(merge10$compli_vasc...12)
merge10$compli_cardVasc <- as.factor(merge10$compli_cardVasc)
merge10$compli_hta <- as.factor(merge10$compli_hta)
merge10$compli_avc <- as.factor(merge10$compli_avc)
merge10$compli_lymphoedeme <- as.factor(merge10$compli_lymphoedeme)
merge10$compli_mvte <- as.factor(merge10$compli_mvte)
merge10$compli_urog <- as.factor(merge10$compli_urog)
merge10$compli_vasc...28 <- as.factor(merge10$compli_vasc...28)
merge10$compli_hemato <- as.factor(merge10$compli_hemato)
merge10$compli_dig <- as.factor(merge10$compli_dig)
merge10$compli_neo <- as.factor(merge10$compli_neo)
merge10$compli_melanome <- as.factor(merge10$compli_arterio)
merge10$compli_Kbaso <- as.factor(merge10$compli_Kbaso)
merge10$compli_Kspino <- as.factor(merge10$compli_Kspino)
merge10$compli_melanome <- as.factor(merge10$compli_melanome)
merge10$compli_lymphome <- as.factor(merge10$compli_lymphome)
merge10$compli_Khemato <- as.factor(merge10$compli_Khemato)
merge10$compli_Ksolide <- as.factor(merge10$compli_Ksolide)
merge10$compli_diabeteDeNovo <- as.factor(merge10$compli_diabeteDeNovo)
merge10$compli_uroNephro <- as.factor(merge10$compli_uroNephro)
merge10$complirave <- as.factor(merge10$complirave)

## ---------------------------------------------------------------------------------------------------------------
library(dplyr)
library(writexl)
library(tableone)

merge10$code_disease <- as.character(merge10$code_disease)
merge10$code_disease[merge10$code_disease==70] <- "renal vascular"
merge10$code_disease[merge10$code_disease==71] <- "Hypertension"
merge10$code_disease[merge10$code_disease==72] <- "Hypertension"
merge10$code_disease[merge10$code_disease==73] <- "renal vascular"
merge10$code_disease[merge10$code_disease==79] <- "renal vascular"
merge10$code_disease[merge10$code_disease==80] <- "Diabetes"
merge10$code_disease[merge10$code_disease==81] <- "Diabetes"
merge10$code_disease[merge10$code_disease==79] <- "renal vascular"
merge10$code_disease[merge10$code_disease==10] <- "glomerulonephritis"
merge10$code_disease[merge10$code_disease==13] <- "glomerulonephritis"
merge10$code_disease[merge10$code_disease==14] <- "glomerulonephritis"
merge10$code_disease[merge10$code_disease==15] <- "glomerulonephritis"
merge10$code_disease[merge10$code_disease==16] <- "glomerulonephritis"
merge10$code_disease[merge10$code_disease==19] <- "glomerulonephritis"
merge10$code_disease[merge10$code_disease==3] <- "glomerulonephritis"
merge10$code_disease[merge10$code_disease==84] <- "glomerulonephritis"
merge10$code_disease[merge10$code_disease==85] <- "glomerulonephritis"
merge10$code_disease[merge10$code_disease==86] <- "glomerulonephritis"
merge10$code_disease[merge10$code_disease==87] <- "glomerulonephritis"
merge10$code_disease[merge10$code_disease==88] <- "glomerulonephritis"
merge10$code_disease[merge10$code_disease==89] <- "glomerulonephritis"
merge10$code_disease[merge10$code_disease==0] <- "Unknown"
merge10$code_disease[merge10$code_disease != "glomerulonephritis" & merge10$code_disease != "Unknown" & merge10$code_disease != "renal vascular" &merge10$code_disease != "Polykistique" &merge10$code_disease != "Diabetes" & merge10$code_disease != "Hypertension"] <- "chronic interstitial nephritis"
merge10$code_disease <- as.factor(merge10$code_disease)

merge10$return_dial_5y <- if_else(merge10$delay_return_dial>0 & merge10$delay_return_dial < 1825,1,0)
merge10$return_dial_10y <- if_else(merge10$delay_return_dial>=1825 & merge10$delay_return_dial < 3650,1,0)
merge10$return_dial_5y <- as.factor(merge10$return_dial_5y)
merge10$return_dial_10y <- as.factor(merge10$return_dial_10y)

merge10$atcdt_diab_g <- as.factor(merge10$atcdt_diab_g)
merge10$atcdt_dyslip_g<- as.factor(merge10$atcdt_dyslip_g)
merge10$atcdt_vasc_g<- as.factor(merge10$atcdt_vasc_g)
merge10$atcdt_card_g<- as.factor(merge10$atcdt_card_g)
merge10$atcdt_cardvasc_g<- as.factor(merge10$atcdt_cardvasc_g)
merge10$atcdt_Ksolide_g<- as.factor(merge10$atcdt_Ksolide_g)
merge10$atcdt_Kspino_g<- as.factor(merge10$atcdt_Kspino_g)
merge10$atcdt_neoplasia_g<- as.factor(merge10$atcdt_neoplasia_g)
merge10$atcdt_Kbaso_g<- as.factor(merge10$atcdt_Kbaso_g)
merge10$atcd_Kmelanome_g<- as.factor(merge10$atcd_Kmelanome_g)
merge10$atcdt_Khemato_g<- as.factor(merge10$atcdt_Khemato_g)
merge10$atcdt_nephroBKV_g<- as.factor(merge10$atcdt_nephroBKV_g)
merge10$atcdt_pregnancy_g<- as.factor(merge10$atcdt_pregnancy_g)
merge10$atcdt_arretCardia_predon_d<- as.factor(merge10$atcdt_arretCardia_predon_d)
merge10$atcdt_smok_g<- as.factor(merge10$atcdt_smok_g)

merge10$age_predon_d <-as.numeric(merge10$age_predon_d)

merge10$compli_rhumato <- as.factor(merge10$compli_rhumato)
merge10$compli_pulmo <- as.factor(merge10$compli_pulmo)
merge10$compli_card <- as.factor(merge10$compli_card)
merge10$compli_coronaro <- as.factor(merge10$compli_coronaro)
merge10$compli_InsufCard <- as.factor(merge10$compli_InsufCard)
merge10$compli_Kbaso <- as.factor(merge10$compli_Kbaso)
merge10$compli_vasc...12 <- as.factor(merge10$compli_vasc...12)
merge10$compli_cardVasc <- as.factor(merge10$compli_cardVasc)
merge10$compli_hta <- as.factor(merge10$compli_hta)
merge10$compli_arterio <- as.factor(merge10$compli_arterio)
merge10$compli_avc <- as.factor(merge10$compli_avc)
merge10$compli_lymphoedeme <- as.factor(merge10$compli_lymphoedeme)
merge10$compli_mvte <- as.factor(merge10$compli_mvte)
merge10$compli_melanome <- as.factor(merge10$compli_melanome)
merge10$compli_Kspino <- as.factor(merge10$compli_Kspino)
merge10$compli_Khemato <- as.factor(merge10$compli_Khemato)
merge10$compli_Ksolide <- as.factor(merge10$compli_Ksolide)
merge10$compli_hemato <- as.factor(merge10$compli_hemato)
merge10$compli_dig <- as.factor(merge10$compli_dig)
merge10$compli_neo <- as.factor(merge10$compli_neo)
merge10$compli_lymphome <- as.factor(merge10$compli_lymphome)
merge10$compli_diabeteDeNovo <- as.factor(merge10$compli_diabeteDeNovo)
merge10$compli_uroNephro <- as.factor(merge10$compli_urog)
merge10$complirave <- as.factor(merge10$complirave)
merge10$compli_vasc...28 <- as.factor(merge10$compli_vasc...28)
merge10$compli_urog <- as.factor(merge10$compli_urog)
merge10$inf_urine <- as.factor(merge10$inf_urine)
merge10$inf_pneumopathie <- as.factor(merge10$inf_pneumopathie)
merge10$inf_pneumocytose <- as.factor(merge10$inf_pneumocytose)
merge10$inf_septicemie <- as.factor(merge10$inf_septicemie)
merge10$inf_cmv <- as.factor(merge10$inf_cmv)
merge10$inf_peritonite <- as.factor(merge10$inf_peritonite)
merge10$inf_bkv <- as.factor(merge10$inf_bkv)
merge10$inf_hsv <- as.factor(merge10$inf_hsv)
merge10$inf_bact <- as.factor(merge10$inf_bact)
merge10$inf_myco <- as.factor(merge10$inf_myco)
merge10$inf_parasit <- as.factor(merge10$inf_parasit)
merge10$inf_virus <- as.factor(merge10$inf_virus)
merge10$infr_gave <- as.factor(merge10$infr_gave)
merge10$atcdt_arretCardia_predon_d <- as.factor(merge10$atcdt_arretCardia_predon_d)
merge10$prot_predon_d <- as.factor(merge10$prot_predon_d)
merge10$echec_pancreas <- as.factor(merge10$echec_pancreas)
merge10$echecraft <- as.factor(merge10$echecraft)
merge10$lost_view <- as.factor(merge10$lost_view)
merge10$death_g <- as.factor(merge10$death_g)
merge10$ac_hbs_r <- as.factor(merge10$ac_hbs_r)
merge10$traitement_ind <- as.factor(merge10$traitement_ind)
merge10$type_d<- as.factor(merge10$type_d)
merge10$type_cadaver_d<- as.factor(merge10$type_cadaver_d)
merge10$ecd_scd_d<- as.factor(merge10$ecd_scd_d)
merge10$code_death_d<- as.factor(merge10$code_death_d)
merge10$cmv_predon_d<- as.factor(merge10$cmv_predon_d)
merge10$ebv_predon_d<- as.factor(merge10$ebv_predon_d)
merge10$hvc_predon_d<- as.factor(merge10$hvc_predon_d)
merge10$ag_hbs_predon_d<- as.factor(merge10$ag_hbs_predon_d)
merge10$ac_hbs_predon_d<- as.factor(merge10$ac_hbs_predon_d)
merge10$hiv_predon_d<- as.factor(merge10$hiv_predon_d)
merge10$ac_hbc_predon_d<- as.factor(merge10$ac_hbc_predon_d)
merge10$hta_predon_d<- as.factor(merge10$hta_predon_d)
merge10$diab_predon_d<- as.factor(merge10$diab_predon_d)
merge10$dyslip_predon_d<- as.factor(merge10$dyslip_predon_d)
merge10$atcdt_arretCardia_predon_d<- as.factor(merge10$atcdt_arretCardia_predon_d)
merge10$hematuria_predon_d<- as.factor(merge10$hematuria_predon_d)
merge10$drug_vaso_predon_d<- as.factor(merge10$drug_vaso_predon_d)
merge10$height_predon_d <- as.numeric(merge10$height_predon_d)
merge10$urea_predon_d <- as.numeric(merge10$urea_predon_d)
merge10$microAlb_predon_d <- as.numeric(merge10$microAlb_predon_d)



## ---------------------------------------------------------------------------------------------------------------
######################################### return_dial_5y
coltype <- c("text","numeric","numeric",rep("text",27))
consult <- read_excel("CLEAN_MERGE_new4.xlsx", sheet = "consult_r_modif",na = "NA",col_types = coltype)

df <- consult %>%
  group_by(id_g) %>%
  mutate(fol_5_years = if_else(any(year_consult >= 5), 1, 0)) %>%
  ungroup()

merge11 <- merge.data.frame(merge10,df[,c(1,31)],by.x = "id_g",by.y = "id_g",all.x = TRUE)
merge12 <- unique(merge11)

merge12$fol_5_years[is.na(merge12$fol_5_years)]<- 0

merge12$fol_5_years[merge12$return_dial_5y == 1] <- 1

merge12$fol_5_years <- as.factor(merge12$fol_5_years)

######################################### rejection_5y

biopsy <- read_excel("CLEAN_MERGE_new4.xlsx", sheet = "biopsy",na = "NA",col_types = "text")

df <- biopsy %>%
  group_by(id_g) %>%
  mutate(rejet_5y = if_else(any(diag_rejet_pbr_biopsy != 0), 1, 0)) %>%
  ungroup()

merge13 <- merge.data.frame(merge12,df[,c(1,47)],by.x = "id_g",by.y = "id_g",all.x = TRUE)
merge14 <- unique(merge13)

merge14$rejet_5y[is.na(merge14$rejet_5y)]<- 0

merge14$rejet_5y <- as.factor(merge14$rejet_5y)

######################################### create table ones
# function to asses the distribution of variables
calc_stats <- function(x) {
  mean_val <- mean(x, na.rm = TRUE)
  sd_val <- sd(x, na.rm = TRUE)
  median_val <- median(x, na.rm = TRUE)
  q1_val <- quantile(x, 0.25, na.rm = TRUE)
  q3_val <- quantile(x, 0.75, na.rm = TRUE)
  nb_NA <- sum(is.na(x))
  return(c(mean = mean_val, sd = sd_val, median = median_val, q1 = q1_val, q3 = q3_val,nb_NA = nb_NA))
}

# apply calc_stat function only numerical column
numeric_columns <- sapply(merge12, is.numeric)
filtered_merge10 <- merge12[,-c(1:4,7,8,10,195)][, numeric_columns[-c(1:4,7,8,10,195)]]
stats_list <- lapply(filtered_merge10, calc_stats)
stats_df <- do.call(rbind, stats_list)
stats_df <- as.data.frame(stats_df)
stats_df$Variable <- rownames(stats_df)

### dem table for donor type
dem_table_donor_type <- CreateTableOne(data = merge14[,-c(1:4,7,8,10,195)],strata = "type_d",addOverall = TRUE)

# add variables names
row_names <- rownames(print(dem_table_donor_type, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE,explain = FALSE))
dem_table_donor_type <- as.data.frame(print(dem_table_donor_type, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE))

if(length(row_names) == nrow(dem_table_donor_type)) {dem_table_donor_type <- cbind(Variable = row_names, dem_table_donor_type)} 

for (i in stats_df$Variable){
  dem_table_donor_type$Overall[dem_table_donor_type$Variable == i]<-NA
  dem_table_donor_type$mean[dem_table_donor_type$Variable == i] <- stats_df$mean[stats_df$Variable == i]
  dem_table_donor_type$SD[dem_table_donor_type$Variable == i] <- stats_df$sd[stats_df$Variable == i]
  dem_table_donor_type$median[dem_table_donor_type$Variable == i] <- stats_df$median[stats_df$Variable == i]
  dem_table_donor_type$Q1[dem_table_donor_type$Variable == i] <- stats_df$`q1.25%`[stats_df$Variable == i]
  dem_table_donor_type$Q3[dem_table_donor_type$Variable == i] <- stats_df$`q3.75%`[stats_df$Variable == i]
  dem_table_donor_type$nb_NA[dem_table_donor_type$Variable == i] <- stats_df$nb_NA[stats_df$Variable == i] 
}
for (i in dem_table_donor_type$Variable){
  if(i == ""){print("pass")}
  else(dem_table_donor_type$nb_NA[dem_table_donor_type$Variable == i] <- sum(is.na(merge14[,-c(1:4,7,8,10,195)][[i]])))
  }


### dem table for return dial 5y
dem_table <- CreateTableOne(data = merge14[merge14$fol_5_years == 1,-c(1:4,7,8,10,195)],strata = "return_dial_5y",addOverall = TRUE)

row_names <- rownames(print(dem_table, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE,explain = FALSE))
dem_table_df <- as.data.frame(print(dem_table, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE))

if(length(row_names) == nrow(dem_table_df)) {dem_table_df <- cbind(Variable = row_names, dem_table_df)}


### dem table for rejection < 5y
dem_table <- CreateTableOne(data = merge14[merge14$fol_5_years == 1,-c(1:4,7,8,10,195)],strata = "rejet_5y",addOverall = TRUE)

row_names <- rownames(print(dem_table, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE,explain = FALSE))
dem_table_rej <- as.data.frame(print(dem_table, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE))

if(length(row_names) == nrow(dem_table_rej)) {dem_table_rej <- cbind(Variable = row_names, dem_table_rej)}


#download the merge tables
final_dem_tabe <- cbind(dem_table_donor_type[,c(1,2,3,8:13,4:6)],dem_table_df[,c(4,5,6)],dem_table_rej[,c(4,5,6)])
writexl::write_xlsx(final_dem_tabe, "merge_dem_table_plus1.xlsx")


## ---------------------------------------------------------------------------------------------------------------
library(FactoMineR)
library(factoextra)
require(missMDA)

######### data frame with type_d pval =< 0.001

final_dem_tabe$p_val <- as.numeric(gsub("<0.001", "0.001", final_dem_tabe$p))
final_dem_tabe$p_val<0.05
names <- final_dem_tabe$Variable[final_dem_tabe$p_val == 0.001]
names <- na.exclude(names)
merge_type_d <- data.frame(merge14[["year_g"]],merge14[["type_g"]])
for (i in names[-c(1,2)]){merge_type_d <- cbind(merge_type_d,merge14[[i]])}
names(merge_type_d)<- names

cols_to_remove <- grep("delay", names(merge_type_d), value = TRUE)
merge_type_d <- merge_type_d[, !names(merge_type_d) %in% cols_to_remove]
cols_to_remove <- grep("ident", names(merge_type_d), value = TRUE)
merge_type_d <- merge_type_d[, !names(merge_type_d) %in% cols_to_remove]

######### data frame with return_dial pval =< 0.001

final_dem_tabe$p_val2 <- as.numeric(gsub("<0.001", "0.001", final_dem_tabe[,15]))
names2 <- final_dem_tabe$Variable[final_dem_tabe$p_val2 == 0.001]
names2 <- na.exclude(names2)
merge_return <- data.frame(merge14[["rank_g"]],merge14[["nb_dial_post_g"]])
for (i in names2[-c(1,2)]){merge_return <- cbind(merge_return,merge14[[i]])}
names(merge_return)<- names2

cols_to_remove <- grep("delay", names(merge_return), value = TRUE)
merge_return <- merge_return[, !names(merge_return) %in% cols_to_remove]
cols_to_remove <- grep("ident", names(merge_return), value = TRUE)
merge_return <- merge_return[, !names(merge_return) %in% cols_to_remove]

######### data frame with rejection_5y pval =< 0.001

final_dem_tabe$p_val3 <- as.numeric(gsub("<0.001", "0.001", final_dem_tabe[,18]))
names3 <- final_dem_tabe$Variable[final_dem_tabe$p_val3 ==0.001]#<= 0.005]
names3 <- na.exclude(names3)
merge_rejet <- data.frame(merge14[["age_predon_r"]],merge14[["day_resemption_fct_g"]])
for (i in names3[-c(1,2)]){merge_rejet <- cbind(merge_rejet,merge14[[i]])}
names(merge_rejet)<- names3

cols_to_remove <- grep("delay", names(merge_rejet), value = TRUE)
merge_rejet <- merge_rejet[, !names(merge_rejet) %in% cols_to_remove]
cols_to_remove <- grep("ident", names(merge_rejet), value = TRUE)
merge_rejet <- merge_rejet[, !names(merge_rejet) %in% cols_to_remove]

print("merge created")

pdf("output FAMD.pdf")
res.impute <- imputeFAMD(merge_return[,-c(25,36,37)])
res.afdm <- FAMD(merge_return[,-c(25,36,37)],tab.disj=res.impute$tab.disj)

res.impute2 <- imputeFAMD(merge_type_d[,-c(38,49,50)])
res.afdm2 <- FAMD(merge_type_d[,-c(38,49,50)],tab.disj=res.impute2$tab.disj)

res.impute3 <- imputeFAMD(merge_rejet[,-c(6,12,13)])
res.afdm3 <- FAMD(merge_rejet[,-c(6,12,13)],tab.disj=res.impute3$tab.disj)
dev.off()

print("FAMD ok")
## ---------------------------------------------------------------------------------------------------------------
pdf("res HCPC.pdf")
res.HCPC <- HCPC(res.afdm, nb.clust=-1, conso=0, min=2, max=10,method = "ward",graph = FALSE)
res.HCPC2 <- HCPC(res.afdm2, nb.clust=-1, conso=0, min=2, max=10,method = "ward",graph = FALSE)
res.HCPC3 <- HCPC(res.afdm3, nb.clust=-1, conso=0, min=2, max=10,method = "ward",graph = FALSE)
#plot.HCPC(res.HCPC, choice = "map", draw.tree = FALSE, ind.names = FALSE)
#fviz_cluster(res.HCPC,
#             main = "FAMD and HCPC on 35 variables with p_val < 0.001 for return dial 5y split",
#             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07","purple"),
#             ellipse.type = "convex", 
#             repel = FALSE, 
#             show.clust.cent = FALSE, 
#             ggtheme = theme_minimal(),
#             labelsize = 0, 
#             geom = "point") 
# Visualize HCPC clusters
p1 <- fviz_cluster(res.HCPC,
                   main = "FAMD and HCPC on 35 variables with p_val < 0.001 for return dial 5y split",
                   palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "purple"),
                   ellipse.type = "convex", 
                   repel = FALSE, 
                   show.clust.cent = FALSE, 
                   ggtheme = theme_minimal(),
                   labelsize = 0, 
                   geom = "point")

# Visualize the correlation circle from the FAMD result
p2 <- fviz_famd_var(res.afdm,
                    "quali.var",
                   col.var = "contrib", # Color by contributions to the PCs
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   repel = TRUE, # Avoid text overlapping
                   ) + 
  theme_minimal()

p3 <- fviz_famd_var(res.afdm,
                    "quanti.var",
                    col.var = "contrib",
                    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                    repel = TRUE, # Avoid text overlapping
) + 
  theme_minimal()

print(p1)
print(p2)
print(p3)

fviz_cluster(res.HCPC2,
             main = "FAMD and HCPC on 64 variables with p_val < 0.001 for type donor split",
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07","purple"),
             ellipse.type = "convex", 
             repel = FALSE, 
             show.clust.cent = FALSE, 
             ggtheme = theme_minimal(),
             labelsize = 0, 
             geom = "point") 
fviz_famd_var(res.afdm2,
                    "quali.var",
                    col.var = "contrib", # Color by contributions to the PCs
                    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                    repel = TRUE, # Avoid text overlapping
              ) + 
              theme_minimal()

fviz_famd_var(res.afdm2,
                    "quanti.var",
                    col.var = "contrib",
                    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                    repel = TRUE, # Avoid text overlapping
              ) + 
              theme_minimal()

fviz_cluster(res.HCPC3,
             main = "FAMD and HCPC on 10 variables with p_val < 0.001 for rejection before 5y split",
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07","purple"),
             ellipse.type = "convex", 
             repel = FALSE, 
             show.clust.cent = FALSE, 
             ggtheme = theme_minimal(),
             labelsize = 0, 
             geom = "point") 
fviz_famd_var(res.afdm3,
                    "quali.var",
                    col.var = "contrib", # Color by contributions to the PCs
                    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                    repel = TRUE, # Avoid text overlapping
) + 
  theme_minimal()

fviz_famd_var(res.afdm3,
                    "quanti.var",
                    col.var = "contrib",
                    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                    repel = TRUE, # Avoid text overlapping
) + 
  theme_minimal()

dev.off()

print("HCPC ok")
## ---------------------------------------------------------------------------------------------------------------
pdf("FAMD return split.pdf")
for (i in c(1,5:28,30:35)){
  plot <- fviz_pca_ind(res.afdm, label="none", habillage= i, addEllipses=TRUE, ellipse.level=0.95, palette = c("skyblue4","goldenrod","pink","green","red","black","purple","lightblue","brown"))+
    theme(plot.title = element_text(face = "bold", size = 12, hjust=0.5),plot.subtitle = element_text(size = 9, hjust=0.5)) + 
    labs(title = "Graft projection with FAMD with 35 variables", subtitle = "Split with return dial 5y")
  print(plot)
}
dev.off()

pdf("FAMD type donor split.pdf")
for (i in c(2,3,8,9,14:46,50:52,55,57:59)){
  plot <- fviz_pca_ind(res.afdm2, label="none", habillage= i, addEllipses=TRUE, ellipse.level=0.95, palette = c("skyblue4","goldenrod","pink","green","red","black","purple","lightblue","brown"))+
    theme(plot.title = element_text(face = "bold", size = 12, hjust=0.5),plot.subtitle = element_text(size = 9, hjust=0.5)) + 
    labs(title = "Graft projection with FAMD with 59 variables", subtitle = "Split with donor type")
  print(plot)
}
dev.off()

pdf("FAMD rejection split.pdf")
for (i in c(3:10)){
  plot <- fviz_pca_ind(res.afdm3, label="none", habillage= i, addEllipses=TRUE, ellipse.level=0.95, palette = c("skyblue4","goldenrod","pink","green","red","black","purple","lightblue","brown"))+
    theme(plot.title = element_text(face = "bold", size = 12, hjust=0.5),plot.subtitle = element_text(size = 9, hjust=0.5)) + 
    labs(title = "Graft projection with FAMD with 10 variables", subtitle = "Split with rejection 5y")
  print(plot)
}
dev.off()

pdf("ind_plot oriented.pdf")
plot <- fviz_pca_ind(res.afdm, label="none", habillage= merge_return$return_dial_5y, addEllipses=TRUE, ellipse.level=0.95, palette = c("skyblue4","goldenrod","pink","green","red","black","purple","lightblue","brown"))+
  theme(plot.title = element_text(face = "bold", size = 12, hjust=0.5),plot.subtitle = element_text(size = 9, hjust=0.5)) + 
  labs(title = "Graft projection with FAMD with 35 variables", subtitle = "Split with return dial 5y")
print(plot)
plot <- fviz_pca_ind(res.afdm2, label="none", habillage= merge_type_d$type_d, addEllipses=TRUE, ellipse.level=0.95, palette = c("skyblue4","goldenrod","pink","green","red","black","purple","lightblue","brown"))+
  theme(plot.title = element_text(face = "bold", size = 12, hjust=0.5),plot.subtitle = element_text(size = 9, hjust=0.5)) + 
  labs(title = "Graft projection with FAMD with 59 variables", subtitle = "Split with donor type")
print(plot)
plot <- fviz_pca_ind(res.afdm3, label="none", habillage= merge_rejet$rejet_5y, addEllipses=TRUE, ellipse.level=0.95, palette = c("skyblue4","goldenrod","pink","green","red","black","purple","lightblue","brown"))+
  theme(plot.title = element_text(face = "bold", size = 12, hjust=0.5),plot.subtitle = element_text(size = 9, hjust=0.5)) + 
  labs(title = "Graft projection with FAMD with 10 variables", subtitle = "Split with rejection 5y")
print(plot)
dev.off()

print("graph 1 ok")
## ---------------------------------------------------------------------------------------------------------------
pdf("eig FAMD.pdf")
fviz_eig(res.afdm, barfill= 'aquamarine4',barcolor = 'aquamarine4',geom="bar",width=0.8,addlabels=T,main = "Scree plot (retour dial split)")+ 
  theme(plot.title = element_text(face = "bold", size = 12, hjust=0.5),plot.subtitle = element_text(size = 9, hjust=0.5)) #+
fviz_eig(res.afdm2, barfill= 'aquamarine4',barcolor = 'aquamarine4',geom="bar",width=0.8,addlabels=T,main = "Scree plot (type donor split)")+ 
  theme(plot.title = element_text(face = "bold", size = 12, hjust=0.5),plot.subtitle = element_text(size = 9, hjust=0.5)) 
fviz_eig(res.afdm3, barfill= 'aquamarine4',barcolor = 'aquamarine4',geom="bar",width=0.8,addlabels=T,main = "Scree plot (rejection split)")+ 
  theme(plot.title = element_text(face = "bold", size = 12, hjust=0.5),plot.subtitle = element_text(size = 9, hjust=0.5)) 
dev.off()

print("graph 2 ok")
## ---------------------------------------------------------------------------------------------------------------
pdf("var_contrib_FAMD retour dial.pdf")
fviz_contrib(res.afdm, "var", axes = 1)
fviz_contrib(res.afdm, "var", axes = 2)
fviz_contrib(res.afdm, "var", axes = 3)
dev.off()

pdf("var_contrib_FAMD donor type.pdf")
fviz_contrib(res.afdm, "var", axes = 1)
fviz_contrib(res.afdm, "var", axes = 2)
fviz_contrib(res.afdm, "var", axes = 3)
dev.off()

pdf("var_contrib_FAMD rejection.pdf")
fviz_contrib(res.afdm, "var", axes = 1)
fviz_contrib(res.afdm, "var", axes = 2)
fviz_contrib(res.afdm, "var", axes = 3)
dev.off()

print("graph 3 ok")
