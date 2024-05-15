
library(readxl)
library(openxlsx)

library(FactoMineR)
library(factoextra)

require(missMDA)
## ------------------------------------------------------------------------------------------------------------
coltype <- c("text","guess","text","guess")
merge_fix <- read_excel("MERGING_part_3.xlsx", sheet = "recipient_before",na = "NA")



## ------------------------------------------------------------------------------------------------------------
merge_fix$sexe_ind <- as.factor(merge_fix$sexe_ind)
merge_fix$date_g <- as.Date(merge_fix$date_g)
merge_fix$year_g <- as.numeric(merge_fix$year_g)
merge_fix$type_g <- as.factor(merge_fix$type_g)
merge_fix$rank_g <- as.factor(merge_fix$rank_g)
merge_fix$code_link_rec_don_g <- as.factor(merge_fix$code_link_rec_don_g)
merge_fix$age_predon_r <- as.numeric(merge_fix$age_predon_r)
merge_fix$height_r <- as.numeric(merge_fix$height_r)
merge_fix$weight_r <- as.numeric(merge_fix$weight_r)
merge_fix$bmi_r <- as.numeric(merge_fix$bmi_r)
merge_fix$code_disease <- as.factor(merge_fix$age_predon_r)
merge_fix$delay_abm_g <- as.numeric(merge_fix$delay_abm_g)
merge_fix$delay_dial_g <- as.numeric(merge_fix$delay_dial_g)
merge_fix$code_purif <- as.factor(merge_fix$code_purif)
merge_fix$mach_perf <- as.factor(merge_fix$mach_perf)
merge_fix$nb_dial_post_g <- as.numeric(merge_fix$nb_dial_post_g)
merge_fix$day_resemption_fct_g <- as.numeric(merge_fix$day_resemption_fct_g)
merge_fix$cold_ischemia_g <- as.numeric(merge_fix$cold_ischemia_g)
merge_fix$luk_ischemia_g <- as.numeric(merge_fix$luk_ischemia_g)
merge_fix$day_hospit_g <- as.numeric(merge_fix$day_hospit_g)
merge_fix$cmv_r <- as.factor(merge_fix$cmv_r)
merge_fix$hcv_r <- as.factor(merge_fix$hcv_r)
merge_fix$ebv_r <- as.factor(merge_fix$ebv_r)
merge_fix$ag_hbs_r <- as.factor(merge_fix$ag_hbs_r)
merge_fix$hiv_r <- as.factor(merge_fix$hiv_r)
merge_fix$transfusion_r <- as.factor(merge_fix$transfusion_r)
merge_fix$code_ttt_ind <- as.factor(merge_fix$code_ttt_ind)
merge_fix$day_ttt_ind <- as.factor(merge_fix$day_ttt_ind)
merge_fix$mach_perf <- as.numeric(merge_fix$mach_perf)
merge_fix$cni_g <- as.factor(merge_fix$cni_g)
merge_fix$csa_g <- as.factor(merge_fix$csa_g)
merge_fix$tacro_g <- as.factor(merge_fix$tacro_g)
merge_fix$mTor_g <- as.factor(merge_fix$mTor_g)
merge_fix$sirolimus_g <- as.factor(merge_fix$sirolimus_g)
merge_fix$everolimus_g <- as.factor(merge_fix$everolimus_g)
merge_fix$apparent_cni_g <- as.factor(merge_fix$apparent_cni_g)
merge_fix$antiprolif_g <- as.factor(merge_fix$antiprolif_g)
merge_fix$mmf_g <- as.factor(merge_fix$mmf_g)
merge_fix$mpa_g <- as.factor(merge_fix$mpa_g)
merge_fix$aza_g <- as.factor(merge_fix$aza_g)
merge_fix$ttt_cortic_g <- as.factor(merge_fix$ttt_cortic_g)
merge_fix$ttt_other_g <- as.factor(merge_fix$ttt_other_g)
merge_fix$prophyl_pnc_g <- as.factor(merge_fix$prophyl_pnc_g)
merge_fix$prophyl_cmv_g <- as.factor(merge_fix$prophyl_cmv_g)
merge_fix$atcdt_diab_g <- as.factor(merge_fix$atcdt_diab_g)
merge_fix$atcdt_dyslip_g <- as.factor(merge_fix$atcdt_dyslip_g)
merge_fix$atcdt_vasc_g <- as.factor(merge_fix$atcdt_vasc_g)
merge_fix$atcdt_card_g <- as.factor(merge_fix$atcdt_card_g)
merge_fix$atcdt_cardvasc_g <- as.factor(merge_fix$atcdt_cardvasc_g)
merge_fix$atcdt_neoplasia_g <- as.factor(merge_fix$atcdt_neoplasia_g)
merge_fix$atcdt_Ksolide_g <- as.factor(merge_fix$atcdt_Ksolide_g)
merge_fix$atcdt_Kspino_g <- as.factor(merge_fix$atcdt_Kspino_g)
merge_fix$atcdt_Kbaso_g <- as.factor(merge_fix$atcdt_Kbaso_g)
merge_fix$atcd_Kmelanome_g <- as.factor(merge_fix$atcd_Kmelanome_g)
merge_fix$atcdt_Khemato_g <- as.factor(merge_fix$atcdt_Khemato_g)
merge_fix$atcdt_nephroBKV_g <- as.factor(merge_fix$atcdt_nephroBKV_g)
merge_fix$atcdt_pregnancy_g <- as.factor(merge_fix$atcdt_pregnancy_g)
merge_fix$incomp_a <- as.factor(merge_fix$incomp_a)
merge_fix$incomp_b <- as.factor(merge_fix$incomp_b)
merge_fix$incomp_abo <- as.factor(merge_fix$incomp_abo)
merge_fix$incomp_dr <- as.factor(merge_fix$incomp_dr)
merge_fix$incomp_ab <- as.factor(merge_fix$incomp_ab)
merge_fix$incomp_bdr <- as.factor(merge_fix$incomp_bdr)
merge_fix$incomp_abdr <- as.factor(merge_fix$incomp_abdr)
merge_fix$ident_a <- as.factor(merge_fix$ident_a)
merge_fix$ident_b <- as.factor(merge_fix$ident_b)
merge_fix$ident_ab <- as.factor(merge_fix$ident_ab)
merge_fix$ident_abdr <- as.factor(merge_fix$ident_abdr)
merge_fix$ident_bdr <- as.factor(merge_fix$ident_bdr)
merge_fix$ident_dr <- as.factor(merge_fix$ident_dr)
merge_fix$ident_dr <- as.factor(merge_fix$ident_dr)
merge_fix$type_d <- as.factor(merge_fix$type_d)
merge_fix$type_cadaver_d <- as.factor(merge_fix$type_cadaver_d)
merge_fix$ecd_scd_d <- as.factor(merge_fix$ecd_scd_d)
merge_fix$code_death_d <- as.factor(merge_fix$code_death_d)
merge_fix$bmi_predon_d <- as.numeric(merge_fix$bmi_predon_d)
merge_fix$weight_predon_d <- as.numeric(merge_fix$weight_predon_d)
merge_fix$height_predon_d <- as.numeric(merge_fix$height_predon_d)
merge_fix$cmv_predon_d <- as.factor(merge_fix$cmv_predon_d)
merge_fix$ebv_predon_d <- as.factor(merge_fix$ebv_predon_d)
merge_fix$hvc_predon_d <- as.factor(merge_fix$hvc_predon_d)
merge_fix$ac_hbs_predon_d <- as.factor(merge_fix$ac_hbs_predon_d)
merge_fix$age_predon_d <- as.numeric(merge_fix$age_predon_d)
merge_fix$hiv_predon_d <- as.numeric(merge_fix$hiv_predon_d)
merge_fix$ac_hbc_predon_d <- as.factor(merge_fix$ac_hbc_predon_)
merge_fix$hta_predon_d <- as.factor(merge_fix$hta_predon_d)
merge_fix$diab_predon_d <- as.factor(merge_fix$diab_predon_d)
merge_fix$dyslip_predon_d <- as.numeric(merge_fix$dyslip_predon_d)
merge_fix$atcdt_arretCardia_predon_d <- as.numeric(merge_fix$atcdt_arretCardia_predon_d)
merge_fix$diuresis_predon_d <- as.factor(merge_fix$diuresis_predon_d)
merge_fix$urea_predon_d <- as.factor(merge_fix$urea_predon_d)
merge_fix$creat_predon_d <- as.factor(merge_fix$creat_predon_d)


## ------------------------------------------------------------------------------------------------------------

res.impute <- imputeFAMD(merge_fix[,c(3:80)], ncp=2)
res.famd <- FAMD(merge_fix[,c(3:80)],tab.disj = res.impute$tab.disj,graph = TRUE)

pdf(file = "screeplot.pdf")
fviz_screeplot(res.famd, barfill= '#00AFBB',barcolor = '#00AFBB',geom="bar",width=0.8,addlabels=T) + 
  theme(plot.title = element_text(face = "bold", size = 12, hjust=0.5),plot.subtitle = element_text(size = 9, hjust=0.5),plot.caption = element_text(face = "italic", size = 5, hjust=1)) +
  labs(title = "Own Values Composants (% ) of FAMD with all recipient variables")
dev.off()


pdf(file = "contrib1.pdf")
fviz_contrib(res.famd, fill= 'steelblue4',color = "steelblue4",width=0.8,addlabels=T,"var",axes=1) + 
  theme(plot.title = element_text(face = "bold", size = 12, hjust=0.5),plot.subtitle = element_text(size = 9, hjust=0.5),plot.caption = element_text(face = "italic", size = 5, hjust=1)) +
  labs(title = "Contribution per variables on Dimension 1 (%) of FAMD with all recipient variables")
dev.off()

pdf(file = "contrib2.pdf")
fviz_contrib(res.famd, fill= "indianred3",color = "indianred3",width=0.8,addlabels=T,"var",axes=2) + 
  theme(plot.title = element_text(face = "bold", size = 12, hjust=0.5),plot.subtitle = element_text(size = 9, hjust=0.5),plot.caption = element_text(face = "italic", size = 5, hjust=1)) +
  labs(title = "Contribution per variables on Dimension 2 (%) of FAMD with all recipient variables")
dev.off()

pdf(file = "var.pdf")
fviz_famd_var(res.famd, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),ggtheme = theme_minimal(),repel=TRUE) +
  theme(plot.title = element_text(face = "bold", size = 12, hjust=0.5),plot.subtitle = element_text(size = 9, hjust=0.5),plot.caption = element_text(face = "italic", size = 7, hjust=1)) + 
  labs(title = "Variables projection of FAMD with all recipient variables")
dev.off()

fviz_famd_var(res.famd, repel = TRUE)

pdf(file = "ind_sex.pdf")
fviz_mfa_ind(res.famd, habillage = 1, palette = c("skyblue4", "#E7B800"),addEllipses = TRUE, ellipse.level = 0.95, repel = TRUE, label="none")+
  theme(plot.title = element_text(face = "bold", size = 12, hjust=0.5),plot.subtitle = element_text(size = 8, hjust=0.5),plot.caption = element_text(face = "italic", size = 7, hjust=1)) + 
  labs(title = "Individuals projection of FAMD with all recipient variables")
dev.off()

pdf(file = "ind_abo.pdf")
fviz_mfa_ind(res.famd, habillage = 2, palette = c("skyblue4", "#E7B800","darkorchid2","brown1"),addEllipses = TRUE, ellipse.level = 0.95, repel = TRUE, label="none")+
  theme(plot.title = element_text(face = "bold", size = 12, hjust=0.5),plot.subtitle = element_text(size = 8, hjust=0.5),plot.caption = element_text(face = "italic", size = 7, hjust=1)) + 
  labs(title = "Individuals projection of FAMD with all recipient variables")
dev.off()

pdf(file = "ind_typeG.pdf")
fviz_mfa_ind(res.famd, habillage = 5, palette = c("skyblue4", "#E7B800","darkorchid2","brown1","darkgreen"),addEllipses = TRUE, ellipse.level = 0.95, repel = TRUE, label="none")+
  theme(plot.title = element_text(face = "bold", size = 12, hjust=0.5),plot.subtitle = element_text(size = 8, hjust=0.5),plot.caption = element_text(face = "italic", size = 7, hjust=1)) + 
  labs(title = "Individuals projection of FAMD with all recipient variables")
dev.off()

pdf(file = "ind_rankG.pdf")
fviz_mfa_ind(res.famd, habillage = 6, palette = c("skyblue4", "#E7B800","darkorchid2","brown1","darkgreen"),addEllipses = TRUE, ellipse.level = 0.95, repel = TRUE, label="none")+
  theme(plot.title = element_text(face = "bold", size = 12, hjust=0.5),plot.subtitle = element_text(size = 8, hjust=0.5),plot.caption = element_text(face = "italic", size = 7, hjust=1)) + 
  labs(title = "Individuals projection of FAMD with all recipient variables")
dev.off()

pdf(file = "ind_traitement.pdf")
fviz_mfa_ind(res.famd, habillage = 31, palette = c("skyblue4", "#E7B800","darkorchid2","brown1","darkgreen"),addEllipses = TRUE, ellipse.level = 0.95, repel = TRUE, label="none")+
  theme(plot.title = element_text(face = "bold", size = 12, hjust=0.5),plot.subtitle = element_text(size = 8, hjust=0.5),plot.caption = element_text(face = "italic", size = 7, hjust=1)) + 
  labs(title = "Individuals projection of FAMD with all recipient variables")
dev.off()



pdf(file = "other.pdf")
## ------------------------------------------------------------------------------------------------------------
fviz_famd_var(res.famd) +
  labs(title = "Res of FAMD with all variables recipient")


## ------------------------------------------------------------------------------------------------------------
fviz_screeplot(res.famd)+
  labs(title = "Res of FAMD with all variables recipient")


## ------------------------------------------------------------------------------------------------------------
fviz_contrib(res.famd, "var", axes = 1)
fviz_contrib(res.famd, "var", axes = 2)
fviz_contrib(res.famd, "var", axes = 3)


## ------------------------------------------------------------------------------------------------------------
fviz_pca_ind(res.famd, label="none", habillage=1, addEllipses=TRUE, ellipse.level=95, palette = c("skyblue4","goldenrod","pink","green","red","yellow"))+
  theme(plot.title = element_text(face = "bold", size = 12, hjust=0.5),plot.subtitle = element_text(size = 9, hjust=0.5)) + 
  labs(title = "Individuals projection of FAMD with 178 mixtes variables")
dev.off()
