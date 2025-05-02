# Create combined excel
# Human

library(writexl)

Bacteria_present_KW<-read.csv("figures/Bacterial_presence/statistical_test_results/Bacteria_present_kw_test.csv")

Bacteria_present_dunn<-read.csv("figures/Bacterial_presence/statistical_test_results/Bacteria_present_dunn_test.csv")

Control_vs_all_pneu_KW<-read.csv("figures/Control_vs_all_pneumonia/statistical_test_results/new_type_kw_test.csv")

Control_vs_all_pneu_dunn<-read.csv("figures/Control_vs_all_pneumonia/statistical_test_results/new_type_dunn_test.csv")

SCV2_present_KW<-read.csv("figures/Pneumo_vs_SARS/statistical_test_results/scv2_status_kw_test.csv")

SCV2_present_Dunn<-read.csv("figures/Pneumo_vs_SARS/statistical_test_results/scv2_status_dunn_test.csv")

fisher_scv2<-read.csv("figures/Supplemental_data/fishers_test_comparing_scv2_enrichment_across_all_clusters.csv")

fishers_bacteria<-read.csv("figures/Supplemental_data/fishers_test_comparing_bacteria_enrichment_across_all_clusters.csv")

Age_KW<-read.csv("figures/Supplemental_figures/statistical_test_results/Age_binarized_kw_test.csv")

Age_Dunn<-read.csv("figures/Supplemental_figures/statistical_test_results/Age_binarized_dunn_test.csv")

Sex_KW<-read.csv("figures/Supplemental_figures/statistical_test_results/Gender_kw_test.csv")

Sex_Dunn<-read.csv("figures/Supplemental_figures/statistical_test_results/Gender_dunn_test.csv")

Alcohol_kw<-read.csv("figures/Supplemental_figures/statistical_test_results/Alcohol_status_kw_test.csv")


Alcohol_dunn<-read.csv("figures/Supplemental_figures/statistical_test_results/Alcohol_status_dunn_test.csv")

Smoking_kw<-read.csv("figures/Supplemental_figures/statistical_test_results/smoking_status_kw_test.csv")

Smoking_dunn<-read.csv("figures/Supplemental_figures/statistical_test_results/smoking_status_dunn_test.csv")


histopathology_stats_file <- list("Bacteria present KW" = Bacteria_present_KW, 
                                  "Bacteria present Dunn" = Bacteria_present_dunn, 
                                  "Control vs Pneumo KW" = Control_vs_all_pneu_KW,
                                  "Control vs Pneumo Dunn" = Control_vs_all_pneu_dunn,
                                  "SCV2 present KW" = SCV2_present_KW,
                                  "SCV2 present Dunn" = SCV2_present_Dunn,
                                  "SCV2 enrich Fisher" = fisher_scv2,
                                  "Bacteria enrich Fisher" = fishers_bacteria,
                                  "Age KW" = Age_KW,
                                  "Age Dunn" = Age_Dunn,
                                  "Sex KW" = Sex_KW,
                                  "Sex Dunn" = Sex_Dunn,
                                  "Alcohol KW" = Alcohol_kw,
                                  "Alcohol Dunn" = Alcohol_dunn,
                                  "Smoking KW" = Smoking_kw,
                                  "Smoking Dunn" = Smoking_dunn)
write_xlsx(histopathology_stats_file, "figures/Supplemental_data/All_histopathology_statistical_tests_human.xlsx")



# Human mfIHC


mfIHC_bacteria<-read.csv("figures/mfIHC/statistical_test_results/Bacteria_present_filtered_two_tailed_wilcox_test.csv")

mfIHC_scv2<-read.csv("figures/mfIHC/statistical_test_results/scv2_status_filtered_two_tailed_wilcox_test.csv")

mfIHC_cont_vs_pneumo<-read.csv("figures/mfIHC/statistical_test_results/new_type_filtered_two_tailed_wilcox_test.csv")

mfIHC_all<-read.csv("figures/mfIHC/statistical_test_results/all_clusters.csv")

mfIHC_all_stats<-list("mfIHC Bacteria present wilcox" = mfIHC_bacteria,
                      "mfICH SCV2 present wilcox" = mfIHC_scv2,
                      "mfIHC cont vs pneumo wilcox" = mfIHC_cont_vs_pneumo,
                      "mfIHC all clusters wilcox" = mfIHC_all)
write_xlsx(mfIHC_all_stats, "figures/Supplemental_data/All_mfIHC_statistical_tests_human.xlsx")


# Mouse


Human_vs_mouse_KW<-read.csv("figures/Mouse/organism_kw_test.csv")
Human_vs_mouse_Dunn<-read.csv("figures/Mouse/organism_dunn_test.csv")
Pathogen_proportions<-read.csv("figures/Supplemental_data/fishers_test_comparing_pathogen_proportions_across_clusters.csv")
histopathology_young<-read.csv("figures/Supplemental_data/mouse_saline_vs_pathogen_across_all_features_young_wilcox_test_results.csv")
histopathology_old<-read.csv("figures/Supplemental_data/mouse_saline_vs_pathogen_across_all_features_old_wilcox_test_results.csv")

mouse_all_stats<-list("Human vs Mouse KW" = Human_vs_mouse_KW,
                      "Human vs Mouse Dunn" = Human_vs_mouse_Dunn,
                      "Histopathology across models fisher" = Pathogen_proportions,
                      "Histopathology feature young wilcox" = histopathology_young,
                      "Histopathology feature old wilcox" = histopathology_old)



