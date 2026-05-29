library(treeio)
library(ggtree)

Anophelestext = "((([&&NHX:P=Anopheles_sundaicus],[&&NHX:P=Anopheles_epiroticus])[&&NHX:P=Sundaicus_complex]:2,([&&NHX:P=Anopheles_stephensi])[&&NHX:A=Stephensi_complex]:2,(([&&NHX:P=Anopheles_nuneztovari_B/C])[&&NHX:P=Nuneztovari_complex],[&&NHX:P=Anopheles_aquasalis]:2)[&&NHX:A=Oswaldoi_subgroup],([&&NHX:P=Anopheles_freeborni]:2)[&&NHX:A=Freeborni_subgroup],([&&NHX:P=Anopheles_ovengensis],[&&NHX:P=Anopheles_nili],[&&NHX:P=Anopheles_carnevalei])[&&NHX:P=Nili_complex]:2,([&&NHX:P=Anopheles_merus],[&&NHX:P=Anopheles_melas],[&&NHX:P=Anopheles_gambiae_ss/coluzzii],[&&NHX:P=Anopheles_arabiensis])[&&NHX:P=Gambiae_complex]:2,([&&NHX:P=Anopheles_barbirostris])[&&NHX:P=Barbirostris_complex]:2,([&&NHX:P=Anopheles_marajoara],[&&NHX:P=Anopheles_albitarsis_E],[&&NHX:P=Anopheles_albitarsis_B],[&&NHX:P=Anopheles_albitarsis_A])[&&NHX:A=Albitarsis_complex]:2,[&&NHX:P=Anopheles_moucheti]:3,([&&NHX:I=Marcello]:0.2)[&&NHX:P=Anopheles_albimanus]:3),
(([&&NHX:P=Anopheles_subpictus])[&&NHX:P=Subpictus_complex]:2,[&&NHX:P=Anopheles_vagus]:3)[&&NHX:A=Subpictus_group],
([&&NHX:P=Anopheles_quadrimaculatus]:3)[&&NHX:P=Quadrimaculatus_group],
(([&&NHX:P=Anopheles_punctulatus])[&&NHX:A=Punctulatus_complex]:2,[&&NHX:P=Anopheles_koliensis]:3,([&&NHX:P=Anopheles_farauti])[&&NHX:P=Farauti_complex]:2)[&&NHX:P=Punctulatus_group],
[&&NHX:P=Pseudopunctipennis_group]:1,
(([&&NHX:P=Anopheles_maculatus]:2)[&&NHX:A=Maculatus_subgroup])[&&NHX:P=Maculatus_group],
((([&&NHX:P=Anopheles_leucosphyrus],[&&NHX:P=Anopheles_latens],[&&NHX:P=Anopheles_balabacensis])[&&NHX:P=Leucosphyrus_complex],([&&NHX:P=Anopheles_dirus],[&&NHX:P=Anopheles_cracens],[&&NHX:P=Anopheles_baimaii])[&&NHX:P=Dirus_complex])[&&NHX:A=Leucosphyrus_subgroup])[&&NHX:A=Leucosphyrus_group],
([&&NHX:P=Anopheles_splendidus]:3,[&&NHX:P=Anopheles_pseudojamesii]:3,[&&NHX:P=Anopheles_jamesii]:3)[&&NHX:A=Jamesii_group],
(([&&NHX:P=Anopheles_sinensis])[&&NHX:A=Sinensis_complex]:2,([&&NHX:P=Anopheles_anthropophagus]:2)[&&NHX:A=Lesteri_subgroup])[&&NHX:A=Hyrcanus_group],
(([&&NHX:P=Anopheles_flavirostris]:2,([&&NHX:P=Anopheles_minimus],[&&NHX:P=Anopheles_harrisoni])[&&NHX:P=Minimus_complex],([&&NHX:P=Anopheles_fluviatilis_U],[&&NHX:P=Anopheles_fluviatilis_T],[&&NHX:P=Anopheles_fluviatilis_S])[&&NHX:P=Fluviatilis_complex])[&&NHX:A=Minimus_subgroup],([&&NHX:P=Anopheles_funestus]:2)[&&NHX:A=Funestus_subgroup],([&&NHX:P=Anopheles_culicifacies_E]:2,[&&NHX:P=Anopheles_culicifacies_D]:2,[&&NHX:P=Anopheles_culicifacies_C]:2,[&&NHX:P=Anopheles_culicifacies_B]:2,[&&NHX:P=Anopheles_culicifacies_A]:2)[&&NHX:A=Culicifacies_subgroup],([&&NHX:P=Anopheles_aconitus]:2)[&&NHX:A=Aconitus_subgroup])[&&NHX:P=Funestus_group],
([&&NHX:P=Anopheles_Nyssorhyncus_darlingi]:3)[&&NHX:A=Darlingi_group],
(([&&NHX:P=Anopheles_nivipes])[&&NHX:A=Nivipes_complex]:2,([&&NHX:P=Anopheles_philippinensis],[&&NHX:A=Anopheles_annularis_B],[&&NHX:P=Anopheles_annularis_A])[&&NHX:P=Annularis_complex]:2)[&&NHX:A=Annularis_group])[&&NHX:R=Anopheles_genus];"

T<-read.nhx(textConnection(Anophelestext))

T@phylo$edge.length[is.nan(T@phylo$edge.length)] <- 1

ggtree(T, ladderize=TRUE, aes(color=I)) +
  geom_label(aes(label=R), fill='grey') +
  geom_label(aes(label=A), fill='grey') +
  geom_label(aes(label=P), fill = "lightgreen")+
  scale_color_manual(values="white", na.value = "black")+
  theme(legend.position = "none")

path_main="C:/Users/chamcl/Swiss Tropical and Public Health Institute, Swiss TPH/AIM - Methodological development/1. Vector Control/Bionomics/Manuscript/Revision/"

ggsave(file.path(path_main,"plot_phylogeny.png"),width = 15, height = 17)
