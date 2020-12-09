# load package yang diperlukan
library(tidyverse)
library(vcd)

# load data daftar frekuensi dengan kata dasar angge dan anggo =========
angg <- readRDS("angge_anggo.rds")

# Figure 1 ==========
fig1 <- angg %>% 
  filter(str_detect(word, '^a')) %>% 
  .[c(1, 2, 4, 5, 6, 10), ] %>% 
  mutate(vocal_ending = if_else(str_detect(word, "(é|e)"), "é/e", "o"), 
         post_vocal_nasal = if_else(str_detect(word, "n$"), "yes", "no"), 
         word = factor(word, 
                       levels = c("anggén", "anggé", "anggen", 
                                  "angge", "anggon", "anggo"))) %>% 
  ggplot(aes(x = word, y = n, group = vocal_ending, fill = vocal_ending)) + 
  geom_bar(stat = "identity") +
  labs(x = "Varian", y = "Frekuensi", fill = "Vokal akhir") +
  geom_text(aes(label = n), vjust = -.25) +
  theme_bw() + theme(legend.position = 'top', 
                     axis.text.x.bottom = element_text(face = "bold"),
                     axis.title.x.bottom = element_text(size = 15),
                     axis.title.y.left = element_text(size = 15)) + 
  scale_fill_grey()
fig1
fig1 + ggsave(filename = "fig1-anggo-angge.png", 
              width = 6.5, height = 5, dpi = 300)

# total frekuensi varian dengan E dan O ==========
total_E_O <- angg %>% 
  filter(str_detect(word, '^a')) %>% 
  .[c(1, 2, 4, 5, 6, 10), ] %>% 
  mutate(vocal_ending = if_else(str_detect(word, "(é|e)"), "é/e", "o"), 
         post_vocal_nasal = if_else(str_detect(word, "n$"), "yes", "no"), 
         word = factor(word, 
                       levels = c("anggén", "anggé", "anggen", 
                                  "angge", "anggon", "anggo"))) %>% 
  group_by(vocal_ending) %>% 
  summarise(n=sum(n), .groups = "drop") %>% 
  pull(n, name = vocal_ending)
total_E_O
## chi-square test goodness-of-fit untuk distribusi varian e and o =========
chisq.test(total_E_O, correct = FALSE)


# Data untuk Tabel 1. Distribusi varian angge/o[n] =========
ANGGO <- angg %>% 
  filter(str_detect(word, '^a')) %>% 
  .[c(1, 2, 4, 5, 6, 10), ] %>% 
  mutate(vocal_ending = if_else(str_detect(word, "(é|e)"), "é/e", "o"), 
         post_vocal_nasal = if_else(str_detect(word, "n$"), "yes", "no")) %>% 
  group_by(vocal_ending, post_vocal_nasal) %>% 
  summarise(n = sum(n), .groups = "drop") %>%
  ungroup() %>% 
  pivot_wider(names_from = post_vocal_nasal, values_from = n) %>% 
  data.frame(row.names = 1) %>% 
  as.matrix()
(dimnames(ANGGO) <- list(vokal_akhir = c("é/e", "o"), 
                         post_vokal_nasal = c("tidak", "ya")))
(ANGGO <- ANGGO[c(2,1), ]) 
ANGGO

## proporsi distribusi dalam Tabel 1=========
round(prop.table(ANGGO, margin = 2)*100, 2)

# Figure 2. =======
fig2 <- angg %>% 
  filter(str_detect(word, '^a')) %>% 
  .[c(1, 2, 4, 5, 6, 10), ] %>% 
  mutate(vocal_ending = if_else(str_detect(word, "(é|e)"), "é/e", "o"), 
         post_vocal_nasal = if_else(str_detect(word, "n$"), "yes", "no")) %>% 
  group_by(vocal_ending, post_vocal_nasal) %>% 
  summarise(n = sum(n), .groups = "drop") %>%
  ungroup() %>% 
  mutate(vocal_ending = factor(vocal_ending, levels = c("é/e", "o")),
         post_vocal_nasal = if_else(post_vocal_nasal == "yes", "ya", "tidak")) %>% 
  ggplot(aes(x = post_vocal_nasal, 
             y = n, 
             fill = vocal_ending, 
             group = post_vocal_nasal)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Kemunculan akhiran nasal", 
       y = "Proporsi", 
       fill = "Varian vokal akhir",
       caption = "Nilai dalam diagram adalah frekuensi kemunculan") +
  geom_text(aes(label = n), 
            position = position_fill(.5), 
            colour = c("white", "white", "black", "black")) +
  theme_bw() + 
  scale_fill_grey(start = 0.1) +
  theme(legend.position = 'top')
fig2
fig2 + ggsave(filename = "fig2-distribusi-akhiran-nasal.png", 
              width = 6.5, height = 5.5, dpi = 300)

## Chi-Square Test bentuk dasar ========
chisq.test(ANGGO, correct = FALSE)
chisq.test(ANGGO, correct = FALSE)$expected # keluarkan frekuensi harapan

## Odds Ratio bentuk dasar =======
round(fisher.test(ANGGO)$estimate, 2)

## Phi coefficient bentuk dasar =========
round(unname(sqrt(chisq.test(ANGGO, correct = FALSE)$statistic/sum(ANGGO))), 1)

## Figure 3. Association plot ========
png("fig3-diagram-asosiasi-bentuk-dasar.png", 
    width = 6.5, height = 6, units = "in", res = 600)
assoc(ANGGO, shade = TRUE, 
      labeling_args = list(set_varnames = c(post_vokal_nasal = "Akhiran Nasal?", 
                                            vokal_akhir = "Vokal Akhir")))
dev.off()

# Analisis bentuk turunan =========
ANGGO_derived <- angg %>% 
  filter(str_detect(word, '^[^a]'), str_detect(word, "^pr[ae]", negate = TRUE)) %>% 
  as.data.frame() %>% 
  mutate(vocal_ending = if_else(str_detect(word, "(?<=g)[ée]", negate = FALSE), 
                                "é/e", "o"),
         post_vocal_nasal = if_else(str_detect(word, "n$"), "ya", "others"),
         post_vocal_nasal = if_else(post_vocal_nasal == "others" & 
                                      str_detect(word, "gg[éeo]$"), 
                                    "tidak", post_vocal_nasal),
         post_vocal_nasal = if_else(post_vocal_nasal == "others" & 
                                      str_detect(word, "gg[éeo](?=nn[ée]$)"), 
                                    "ya", post_vocal_nasal),
         post_vocal_nasal = if_else(post_vocal_nasal == "others" & 
                                      str_detect(word, "gg[éeo](?=n[ée]$)"), 
                                    "tidak", post_vocal_nasal),
         post_vocal_nasal = replace(post_vocal_nasal, 
                                    word == "panganggénnyané", "ya")) %>% 
  filter(!word %in% c("panganggénipuné", "panganggénnyané"))

ANGGO_derived_mtx <- ANGGO_derived %>% 
  group_by(vocal_ending, post_vocal_nasal) %>% 
  summarise(n = sum(n), .groups = "drop") %>%
  ungroup() %>% 
  pivot_wider(names_from = post_vocal_nasal, values_from = n) %>% 
  data.frame(row.names = 1) %>% 
  as.matrix()
(dimnames(ANGGO_derived_mtx) <- list(vokal_akhir = c("é/e", "o"), 
                                     post_vokal_nasal = c("tidak", "ya")))
(ANGGO_derived_mtx <- ANGGO_derived_mtx[c(2,1), ])

## Chi-Square test untuk data bentuk turunan ==========
chisq.test(ANGGO_derived_mtx, correct = FALSE)
chisq.test(ANGGO_derived_mtx, correct = FALSE)$expected # tampilkan frekuensi harapan

## Odds Ratio untuk data bentuk turunan =======
round(fisher.test(ANGGO_derived_mtx)$estimate, 2)

## Phi-Coefficient untuk data bentuk turunan ==========
round(unname(sqrt(chisq.test(ANGGO_derived_mtx, correct = FALSE)$statistic/sum(ANGGO_derived_mtx))), 2)

## Figure 4. Association plot untuk bentuk turunan ==========
png("fig4-diagram-asosiasi-bentuk-turunan.png", 
    width = 6.5, height = 6, units = "in", res = 600)
assoc(ANGGO_derived_mtx, shade = TRUE, 
      labeling_args = list(set_varnames = c(post_vokal_nasal = "Akhiran Nasal?", 
                                            vokal_akhir = "Vokal Akhir")))
dev.off()

# Analisis Kolokat Khas ==========
anggOn_tb_dca <- readr::read_tsv("anggOn_dca_input.txt")
anggEn_tb_dca <- readr::read_tsv("anggEn_dca_input.txt")
ANGGO_distcoll <- merge(anggOn_tb_dca, anggEn_tb_dca, by = 'collocs', all = TRUE)

ANGGO_distcoll[is.na(ANGGO_distcoll)] <-0
ANGGO_distcoll <- subset(ANGGO_distcoll, 
                         !collocs %in% c("anggon", "anggén", "anggo", 
                                         "anggen", "angge", "anggé"))
ANGGO_distcoll <- ANGGO_distcoll %>% 
  rowwise() %>% 
  mutate(n_coll = sum(anggon, anggen)) %>% 
  ungroup() %>% 
  mutate(n_anggon = sum(anggon),
         n_anggen = sum(anggen),
         n_coll_anggen_others = n_anggen - anggen,
         n_coll_anggon_others = n_anggon - anggon,
         N = n_anggon + n_anggen,
         a_exp = (n_anggen * n_coll)/N,
         assoc = if_else(anggen > a_exp, "greater", "less"),
         p_fye = if_else(assoc == 'greater',
                         pmap_dbl(list(anggen, n_coll, n_anggon, n_anggen), 
                                  function(anggen,n_coll,n_anggon, n_anggen) 
                                    sum(dhyper(anggen:n_coll, n_anggen, n_anggon, n_coll))),
                         pmap_dbl(list(anggen, n_coll, n_anggon, n_anggen),
                                  function(anggen,n_coll,n_anggon,n_anggen) 
                                    sum(dhyper(0:anggen, n_anggen, n_anggon, n_coll))))) %>% 
  rowwise() %>% 
  mutate(OR = if_else(assoc == "greater",
                      (anggen/n_coll_anggen_others)/(anggon/n_coll_anggon_others),
                      (anggon/n_coll_anggon_others)/(anggen/n_coll_anggen_others)))

## menyimpan hasil DCA utuh =========
# readr::write_tsv(ANGGO_distcoll, "hasil_analisis_kolokat_khas_ANGGO.txt")

## menyimpan hasil DCA masing-masing varian =======
ANGGO_distcoll %>% 
  select(1:3, assoc, p_fye) %>% 
  filter(assoc == 'less', p_fye < 0.01) %>% 
  mutate(CollStr = round(-log10(p_fye), 2)) %>% 
  arrange(p_fye) %>% 
  select(-assoc) # %>% 
  # write_tsv("hasil_kolokat_khas_signifikan_ANGGON.txt")
ANGGO_distcoll %>% 
  select(1:3, assoc, p_fye) %>% 
  filter(assoc == 'greater', p_fye < 0.01) %>% 
  mutate(CollStr = round(-log10(p_fye), 2)) %>% 
  arrange(p_fye) %>% 
  select(-assoc) # %>% 
  # write_tsv("hasil_kolokat_khas_signifikan_ANGGEN.txt")

## Analisis total nilai CollStr per tingkatan bahasa ============
anggen <- readr::read_tsv("hasil_kolokat_khas_signifikan_ANGGEN.txt")
anggen_register <- anggen %>% 
  filter(!is.na(tingkatan)) %>% 
  group_by(tingkatan) %>% 
  summarise(`Jumlah tipe kolokat` = n_distinct(collocs), 
            SumCollStr = sum(CollStr), 
            .groups = "drop") %>% 
  as.data.frame() %>% 
  arrange(desc(SumCollStr)) %>% 
  readr::write_tsv("hasil_kolokat_khas_ragam_ANGGEN.txt")

anggon <- readr::read_tsv("hasil_kolokat_khas_signifikan_ANGGON.txt")
anggon_register <- anggon %>% 
  filter(!is.na(tingkatan)) %>% 
  .[1:30, ] %>% 
  group_by(tingkatan) %>% 
  summarise(`Jumlah tipe kolokat` = n_distinct(collocs), 
            SumCollStr = sum(CollStr), 
            .groups = "drop") %>% 
  as.data.frame() %>% 
  arrange(desc(SumCollStr)) %>% 
  readr::write_tsv("hasil_kolokat_khas_ragam_ANGGON.txt")

# Figure 5. ================
anggon_register %>% 
  mutate(varian = "anggo(n)") %>% 
  bind_rows(anggen_register %>% 
              mutate(varian = "anggé/e(n)")) %>% 
  ggplot(aes(x = varian, y = SumCollStr, fill = tingkatan)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_grey(end = .9) + 
  geom_text(aes(label = `Jumlah tipe kolokat`), 
            position = position_dodge(.9), 
            vjust = 1.15, 
            colour = c("white", "black", "black", "white", "white", "white")) +
  theme_bw() + 
  labs(caption = "Nilai dalam diagram adalah jumlah kolokat khas untuk tiap-tiap ragam", 
       fill = "Ragam", 
       y = "Total CollStr per Ragam", x = "Varian") + 
  theme(legend.position = "top") +  
  ggsave(filename = "fig5-distribusi-total-CollStr-per-ragam.png", 
         width = 6.5, height = 5.5, dpi = 300)
