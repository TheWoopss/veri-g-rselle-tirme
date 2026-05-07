# ============================================================
#  Turkiye Sosyal Koruma Harcamalari - 6 Grafik + Poster
#  Paketler: ggplot2, plotly, readxl, dplyr, tidyr, scales,
#            patchwork, ggtext
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(scales)
library(patchwork)
library(ggtext)
library(ggrepel)

# ------------------------------------------------------------
# 0. DOSYA YOLLARI - kendi dizininize gore duzenleyin
# ------------------------------------------------------------
dosya_t1 <- "t1_yardim_turleri_2000_2024.xlsx"
dosya_t2 <- "t2_sartli_sartsiz_2000_2024.xlsx"

# ------------------------------------------------------------
# 1. VERI OKUMA VE TEMIZLEME
# ------------------------------------------------------------
ana_kategoriler <- c(
  "Emekli/yasli",
  "Hastalik/saglik bakimi",
  "Dul/yetim",
  "Aile/cocuk",
  "Engelli/malul",
  "Issizlik",
  "Sosyal dislanma b.y.s"
)

etiketler <- c(
  "Emekli/yasli"           = "Emekli/Yasli",
  "Hastalik/saglik bakimi" = "Hastalik/Saglik",
  "Dul/yetim"              = "Dul/Yetim",
  "Aile/cocuk"             = "Aile/Cocuk",
  "Engelli/malul"          = "Engelli/Malul",
  "Issizlik"               = "Issizlik",
  "Sosyal dislanma b.y.s"  = "Sosyal Dislanma"
)

renkler <- c(
  "Emekli/Yasli"    = "#378ADD",
  "Hastalik/Saglik" = "#1D9E75",
  "Dul/Yetim"       = "#D85A30",
  "Aile/Cocuk"      = "#BA7517",
  "Engelli/Malul"   = "#7F77DD",
  "Issizlik"        = "#D4537E",
  "Sosyal Dislanma" = "#888780"
)

# T1: genis formattan uzun formata
t1_raw <- read_excel(dosya_t1, sheet = "data")

t1_long <- t1_raw %>%
  filter(Kategori %in% ana_kategoriler) %>%
  mutate(Kategori = recode(Kategori, !!!etiketler)) %>%
  pivot_longer(
    cols      = starts_with("Y_"),
    names_to  = "Yil",
    values_to = "Harcama_milyon"
  ) %>%
  mutate(
    Yil            = as.integer(sub("Y_", "", Yil)),
    Harcama_milyar = Harcama_milyon / 1000
  )

# T2: 2024 kesiti
t2_raw <- read_excel(dosya_t2, sheet = "data")

t2_2024 <- t2_raw %>%
  filter(Kategori %in% ana_kategoriler) %>%
  mutate(Kategori = recode(Kategori, !!!etiketler)) %>%
  transmute(
    Kategori,
    Sartli  = Sartli_2024  / 1000,
    Sartsiz = Sartsiz_2024 / 1000,
    Toplam  = Sartli + Sartsiz
  )

# ============================================================
# GRAFIK 1 - Harcama Trendi (logaritmik)
# ============================================================
g1 <- ggplot(t1_long, aes(x = Yil, y = Harcama_milyar,
                          color = Kategori, group = Kategori)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.2, alpha = 0.6) +
  scale_y_log10(
    labels = label_number(accuracy = 1, suffix = " mr TL"),
    breaks = c(0.01, 0.1, 1, 10, 100, 1000)
  ) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 4)) +
  scale_color_manual(values = renkler) +
  labs(
    title   = "Sosyal Yardim Turleri - Harcama Trendi (2000-2024)",
    subtitle = "Yardim kategorilerine gore yillik harcama - Logaritmik olcek",
    x       = NULL,
    y       = "Milyar TL (log)",
    color   = NULL,
    caption = "Kaynak: t1_yardim_turleri_2000_2024.xlsx"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "bottom",
    legend.key.width = unit(1.5, "cm"),
    panel.grid.minor = element_blank()
  )

print(g1)
# ggsave("grafik1_trend.png", g1, width = 10, height = 6, dpi = 150)


# ============================================================
# GRAFIK 3 - Kategori Paylari Egim Grafigi
# ============================================================
pay_data <- t1_long %>%
  filter(Yil %in% c(2000, 2024)) %>%
  group_by(Yil) %>%
  mutate(Pay = Harcama_milyar / sum(Harcama_milyar) * 100) %>%
  ungroup() %>%
  select(Kategori, Yil, Pay) %>%
  mutate(Yil = factor(Yil))

g3 <- ggplot(pay_data, aes(x = Yil, y = Pay,
                           color = Kategori, group = Kategori)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  geom_text(
    data = pay_data %>% filter(Yil == 2000),
    aes(label = paste0("%", round(Pay, 1))),
    hjust = 1.3, size = 3.2, fontface = "bold"
  ) +
  geom_text(
    data = pay_data %>% filter(Yil == 2024),
    aes(label = paste0("%", round(Pay, 1), "  ", Kategori)),
    hjust = -0.1, size = 3.2, fontface = "bold"
  ) +
  scale_color_manual(values = renkler) +
  scale_y_continuous(labels = function(x) paste0("%", x)) +
  expand_limits(x = c(0.6, 2.9)) +
  labs(
    title   = "Kategori Paylarinin 24 Yillik Degisimi",
    subtitle = "Sosyal yardimlar toplami icindeki yuzde pay (2000 -> 2024)",
    x       = NULL,
    y       = "Toplam icindeki pay (%)",
    caption = "Kaynak: t1_yardim_turleri_2000_2024.xlsx"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank()
  )

print(g3)
# ggsave("grafik3_egim.png", g3, width = 8, height = 7, dpi = 150)

# ============================================================
# GRAFIK 4 - Sartli/Sartsiz Konumlanma Haritasi (ggplot2)
# ============================================================
g4 <- ggplot(t2_2024, aes(x = Sartli, y = Sartsiz)) +
  geom_point(aes(size = Toplam, fill = Kategori),
             shape = 21, color = "white", stroke = 0.8, alpha = 0.85) +
  scale_fill_manual(
    values = renkler,
    name   = "Kategori",
    guide  = guide_legend(
      override.aes   = list(size = 4, alpha = 1, shape = 21, color = "white"),
      title.position = "top",
      nrow           = 7
    )
  ) +
  scale_size_continuous(
    range  = c(5, 35),
    name   = "Toplam harcama (mr TL)",
    breaks = c(50, 200, 500, 1000, 2000),
    guide  = guide_legend(
      override.aes   = list(fill = "grey60", color = "white", alpha = 0.7,
                            shape = 21, size = c(2, 3, 4, 5, 6)),
      title.position = "top"
    )
  ) +
  scale_x_continuous(labels = label_number(suffix = " mr TL"),
                     expand = expansion(mult = 0.15)) +
  scale_y_continuous(labels = label_number(suffix = " mr TL"),
                     expand = expansion(mult = c(0.1, 0.3))) +
  labs(
    title   = "Sartli - Sartsiz Konumlanma Haritasi (2024)",
    subtitle = "Balon buyuklugu toplam harcamayi gostermektedir",
    x       = "Sartli harcama (Milyar TL)",
    y       = "Sartsiz harcama (Milyar TL)",
    caption = "Kaynak: t2_sartli_sartsiz_2000_2024.xlsx"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "right",
    legend.box       = "vertical",
    legend.spacing.y = unit(0.5, "cm"),
    panel.grid.minor = element_blank()
  )

print(g4)
# ggsave("grafik4_bubble.png", g4, width = 8, height = 7, dpi = 150)

# ============================================================
# GRAFIK 5 - Sosyal Koruma Harcamalarinin Risk Gruplarina Gore Dagilimi
#            Yigilmis cubuk grafik (2000-2024)
# ============================================================
risk_kategoriler <- c(
  "Aile/cocuk",
  "Emekli/yasli",
  "Hastalik/saglik bakimi",
  "Issizlik"
)

risk_etiketler <- c(
  "Aile/cocuk"             = "Aile/cocuk",
  "Emekli/yasli"           = "Emekli/yasli",
  "Hastalik/saglik bakimi" = "Hastalik/saglik bakimi",
  "Issizlik"               = "Issizlik"
)

risk_renkler <- c(
  "Aile/cocuk"             = "#4472C4",
  "Emekli/yasli"           = "#E8604C",
  "Hastalik/saglik bakimi" = "#70AD47",
  "Issizlik"               = "#C39BD3"
)

risk_data <- t1_raw %>%
  filter(Kategori %in% risk_kategoriler) %>%
  pivot_longer(
    cols      = starts_with("Y_"),
    names_to  = "Yil",
    values_to = "Harcama_milyon"
  ) %>%
  mutate(
    Yil      = as.integer(sub("Y_", "", Yil)),
    Kategori = factor(Kategori, levels = risk_kategoriler)
  )

g5 <- ggplot(risk_data, aes(x = factor(Yil), y = Harcama_milyon, fill = Kategori)) +
  geom_col(width = 0.75) +
  scale_fill_manual(
    values = risk_renkler,
    name   = NULL,
    labels = risk_etiketler
  ) +
  scale_y_continuous(
    labels = label_number(suffix = " TL", big.mark = ".", decimal.mark = ",", scale = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(
    title   = "Sosyal Koruma Harcamalarinin Risk Gruplarina Gore Dagilimi",
    x       = "Yil",
    y       = "Harcama Tutari (Milyon TL)",
    caption = "Kaynak: t1_yardim_turleri_2000_2024.xlsx"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "right",
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(size = 9)
  )

print(g5)
# ggsave("grafik5_yigilmis_cubuk.png", g5, width = 11, height = 7, dpi = 150)

# ============================================================
# GRAFIK 6 - Kategorilere Gore Harcama Dagilimi (Box Plot)
#            Logaritmik olcek - 2000-2024
# ============================================================
g6 <- ggplot(t1_long, aes(x = reorder(Kategori, Harcama_milyar, FUN = median),
                          y = Harcama_milyar,
                          fill = Kategori)) +
  geom_boxplot(alpha = 0.75, outlier.shape = 21,
               outlier.fill = "white", outlier.size = 2,
               outlier.stroke = 0.6) +
  geom_jitter(aes(color = Kategori), width = 0.15,
              size = 1.2, alpha = 0.45) +
  scale_y_log10(
    labels = label_number(accuracy = 0.1, suffix = " mr TL"),
    breaks = c(0.01, 0.1, 1, 10, 100, 1000)
  ) +
  scale_fill_manual(values  = renkler, guide = "none") +
  scale_color_manual(values = renkler, guide = "none") +
  coord_flip() +
  labs(
    title   = "Sosyal Yardim Kategorilerinin Harcama Dagilimi (2000-2024)",
    subtitle = "Her kutu 25 yillik dagilimi gostermektedir - Logaritmik olcek",
    x       = NULL,
    y       = "Milyar TL (log)",
    caption = "Kaynak: t1_yardim_turleri_2000_2024.xlsx"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

print(g6)
# ggsave("grafik6_boxplot.png", g6, width = 10, height = 6, dpi = 150)

# ============================================================
# GRAFIK 7 - Sacilim Grafigi (Scatter Plot)
#            Yil (X) vs Harcama (Y), logaritmik olcek - 2000-2024
# ============================================================
g7 <- ggplot(t1_long, aes(x = Yil, y = Harcama_milyar,
                          color = Kategori, fill = Kategori)) +
  geom_point(shape = 21, size = 3, alpha = 0.85,
             color = "white", stroke = 0.5,
             aes(fill = Kategori)) +
  scale_y_log10(
    labels = label_number(accuracy = 0.1, suffix = " mr TL"),
    breaks = c(0.01, 0.1, 1, 10, 100, 1000, 2500)
  ) +
  scale_x_continuous(
    breaks = 2000:2024,
    guide  = guide_axis(angle = 45)
  ) +
  scale_fill_manual(values = renkler, name = NULL) +
  labs(
    title    = "Sosyal Yardim Turleri - Sacilim Grafigi (2000-2024)",
    subtitle = "Her nokta bir yil-kategori ciftini temsil eder - Logaritmik olcek",
    x        = NULL,
    y        = "Milyar TL (log)",
    caption  = "Kaynak: t1_yardim_turleri_2000_2024.xlsx"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "bottom",
    legend.key.width = unit(0.8, "cm"),
    panel.grid.minor = element_blank()
  )

print(g7)
# ggsave("grafik7_sacilim.png", g7, width = 11, height = 6, dpi = 150)

# ============================================================
# POSTER - A0 Dikey (84x119 cm) Akademik Poster
# ============================================================

# --- Grafikleri poster icin sadele ---
p1 <- g1 + labs(title    = "1. Harcama Trendi (2000-2024)",
                subtitle = "Logaritmik olcek, Milyar TL") +
  theme(plot.title    = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11),
        legend.text   = element_text(size = 9),
        axis.text     = element_text(size = 9))

p3 <- g3 + labs(title    = "3. Kategori Paylarinin Degisimi",
                subtitle = "Toplam icerisindeki yuzde pay (2000 vs 2024)") +
  theme(plot.title    = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11),
        axis.text     = element_text(size = 9))

p4 <- g4 + labs(title    = "4. Sartli - Sartsiz Konumlanma (2024)",
                subtitle = "Balon buyuklugu toplam harcamayi gostermektedir") +
  theme(plot.title    = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11),
        axis.text     = element_text(size = 9),
        legend.text   = element_text(size = 9))

p5 <- g5 + labs(title    = "5. Risk Gruplarina Gore Dagilim",
                subtitle = "Yigilmis cubuk grafik, Milyon TL") +
  theme(plot.title    = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11),
        axis.text     = element_text(size = 8),
        legend.text   = element_text(size = 9))

p6 <- g6 + labs(title    = "6. Harcama Dagilimi - Box Plot",
                subtitle = "25 yillik istatistiksel dagilim, Logaritmik olcek") +
  theme(plot.title    = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11),
        axis.text     = element_text(size = 9))

p7 <- g7 + labs(title    = "7. Sacilim Grafigi (2000-2024)",
                subtitle = "Yil vs harcama, logaritmik olcek") +
  theme(plot.title    = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11),
        legend.text   = element_text(size = 9),
        axis.text     = element_text(size = 8))

# --- Poster duzeni: 2 sutun x 4 satir (son satir tek grafik tam genislikte) ---
poster <- (p1 | p3) / (p4 | p5) / (p6 | p7) +
  plot_annotation(
    title    = "Turkiye'de Sosyal Koruma Harcamalarinin Yapisal Donusumu: 2000-2024",
    subtitle = "Mustafa KOSE  |  Sueda DEMIRASLAN  |  Aksaray Universitesi",
    caption  = "Veri kaynagi: t1_yardim_turleri_2000_2024.xlsx  &  t2_sartli_sartsiz_2000_2024.xlsx",
    theme = theme(
      plot.title    = element_text(size = 22, face = "bold",
                                   hjust = 0.5, margin = margin(b = 6)),
      plot.subtitle = element_text(size = 16, hjust = 0.5, color = "grey30",
                                   margin = margin(b = 10)),
      plot.caption  = element_text(size = 10, hjust = 0.5, color = "grey50"),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  )

# --- A0 dikey olarak kaydet (84 x 119 cm, 150 dpi) ---
ggsave(
  filename = "poster_sosyal_koruma_A0.pdf",
  plot     = poster,
  width    = 84,
  height   = 119,
  units    = "cm",
  dpi      = 150,
  device   = "pdf"
)

message("Poster kaydedildi: poster_sosyal_koruma_A0.pdf")