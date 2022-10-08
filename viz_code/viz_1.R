# Loading data ------------------------------------------------------------
source(file = "viz_code/setup_pkgs_preliminaries.R")
source(file = "viz_code/etl_data-1.R")


# Treemap for visitor data ------------------------------------------
vstr_only <- dplyr::filter(vstr, 
                           indicators %in% c("Number of Visitors"))

vstr_only_rgn <- vstr_only |> 
  filter(year == 2021) |> 
  group_by(regions) |> 
  summarise(n = sum(value, na.rm = TRUE), .groups = "drop") |> 
  mutate(prop = n/sum(n, na.rm = TRUE)) |> 
  mutate(prop_lbl = str_c(regions, " ", round(prop*100), "%"))

tmap_by_gvrnt <- ggplot(data = vstr_only_rgn, aes(
  fill = regions, area = n, label = prop_lbl)
  ) +
  geom_treemap(show.legend = FALSE) +
  geom_treemap_text(reflow = TRUE, color = "white", size = 12) +
  theme_minimal(
    base_family = "cabinet_grotesk"
  ) +
  labs(
    title = "2021 tourist visitors in Oman",
    subtitle = "Proportion of visitors by Governorates"
  ) +
  theme(
    plot.title = element_textbox_simple(size = "13",face = "bold"),
    plot.margin = margin(6, 6, 3, 6)
  ) +
  scale_fill_paletteer_d(`"calecopal::kelp1"`)

tmap_by_gvrnt


# Plot 1 | Ad Dakhliyah ---------------------------------------------------
vstr_only_ad_dkh <- ggplot(data = filter(vstr_only, 
                                         regions %in% c("Ad Dakhliyah")),
                           aes(x = year, y = value, 
                             fill = reseves)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(
    base_family = "cabinet_grotesk"
  ) +
  labs(
    title = ("Ad Dakhliyah"),
    subtitle = c("Al Jabal Al Akhdar Sanctuary for Natural Sceneries"),
    y = c("No. of Visitors")
  ) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_markdown(face = "bold"),
    plot.title.position = "plot",
    plot.margin = margin(6, 6, 3, 6)
  )
vstr_only_ad_dkh
# Al Jabal Al Akhdar Sanctuary for Natural Sceneries
vstr_only_ad_dkh


# Plot 2 | Dhofar ---------------------------------------------------------
vstr_only_dfr <- ggplot(data = filter(vstr_only, 
                                         regions %in% c("Dhofar"),
                                         !year %in% c(2014, 2015),
                                      !reseves %in% c("Khawr Salalah Natural Reserve",
                                                      "Jabal Samhan Nature Reserve")
                                      ),
                           aes(x = year, y = value,
                               fill = reseves)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal(
    base_family = "cabinet_grotesk"
  ) + 
  labs(
    title = c("Dhofar"),
    y = c("No. of Visitors"),
    caption = c("Notes: Does not include Khawr Salalah Natural Reserve & Jabal Samhan Nature Reserve")
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_markdown(face = "bold"),
    legend.position = "top",
    plot.margin = margin(6, 6, 3, 6),
    plot.title.position = "plot"
  ) +
  guides(
    fill = guide_legend(size = 3, title = element_blank())
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_paletteer_d(`"calecopal::wetland"`)

vstr_only_dfr

# Plot 3 | Ash Sharqiyah South --------------------------------------------
vstr_only_ash_shr <- ggplot(data = filter(vstr_only, regions %in% c("Ash Sharqiyah South")),
                            aes(x = year, y = value, 
           fill = reseves)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal(
    base_family = c("cabinet_grotesk")
  ) + 
  theme(
    legend.position = "none",
    plot.margin = margin(6, 6, 3, 6),
    plot.title = element_textbox_simple(face = "bold"),
    plot.title.position = "plot",
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  ) + 
  scale_y_continuous(label = scales::comma) +
  scale_fill_paletteer_d(`"awtools::ppalette"`) +
  labs(
    title = c("Ash Sharqiyah South"),
    plot.title = element_markdown(face = "bold"),
    y = c("No. of Visitors")
  )

vstr_only_ash_shr


# Composition -------------------------------------------------------------
ptch_cmpos <- (tmap_by_gvrnt | vstr_only_ad_dkh) / (vstr_only_dfr | vstr_only_ash_shr) +
  patchwork::plot_annotation(title = ("Oman's visitors/tourist data summary by Governorates"),
                             subtitle = "Jabel e Akhdar (Dakhliyah) & Khor Al Baleed (Dhofar) were the most consistently visited place",
                             caption = "Note: Data Sourced from Open Data Oman (https://opendata.om/)",
                             theme = theme(
                               plot.title = element_text(face = "bold", size = 16)
                             )
                             )
  

ggsave(ptch_cmpos, filename = "ptch_cmpos.pdf", 
       path = "viz_plots", width = 11, height = 7, device = cairo_pdf)



ggsave(ptch_cmpos, filename = "ptch_cmpos.png", 
       path = "viz_plots", width = 11, height = 7)