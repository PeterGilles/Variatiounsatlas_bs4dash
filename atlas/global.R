library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(cowplot)
library(scales)
library(DT)
library(shiny)
library(jpeg)
library(raster)
library(mapproj)
library(magick)
library(memoise)
library(googledrive)
library(googlesheets4)
library(partykit)
library(bs4Dash)
library(waiter)

shinyOptions(cache = cachem::cache_disk("cache"))

preloader <- list(html = tagList(spin_1(), "Loading ..."), color = "#343a40")

## Prepare stuff
# Polygoner fir Kanonten a Gemengen
cantons_df <- readRDS("cantons_df.RDS")
communes_df <- readRDS("communes_df.RDS")

color_palette <- c('#56cc9d','#beaed4','#fdc086','#ffff99','#386cb0','#f0027f','#bf5b17','#666666')

# load directly from Google Docs    
gs4_deauth()
variables <- range_read(ss = "1IDtvxHgccWg2JMu-dqJyqsCpwsurpYLOgm6rfE0mMGU", sheet = "kaartesettings", col_names=T, col_types = "c") %>%
  filter(active == "yes") %>%
  arrange(map_category, input_choice)

## Functions
make_choices <- function(category) {
  choices <- variables %>%
    #filter(map_category == category) %>%
    #dplyr::select(variable, input_choice) %>%
    arrange(input_choice)
  choices <- set_names(choices$variable, choices$input_choice)
  # selectInput(inputId = category, label = h3(category),
  #             choices = choices,
  #             selectize = FALSE,
  #             size = 25)
}

prepare_data <- function(variable, geo_type, selection) {
  
  longer_google_df <- google_df %>%
    pivot_longer(cols = {{variable}},
                 names_to = "variable",
                 values_to = "variants") %>%
    pivot_longer(cols = c("Gemeng_alt"),
                 names_to = "geo_type",
                 values_to = "geo_name") %>%
    mutate(variants = factor(variants, ordered = TRUE)) 
  
  variants_total <- longer_google_df %>%
    filter(geo_type == geo_type) %>%
    group_by(geo_name, {{variable}}, variants) %>%
    count() %>%
    group_by(geo_name, {{variable}})%>%
    drop_na(variants) %>%
    filter(variants %in% selection) %>%
    mutate(total = sum(n)) %>%
    mutate(freq = n/total)
  
  color <- color_palette
  color_column <<- tribble(~variants, ~color,
                           selection[1], color[1],
                           selection[2], color[2],
                           selection[3], color[3],
                           selection[4], color[4],
                           selection[5], color[5],
                           selection[6], color[6],
                           selection[7], color[7],
                           selection[8], color[8])
  
  variants_total <- left_join(variants_total, color_column, copy = TRUE)
  
  inner_join(communes_df, variants_total, by = c("id"= "geo_name"))
  
}

prepare_data_m <- memoise(prepare_data)

# Function Iwwerbléckskaart
make_summary_plot <- function(dataset, lsa_map_number, selection, color_num, map_title, item_number ="", item_text = "") {
  
  color <- color_palette[1:color_num]
  # TODO remove '_' from selection
  #selection <- str_replace(selection, "_", "")
  
  # get count of all observations
  variant_count <- dataset %>% distinct(id, total)
  variant_count <- sum(variant_count$total)
  
  dataset <- dataset %>%
    group_by(id) %>%
    filter(n == max(n)) %>%
    mutate(max_variant = variants)
  
  p <- ggplot(data = dataset) +
    geom_polygon(aes(x = long, y = lat, fill=max_variant,
                     group = id),
                 size=0, alpha = 0.9) +
    geom_polygon(data = cantons_df, aes(x = long, y = lat, group = id),
                 size= .1, colour = "black", fill = NA) +
    coord_map() +
    #scale_colour_identity() +
    scale_fill_manual(values = color[1:color_num], breaks = selection) +
    labs(title = paste(word(map_title, 2, sep="_")),
         fill = paste("Main Variant"),
         x = "", y = "",
         caption = paste0(variant_count, " Participants | Schnëssen-Item ", item_number, "\n", item_text)) +
    #theme_fira() +
    theme_void() +
    theme(plot.title = element_text(size=16, hjust = 0.5),
          plot.caption = element_text(size=11),
          legend.position =  c(0.85, 0.75),
          legend.text = element_text(size=13),
          legend.title = element_text(size=14))
  
  # plot with or without LSA
  # with LSA
  if(lsa_map_number != "NO") {
    # prepare LSA map into grid
    # für Docker
    #lsa <- readJPEG(source =  paste0("atlas/Kaarten-LSA_small/", lsa_map_number, "_lux.jpg"))
    lsa <- readJPEG(source = paste0("./Kaarten-LSA_small/", lsa_map_number, "_lux.jpg"))
    lsa <- ggdraw() + draw_image(lsa) +
      #library(grid)
      #lsa <- rasterGrob(lsa, interpolate=TRUE) +
      labs(caption ="Vergläichskaart aus dem 'Luxemburgischer Sprachatlas', LSA (1963)") +
      theme_void() +
      theme(plot.caption = element_text(size=11))
    
    plot_row <- cowplot::plot_grid(p, lsa,
                                   nrow = 1,
                                   ncol = 2)
  } else {
    # without LSA
    plot_row <- p
  }
  
  # to save map as pdf and png
  #ggsave(plot = plot_row, filename = paste0(map_title, ".png"), units = "cm", width = 22)
  #ggsave(plot = p, filename = paste0(map_title, ".pdf"), units = "cm", width = 22)
  #ggsave(plot = plot_row, filename = paste0(map_title, "_mat_LSA.pdf"), units = "cm", width = 28)
  #saveRDS(plot_row, file = paste0(map_title, ".RDS"))
  
  print(plot_row)
}

make_summary_plot_m <- memoise(make_summary_plot)

### Function Variantekaarten
make_plot <- function(dataset, variable, color) {
  
  # get count of all observations
  variant_count <- dataset %>% 
    distinct(id, n) %>% 
    summarize(sum(n))
  
  ggplot() +
    geom_polygon(data = dataset, aes(x = long, y = lat, fill= freq, 
                                     group = id), size=0, alpha = 0.9, colour = "lightgrey") +
    geom_polygon(data = cantons_df, aes(x = long, y = lat, group = id), 
                 size= .1, colour = "black", fill = NA) +
    coord_map() +
    scale_fill_gradient(guide = guide_legend(), low = "white", high = color,
                        name = paste0("pro Gemeng (Total: ", variant_count, ")"), na.value="white", limits=0:1, labels = percent(0.25*0:4)) +
    labs(title = str_replace({{variable}}, "_", " "), x = "", y = "") +
    theme_void() +
    theme(plot.title = element_text(size=16, hjust = 0.5),
          legend.position = "bottom", # c(0.85, 0.75)
          legend.text = element_text(size=11),
          legend.title = element_text(size=12),
          plot.margin=unit(c(0.2, 0, 0.2, 0), "cm") ) +
    guides(fill = guide_colourbar(barwidth = 7, barheight = .7, ticks = FALSE, title.position = "bottom", title.hjust = 0.5)) 
  
  # to save maps as pdf and png
  #ggsave(filename = paste0(str_replace(variable, "/", "_"), ".png"), units = "cm", width = 22)
  #ggsave(filename = paste0(str_replace(variable, "/", "_"), ".pdf"), units = "cm", width = 22)
}

make_plot_m <- memoise(make_plot)

# Function for Sozialdaten
plot_social_categories <- function(social_category, variable, selection, caption = "") {
  
  color <- color_palette[1:length(selection)]
  
  bar_df <- google_df %>%
    filter(Geschlecht != "Aner") %>%
    mutate(Ausbildung = recode(Ausbildung, 
                               `Etudes techniques supérieures (BTS, brevet de maîtrise)` = "Lycée technique",
                               `13e/14e (Lycée technique)` = "Lycée technique",
                               `CCP/DAP (CATP)` = "Lycée technique",
                               `9e (Lycée technique)` = "Lycée technique",
                               `Primaire/Grondschoul` = "just Primaire",
                               `5e (Lycée classique)` = "Lycée classique",
                               `1e (Lycée classique)` = "Lycée classique" )) %>%
    mutate(Ausbildung = factor(Ausbildung, levels = c("just Primaire", "Lycée technique", "Lycée classique", "Fachhéichschoul/Universitéit"))) %>%
    dplyr::rename(`Kompetenz am Däitschen` = Kompetenz_Deutsch) %>%
    dplyr::rename(`Kompetenz am Franséischen` = Kompetenz_Französisch) %>%
    dplyr::select({{social_category}}, variable = {{variable}}) %>%
    filter(variable %in% selection) %>%
    drop_na() %>%
    group_by({{social_category}}, variable) %>%
    count() %>%
    group_by({{social_category}}) %>%
    mutate(total = sum(n)) %>%
    mutate(Prozent = n/total) %>%
    dplyr::rename(count_variant = n) %>%
    group_by({{social_category}}) %>%
    mutate(total_alter = sum(count_variant)) %>%
    mutate(Prozent = count_variant/total_alter) %>%
    ungroup() %>%
    mutate(total_variable = sum(count_variant)) %>%
    mutate(Prozent_variable = count_variant/total_variable)  %>%
    dplyr::select(-total_alter, -total_variable) %>%
    group_by({{social_category}}) %>%
    mutate(Prozent_social_category = sum(Prozent_variable))
  
  #bar_df$w <- cumsum(bar_df$total)
  #bar_df$wm <- bar_df$w - bar_df$total
  #bar_df$wt <- with(bar_df, wm + (w - wm)/2)
  
  # # Groups:   Alter [6]
  #    Alter     variable count_variant total Prozent
  #    <chr>     <chr>            <int> <int>   <dbl>
  #  1 ≤ 24      lo[g]ie             40   224  0.179 
  #  2 ≤ 24      lo[ʒ]ie            184   224  0.821 
  #  3 25 bis 34 lo[g]ie             45   305  0.148 
  #  4 25 bis 34 lo[ʒ]ie            260   305  0.852 
  #  5 35 bis 44 lo[g]ie             11   179  0.0615
  #  6 35 bis 44 lo[ʒ]ie            168   179  0.939 
  #  7 45 bis 54 lo[g]ie             20   183  0.109 
  #  8 45 bis 54 lo[ʒ]ie            163   183  0.891 
  #  9 55 bis 64 lo[g]ie              8   123  0.0650
  # 10 55 bis 64 lo[ʒ]ie            115   123  0.935 
  # 11 65+       lo[g]ie              2    40  0.05  
  # 12 65+       lo[ʒ]ie             38    40  0.95  
  # # A tibble: 4 x 5
  # # Groups:   Geschlecht [2]
  #   Geschlecht variable count_variant total Prozent
  #   <chr>      <chr>            <int> <int>   <dbl>
  # 1 Männlech   lo[g]ie             31   328  0.0945
  # 2 Männlech   lo[ʒ]ie            297   328  0.905 
  # 3 Weiblech   lo[g]ie             95   726  0.131 
  # 4 Weiblech   lo[ʒ]ie            631   726  0.869 
  
  # Stacked barplot with multiple groups
  ggplot(data=bar_df, aes(x= {{social_category}}, y = Prozent, fill = variable, label = count_variant), alpha = 0.9) +
    geom_col(position = position_stack()) +
    # with proportional bar widths
    #  geom_col(position = position_stack(), aes(width = Prozent_social_category * 2)) +
    #geom_rect(position = position_stack(), aes(xmin = wm, xmax = w,
    #   ymax = Prozent, fill = variable)) +
    geom_text(position = position_stack(vjust = .5), size = 5, alpha = 0.9) +
    labs(fill = word(variable, 2, sep="_"),
         caption = caption) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(values = color[1:length(selection)], breaks = selection) +
    theme_minimal() +
    theme(legend.text = element_text(size=14),
          legend.position="bottom",
          legend.title = element_blank(),
          axis.title = element_text(size=15),
          axis.text = element_text(size=14))
}

# Function for data table
plot_datatable <- function(variable) {
  datatable(google_df %>%
              dplyr::select(Gemeng_alt, Variant = as.name({{variable}}), recordingURL, Kanton, Dialektgebitt = Dialektgebiet) %>%
              dplyr::filter(Variant != "FALSE") %>%
              mutate(Lauschtert = paste0("<audio controls preload=\"none\" type=\"audio/wav\" src=\"", recordingURL, "\"> </audio>")) %>%
              dplyr::select(-recordingURL) %>%
              dplyr::select(Variant, Lauschtert, Dialektgebitt, Gemeng = Gemeng_alt, Kanton),
            escape = FALSE,
            height = 600,
            extensions = 'Scroller',
            filter = 'top', options = list(
              deferRender = TRUE,
              scrollY = 200,
              scroller = TRUE,
              autoWidth = TRUE
            ))
}

# Function for decision tree
plot_decision_tree <- function(variable, selection) {
  
  cond_df <-  google_df %>%
    filter(Muttersprache != "Neen") %>%
    filter(Geschlecht != "Aner") %>%
    mutate(Ausbildung = recode(Ausbildung, 
                               `Etudes techniques supérieures (BTS, brevet de maîtrise)` = "Lycée technique",
                               `13e/14e (Lycée technique)` = "Lycée technique",
                               `CCP/DAP (CATP)` = "Lycée technique",
                               `9e (Lycée technique)` = "Lycée technique",
                               `Primaire/Grondschoul` = "just Primaire",
                               `5e (Lycée classique)` = "Lycée classique",
                               `1e (Lycée classique)` = "Lycée classique" )) %>%
    mutate(Ausbildung = factor(Ausbildung, levels = c("just Primaire", "Lycée technique", "Lycée classique", "Fachhéichschoul/Universitéit"))) %>%
    dplyr::select(variable = {{variable}}, Alter, Geschlecht, Dialektgebiet, Ausbildung) %>%
    dplyr::filter((variable) %in% selection) %>%
    # filter variants above a certain frequency level
    group_by(variable) %>%
    filter(n()/nrow(google_df) >= 0.02) %>%
    ungroup() %>%
    na.omit() 
  
  cond_df[sapply(cond_df, is.character)] <- lapply(cond_df[sapply(cond_df, is.character)], as.factor)
  
  set.seed(124)
  # '~ . = take all variables available, if not single variables have to be listed
  # ~ . works only, when all variables a factors (not character)
  
  cond_tree <- ctree(formula = variable ~ Alter + Geschlecht + Dialektgebiet + Ausbildung,
                     data = cond_df, 
                     control = ctree_control(testtype = "Univariate", minbucket = 20))
  
  # standard plot
  plot(cond_tree)
  
  # # ggparty
  # library(ggparty)
  # ggparty(cond_tree) +
  #   geom_edge() +
  #   geom_edge_label() +
  #   geom_node_splitvar() +
  #   # pass list to gglist containing all ggplot components we want to plot for each
  #   # (default: terminal) node
  #   geom_node_plot(gglist = list(geom_bar(aes(x = "", fill = play),
  #                                         position = position_fill()),
  #                                xlab("play")))
  
  # decision rules
  # how to display them?
  # fit <- rpart::rpart(
  #   formula = variable ~ Alter + Geschlecht + Dialektgebiet,
  #   data = cond_df,
  #   method = "class",
  #   control = rpart::rpart.control(cp = 0.05)
  # )
  # party_obj <- as.party.rpart(fit, data = TRUE)
  # decisions <- partykit:::.list.rules.party(party_obj)
  # 
  # paste(decisions, collapse = "\n")
}
