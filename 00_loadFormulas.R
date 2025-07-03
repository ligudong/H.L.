
fml_no_jiaohu <- str_c("GOSIF_sum ~
            Caod + Caod ^ 2 +
            Faod + Faod ^ 2 +
            cloud + cloud ^ 2 + AOT40 +
            ", add_terms(c(
              str_c("bin", 1:42),
              str_c("root_", 1:9),
              str_c("d", 1:13)
            )), "|
            x_y[year]") %>%
  as.formula()

fml_O3 <- str_c("GOSIF_sum ~ AOT40 |x_y[year]") %>%
  as.formula()

fml_base_lin <- str_c("GOSIF_sum ~
            Caod + Faod + cloud  + AOT40 + Faod*AOT40 +
            ", add_terms(c(
              str_c("bin", 1:42),
              str_c("root_", 1:9),
              str_c("d", 1:13)
            )), "|
            x_y[year]") %>%
  as.formula()


fml_base <- str_c("GOSIF_sum ~
            Caod + Caod ^ 2 +
            Faod + Faod ^ 2 +
            cloud + cloud ^ 2 + AOT40 + Faod*AOT40 + (Faod^ 2)*AOT40 +
            ", add_terms(c(
              str_c("bin", 1:42),
              str_c("root_", 1:9),
              str_c("d", 1:13)
            )), "|
            x_y[year]") %>%
  as.formula()


fml_base2 <- str_c("GOSIF_sum ~
            Caod + Caod ^ 2 +
            Faod + Faod ^ 2 +
            cloud + cloud ^ 2 + AOT40 + Faod*AOT40 + (Faod^ 2)*AOT40 + Caod*AOT40 +(Caod^ 2)*AOT40+
            ", add_terms(c(
              str_c("bin", 1:42),
              str_c("root_", 1:9),
              str_c("d", 1:13)
            )), "|
            x_y[year]") %>%
  as.formula()

fml_base_O3 <- str_c("GOSIF_sum ~
            Caod + Caod ^ 2 +
            Faod + Faod ^ 2 +
            cloud + cloud ^ 2 + O3+ Faod*O3 + (Faod^ 2)*O3 +
            ", add_terms(c(
              str_c("bin", 1:42),
              str_c("root_", 1:9),
              str_c("d", 1:13)
            )), "|
            x_y[year]") %>%
  as.formula()

fml_inter_region <- str_c(
  "GOSIF_sum ~ ",
  c(
    str_c("R", 1:6, "_Caod"), str_c("R", 1:6, "_Caod2"),  str_c("R", 1:6, "_Faod"), str_c("R", 1:6, "_Faod2"), str_c("R", 1:4, "_AOT40"), "cloud", "cloud^2",
    str_c("bin", 1:42),
    str_c("root_", 1:9),
    str_c("d", 1:13)
  ) %>%
    str_flatten(collapse = "+"), "| x_y[year]"
) %>%
  as.formula()
#------figure 2
fml_no_jiaohu2 <- str_c("GOSIF_sum ~
            Caod + Caod ^ 2 +
            Faod + Faod ^ 2 +
            cloud + cloud ^ 2 + O3 +
            ", add_terms(c(
              str_c("bin", 1:42),
              str_c("root_", 1:9),
              str_c("d", 1:13)
            )), "|
            x_y[year]") %>%
  as.formula()

fml_no_jiaohu3 <- str_c("GOSIF_sum ~
            Caod + Caod ^ 2 +
            Faod + Faod ^ 2 +
            cloud + cloud ^ 2 + AOT40 +Faod*AOT40+ (Faod^2)*AOT40 +
            ", add_terms(c(
              str_c("bin", 1:42),
              str_c("root_", 1:9),
              str_c("d", 1:13)
            )), "|
            x_y[year]") %>%
  as.formula()



fml_no_Faod <- str_c("GOSIF_sum ~
            Caod + Caod ^ 2 +
            cloud + cloud ^ 2 + AOT40 + 
            ", add_terms(c(
              str_c("bin", 1:42),
              str_c("root_", 1:9),
              str_c("d", 1:13)
            )), "|
            x_y[year]") %>%
  as.formula()

fml_no_O3 <- str_c("GOSIF_sum ~
            Caod + Caod ^ 2 +
            Faod + Faod ^ 2 +
            cloud + cloud ^ 2 + 
            ", add_terms(c(
              str_c("bin", 1:42),
              str_c("root_", 1:9),
              str_c("d", 1:13)
            )), "|
            x_y[year]") %>%
  as.formula()

fml_no_Caod <- str_c("GOSIF_sum ~
            Faod + Faod ^ 2 +
            cloud + cloud ^ 2 + AOT40 + Faod*AOT40 +  (Faod^ 2)*AOT40 +
            ", add_terms(c(
              str_c("bin", 1:42),
              str_c("root_", 1:9),
              str_c("d", 1:13)
            )), "|
            x_y[year]") %>%
  as.formula()


fml_base_w126 <- str_c("GOSIF_sum ~
            Faod + Faod ^ 2 + Caod + Caod ^ 2 +
            W126 +
            cloud + cloud ^ 2 +
            ", add_terms(c(
              str_c("bin", 1:42),
              str_c("surface_", 1:9),
              str_c("D", 1:13)
            )), "|
            x_y[year]") %>%
  as.formula()

fml_inter_i <- str_c(
  "GOSIF_sum ~ ",
  c(
    str_c(c(
      "Faod", "Faod ^ 2","Caod", "Caod ^ 2", "AOT40", "cloud", "cloud ^ 2",
      str_c("bin", 1:42),
      str_c("D", 1:13),
      str_c("surface_", 1:9)
    ), " * irg_fraction")
  ) %>% str_flatten(collapse = "+"), "| x_y[year]"
) %>%
  as.formula()
