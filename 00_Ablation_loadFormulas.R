fml_1 <- str_c("GOSIF_sum ~
            Caod + Caod ^ 2|x_y[year]") %>%
  as.formula()

fml_2 <- str_c("GOSIF_sum ~
            Caod + Caod ^ 2 + Faod + Faod ^ 2|x_y[year]") %>%
  as.formula()


fml_3 <- str_c("GOSIF_sum ~
            Caod + Caod ^ 2 + Faod + Faod ^ 2 + AOT40|x_y[year]") %>%
  as.formula()

fml_4 <- str_c("GOSIF_sum ~
            Caod + Caod ^ 2 + Faod + Faod ^ 2 + AOT40 +cloud + cloud^2|x_y[year]") %>%
  as.formula()


fml_5 <- str_c("GOSIF_sum ~
            Caod + Caod ^ 2 + Faod + Faod ^ 2 + AOT40 +cloud + cloud^2 + Faod*AOT40 +(Faod^2)*AOT40|x_y[year]") %>%
  as.formula()

fml_6 <- str_c("GOSIF_sum ~
            Caod + Caod ^ 2 +
            Faod + Faod ^ 2 +
            cloud + cloud ^ 2 + AOT40 + Faod*AOT40 + (Faod^ 2)*AOT40 +
            ", add_terms(c(
              str_c("bin", 1:42)
            )), "|
            x_y[year]") %>%
  as.formula()

fml_7 <- str_c("GOSIF_sum ~
            Caod + Caod ^ 2 +
            Faod + Faod ^ 2 +
            cloud + cloud ^ 2 + AOT40 + Faod*AOT40 + (Faod^ 2)*AOT40 +
            ", add_terms(c(
              str_c("bin", 1:42),
              str_c("root_", 1:9)
            )), "|
            x_y[year]") %>%
  as.formula()


fml_8 <- str_c("GOSIF_sum ~
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
fml_9 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 +
", add_terms(c(
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_10 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 +
", add_terms(c(
    str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_11 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 +
", add_terms(c(
    str_c("root_", 1:9),
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_12 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 +
", add_terms(c(
    str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_13 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 +
", add_terms(c(
    str_c("bin", 1:42),
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_14 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 +
", add_terms(c(
    str_c("bin", 1:42),
    str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_15 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 +
", add_terms(c(
    str_c("bin", 1:42),
    str_c("root_", 1:9),
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_16 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + Faod*AOT40 + (Faod^2)*AOT40 | x_y[year]") %>% as.formula()
fml_17 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_18 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_19 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("root_", 1:9),
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_20 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_21 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("bin", 1:42),
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_22 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("bin", 1:42),
    str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_23 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("bin", 1:42),
    str_c("root_", 1:9),
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_24 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + cloud + cloud^2 | x_y[year]") %>% as.formula()
fml_25 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + cloud + cloud^2 +
", add_terms(c(
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_26 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + cloud + cloud^2 +
", add_terms(c(
    str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_27 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + cloud + cloud^2 +
", add_terms(c(
    str_c("root_", 1:9),
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_28 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + cloud + cloud^2 +
", add_terms(c(
    str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_29 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + cloud + cloud^2 +
", add_terms(c(
    str_c("bin", 1:42),
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_30 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + cloud + cloud^2 +
", add_terms(c(
    str_c("bin", 1:42),
    str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_31 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + cloud + cloud^2 +
", add_terms(c(
    str_c("bin", 1:42),
    str_c("root_", 1:9),
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_32 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 | x_y[year]") %>% as.formula()
fml_33 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_34 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_35 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("root_", 1:9),
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_36 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_37 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("bin", 1:42),
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_38 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("bin", 1:42),
    str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_39 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("bin", 1:42),
    str_c("root_", 1:9),
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_40 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 +
", add_terms(c(
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_41 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 +
", add_terms(c(
    str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_42 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 +
", add_terms(c(
    str_c("root_", 1:9),
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_43 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 +
", add_terms(c(
    str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_44 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 +
", add_terms(c(
    str_c("bin", 1:42),
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_45 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 +
", add_terms(c(
    str_c("bin", 1:42),
    str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_46 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 +
", add_terms(c(
    str_c("bin", 1:42),
    str_c("root_", 1:9),
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_47 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 + Faod*AOT40 + (Faod^2)*AOT40 | x_y[year]") %>% as.formula()
fml_48 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_49 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_50 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("root_", 1:9),
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_51 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_52 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("bin", 1:42),
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_53 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("bin", 1:42),
    str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_54 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("bin", 1:42),
    str_c("root_", 1:9),
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_55 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 + cloud + cloud^2 +
", add_terms(c(
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_56 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 + cloud + cloud^2 +
", add_terms(c(
    str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_57 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 + cloud + cloud^2 +
", add_terms(c(
    str_c("root_", 1:9),
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_58 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 + cloud + cloud^2 +
", add_terms(c(
    str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_59 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 + cloud + cloud^2 +
", add_terms(c(
    str_c("bin", 1:42),
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_60 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 + cloud + cloud^2 +
", add_terms(c(
    str_c("bin", 1:42),
    str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_61 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 + cloud + cloud^2 +
", add_terms(c(
    str_c("bin", 1:42),
    str_c("root_", 1:9),
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_62 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_63 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_64 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("root_", 1:9),
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_65 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod + Faod^2 + AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
    str_c("bin", 1:42),
    str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_66 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 | x_y[year]") %>% as.formula()
fml_67 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 +
", add_terms(c(
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_68 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 +
", add_terms(c(
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_69 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 +
", add_terms(c(
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_70 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 +
", add_terms(c(
  str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_71 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_72 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_73 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_74 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 + Faod*AOT40 + (Faod^2)*AOT40 | x_y[year]") %>% as.formula()
fml_75 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_76 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_77 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_78 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_79 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_80 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_81 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_82 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 + cloud + cloud^2 | x_y[year]") %>% as.formula()
fml_83 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_84 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_85 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_86 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_87 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_88 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_89 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_90 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 | x_y[year]") %>% as.formula()
fml_91 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_92 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_93 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_94 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_95 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_96 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_97 <- str_c("GOSIF_sum ~
Caod + Caod^2 + AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_98 <- str_c("GOSIF_sum ~
Caod + Caod^2 + cloud + cloud^2 | x_y[year]") %>% as.formula()
fml_99 <- str_c("GOSIF_sum ~
Caod + Caod^2 + cloud + cloud^2 +
", add_terms(c(
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_100 <- str_c("GOSIF_sum ~
Caod + Caod^2 + cloud + cloud^2 +
", add_terms(c(
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_101 <- str_c("GOSIF_sum ~
Caod + Caod^2 + cloud + cloud^2 +
", add_terms(c(
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_102 <- str_c("GOSIF_sum ~
Caod + Caod^2 + cloud + cloud^2 +
", add_terms(c(
  str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_103 <- str_c("GOSIF_sum ~
Caod + Caod^2 + cloud + cloud^2 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_104 <- str_c("GOSIF_sum ~
Caod + Caod^2 + cloud + cloud^2 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_105 <- str_c("GOSIF_sum ~
Caod + Caod^2 + cloud + cloud^2 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_106 <- str_c("GOSIF_sum ~
Caod + Caod^2 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 | x_y[year]") %>% as.formula()
fml_107 <- str_c("GOSIF_sum ~
Caod + Caod^2 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_108 <- str_c("GOSIF_sum ~
Caod + Caod^2 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_109 <- str_c("GOSIF_sum ~
Caod + Caod^2 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_110 <- str_c("GOSIF_sum ~
Caod + Caod^2 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_111 <- str_c("GOSIF_sum ~
Caod + Caod^2 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_112 <- str_c("GOSIF_sum ~
Caod + Caod^2 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_113 <- str_c("GOSIF_sum ~
Caod + Caod^2 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_114 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod*AOT40 + (Faod^2)*AOT40 | x_y[year]") %>% as.formula()
fml_115 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_116 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_117 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_118 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_119 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_120 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_121 <- str_c("GOSIF_sum ~
Caod + Caod^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_122 <- str_c("GOSIF_sum ~
Caod + Caod^2 +
", add_terms(c(
  str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_123 <- str_c("GOSIF_sum ~
Caod + Caod^2 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_124 <- str_c("GOSIF_sum ~
Caod + Caod^2 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_125 <- str_c("GOSIF_sum ~
Caod + Caod^2 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_126 <- str_c("GOSIF_sum ~
Caod + Caod^2 +
", add_terms(c(
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_127 <- str_c("GOSIF_sum ~
Caod + Caod^2 +
", add_terms(c(
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_128 <- str_c("GOSIF_sum ~
Caod + Caod^2 +
", add_terms(c(
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_129 <- str_c("GOSIF_sum ~
            Faod + Faod ^ 2|x_y[year]") %>%
  as.formula()
fml_130 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 | x_y[year]") %>% as.formula()
fml_131 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 +
", add_terms(c(
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_132 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 +
", add_terms(c(
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_133 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 +
", add_terms(c(
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_134 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 +
", add_terms(c(
  str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_135 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_136 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_137 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_138 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 + Faod*AOT40 + (Faod^2)*AOT40 | x_y[year]") %>% as.formula()
fml_139 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_140 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_141 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_142 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_143 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_144 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_145 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_146 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 + cloud + cloud^2 | x_y[year]") %>% as.formula()
fml_147 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_148 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_149 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_150 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_151 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_152 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_153 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_154 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 | x_y[year]") %>% as.formula()
fml_155 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_156 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_157 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_158 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_159 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_160 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_161 <- str_c("GOSIF_sum ~
Faod + Faod^2 + AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_162 <- str_c("GOSIF_sum ~
Faod + Faod^2 + cloud + cloud^2 | x_y[year]") %>% as.formula()
fml_163 <- str_c("GOSIF_sum ~
Faod + Faod^2 + cloud + cloud^2 +
", add_terms(c(
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_164 <- str_c("GOSIF_sum ~
Faod + Faod^2 + cloud + cloud^2 +
", add_terms(c(
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_165 <- str_c("GOSIF_sum ~
Faod + Faod^2 + cloud + cloud^2 +
", add_terms(c(
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_166 <- str_c("GOSIF_sum ~
Faod + Faod^2 + cloud + cloud^2 +
", add_terms(c(
  str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_167 <- str_c("GOSIF_sum ~
Faod + Faod^2 + cloud + cloud^2 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_168 <- str_c("GOSIF_sum ~
Faod + Faod^2 + cloud + cloud^2 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_169 <- str_c("GOSIF_sum ~
Faod + Faod^2 + cloud + cloud^2 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_170 <- str_c("GOSIF_sum ~
Faod + Faod^2 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 | x_y[year]") %>% as.formula()
fml_171 <- str_c("GOSIF_sum ~
Faod + Faod^2 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_172 <- str_c("GOSIF_sum ~
Faod + Faod^2 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_173 <- str_c("GOSIF_sum ~
Faod + Faod^2 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_174 <- str_c("GOSIF_sum ~
Faod + Faod^2 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_175 <- str_c("GOSIF_sum ~
Faod + Faod^2 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_176 <- str_c("GOSIF_sum ~
Faod + Faod^2 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_177 <- str_c("GOSIF_sum ~
Faod + Faod^2 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_178 <- str_c("GOSIF_sum ~
Faod + Faod^2 + Faod*AOT40 + (Faod^2)*AOT40 | x_y[year]") %>% as.formula()
fml_179 <- str_c("GOSIF_sum ~
Faod + Faod^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_180 <- str_c("GOSIF_sum ~
Faod + Faod^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_181 <- str_c("GOSIF_sum ~
Faod + Faod^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_182 <- str_c("GOSIF_sum ~
Faod + Faod^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_183 <- str_c("GOSIF_sum ~
Faod + Faod^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_184 <- str_c("GOSIF_sum ~
Faod + Faod^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_185 <- str_c("GOSIF_sum ~
Faod + Faod^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_186 <- str_c("GOSIF_sum ~
Faod + Faod^2 +
", add_terms(c(
  str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_187 <- str_c("GOSIF_sum ~
Faod + Faod^2 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_188 <- str_c("GOSIF_sum ~
Faod + Faod^2 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_189 <- str_c("GOSIF_sum ~
Faod + Faod^2 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()

fml_190 <- str_c("GOSIF_sum ~
Faod + Faod^2 +
", add_terms(c(
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_191 <- str_c("GOSIF_sum ~
Faod + Faod^2 +
", add_terms(c(
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_192 <- str_c("GOSIF_sum ~
Faod + Faod^2 +
", add_terms(c(
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_193 <- str_c("GOSIF_sum ~
            AOT40|x_y[year]") %>%
  as.formula()
fml_194 <- str_c("GOSIF_sum ~
AOT40 + cloud + cloud^2 | x_y[year]") %>% as.formula()
fml_195 <- str_c("GOSIF_sum ~
AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_196 <- str_c("GOSIF_sum ~
AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_197 <- str_c("GOSIF_sum ~
AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_198 <- str_c("GOSIF_sum ~
AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_199 <- str_c("GOSIF_sum ~
AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_200 <- str_c("GOSIF_sum ~
AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_201 <- str_c("GOSIF_sum ~
AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_202 <- str_c("GOSIF_sum ~
AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 | x_y[year]") %>% as.formula()
fml_203 <- str_c("GOSIF_sum ~
AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_204 <- str_c("GOSIF_sum ~
AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_205 <- str_c("GOSIF_sum ~
AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_206 <- str_c("GOSIF_sum ~
AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_207 <- str_c("GOSIF_sum ~
AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_208 <- str_c("GOSIF_sum ~
AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_209 <- str_c("GOSIF_sum ~
AOT40 + cloud + cloud^2 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_210 <- str_c("GOSIF_sum ~
AOT40 + Faod*AOT40 + (Faod^2)*AOT40 | x_y[year]") %>% as.formula()
fml_211 <- str_c("GOSIF_sum ~
AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_212 <- str_c("GOSIF_sum ~
AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_213 <- str_c("GOSIF_sum ~
AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_214 <- str_c("GOSIF_sum ~
AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_215 <- str_c("GOSIF_sum ~
AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_216 <- str_c("GOSIF_sum ~
AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_217 <- str_c("GOSIF_sum ~
AOT40 + Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_218 <- str_c("GOSIF_sum ~
AOT40 +
", add_terms(c(
  str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_219 <- str_c("GOSIF_sum ~
AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_220 <- str_c("GOSIF_sum ~
AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_221 <- str_c("GOSIF_sum ~
AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_222 <- str_c("GOSIF_sum ~
AOT40 +
", add_terms(c(
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_223 <- str_c("GOSIF_sum ~
AOT40 +
", add_terms(c(
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_224 <- str_c("GOSIF_sum ~
AOT40 +
", add_terms(c(
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_225 <- str_c("GOSIF_sum ~
            Faod*AOT40 + (Faod^2)*AOT40|x_y[year]") %>%
  as.formula()
fml_226 <- str_c("GOSIF_sum ~
Faod*AOT40 + (Faod^2)*AOT40 + cloud + cloud^2 | x_y[year]") %>% as.formula()
fml_227 <- str_c("GOSIF_sum ~
Faod*AOT40 + (Faod^2)*AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_228 <- str_c("GOSIF_sum ~
Faod*AOT40 + (Faod^2)*AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_229 <- str_c("GOSIF_sum ~
Faod*AOT40 + (Faod^2)*AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_230 <- str_c("GOSIF_sum ~
Faod*AOT40 + (Faod^2)*AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_231 <- str_c("GOSIF_sum ~
Faod*AOT40 + (Faod^2)*AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_232 <- str_c("GOSIF_sum ~
Faod*AOT40 + (Faod^2)*AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_233 <- str_c("GOSIF_sum ~
Faod*AOT40 + (Faod^2)*AOT40 + cloud + cloud^2 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_234 <- str_c("GOSIF_sum ~
Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42)
)), "|
x_y[year]") %>% as.formula()
fml_235 <- str_c("GOSIF_sum ~
Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_236 <- str_c("GOSIF_sum ~
Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_237 <- str_c("GOSIF_sum ~
Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("bin", 1:42),
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_238 <- str_c("GOSIF_sum ~
Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("root_", 1:9)
)), "|
x_y[year]") %>% as.formula()
fml_239 <- str_c("GOSIF_sum ~
Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("root_", 1:9),
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()
fml_240 <- str_c("GOSIF_sum ~
Faod*AOT40 + (Faod^2)*AOT40 +
", add_terms(c(
  str_c("d", 1:13)
)), "|
x_y[year]") %>% as.formula()