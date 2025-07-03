source("script/00_loadPackages.R")
source("script/loadFunctions.R")

# ozone -------------------------------------------------------------------

ozone <- read_rds("E:/data/outputs/ozone/tidied.rds") # unit is ug/m3

yearly <- ozone %>%
  lazy_dt() %>%
  group_by(x, y, year) %>%
  summarise(peak = fmean(O3[month %in% 4:9]), annual = fmean(O3), .groups = "drop") %>%
  as_tibble()

model <- feols(annual ~ peak | x^y, yearly, cluster = ~ x^y, nthreads = 0, combine.quick = F)

month_climatology <- ozone %>%
  fgroup_by(x, y, month) %>%
  fsummarise(O3_month = fmean(O3)) %>%
  fgroup_by(x, y) %>%
  fmutate(O3_month_percent = O3_month / fmean(O3_month), .keep = "unused") %>%
  fungroup()

veg_model <- read_rds("E:/data/outputs/ozone/gam.rds")

require(parallel)
cl <- makeCluster(qn)
ozone_cft_all <-
  tibble(
    peak_level = seq(30, 150, 5),
    ozone_data = map(peak_level, function(anum) {
      month_climatology %>%
        distinct(x, y) %>%
        mutate(peak = anum) %>%
        mutate(
          O3 = predict(model, .),
          O3 = fifelse(O3 < 0, 0, O3)
        ) %>%
        select(-peak) %>%
        inner_join(month_climatology, by = join_by(x, y)) %>%
        mutate(O3 = O3_month_percent * O3, .keep = "unused") %>%
        fmutate(
          W126_cft = exp(predict(veg_model$model_w126, ., cluster = cl)),
          AOT40_cft = exp(predict(veg_model$model_aot40, ., cluster = cl))
        ) %>%
        select(-O3)
    }, .progress = T)
  )

saveRDS(ozone_cft_all, "E:/data/ozone_cft.rds")
