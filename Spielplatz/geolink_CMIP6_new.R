start_date <- as.Date(start_date)
end_date <- as.Date(end_date)

s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

it_obj <- s_obj %>%
  stac_search(
    collections = "nasa-nex-gddp-cmip6",
    datetime = "1950/1960",
    query = list("cmip6:model" = list(eq = "ACCESS-CM2"))
  ) %>%
  get_request() %>%
  items_sign(sign_fn = sign_planetary_computer())

url_list <- lapply(it_obj$features, function(feature) {
  paste0("/vsicurl/", feature$assets$data$href)
})
