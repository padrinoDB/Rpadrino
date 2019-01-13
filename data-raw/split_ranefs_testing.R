
library(RPadrino)
padrino <- RPadrino:::.read_all_sheets('data-raw/all_progress.xlsx')

i <- 1
db <- padrino_filter(padrino, ipm_id == 'aaaa38')

current_id <- unique(db[[1]]$ipm_id)[i]

current_model <- padrino_filter(db, .data$ipm_id == current_id)

# levels <- RPadrino:::.make_ranef_levels(current_model[[11]])


# test <- RPadrino:::.split_ranefs(current_model)


test_proto <- make_proto_ipm(current_model)
test_ipm <- make_ipm(test_proto)

# Not yet generalized...
test_K <- make_k(test_ipm, test_proto)
