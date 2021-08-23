# Sandbox testing

data(pdb)

id <- "aaaa55"

proto <- pdb_make_proto_ipm(pdb,
                            ipm_id = id,
                            det_stoch = "det",
                            kern_param = NA)

test_that("proto_ipm is named and classed right", {

  expect_true("aaaa55" %in% names(proto))
  expect_s3_class(proto[[1]], "general_di_det")

})

x <- pdb_make_ipm(proto)

test_that("ipmr classes set for outputs from pdb_make_proto", {

  expect_s3_class(x[[1]], "general_di_det_ipm")

})

# For the sake of this rebuild, will switch my model's eviction function
# to truncated_distributions

# pdb$Metadata$evict_type[pdb$Metadata$ipm_id == 'aaa341'] <- 'truncated_distributions'

id <- c("aaa310", "aaaa55")

proto_2 <- pdb_make_proto_ipm(pdb, ipm_id = id, det_stoch = c("det", "det"))

x_2 <- pdb_make_ipm(proto_2)

test_that("building multiple protos works", {

  cls <- vapply(x_2, class, character(3L))

  expect_equal(as.vector(cls[1, ]),
               c("general_di_det_ipm", "simple_di_det_ipm"))

  expect_equal(names(x_2), c("aaaa55", "aaa310"))

})

