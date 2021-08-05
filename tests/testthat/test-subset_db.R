
data(pdb)

test_that("pdb_subset works", {

  sub_ind <- paste("aaaa",15:20 , sep = "")

  sub_db <- pdb_subset(pdb, sub_ind)

  expect_s3_class(sub_db, "pdb")

  expect_equal(dim(sub_db$Metadata)[1], 6L)


  sub_ind <- "xyz"

  empty_db <- pdb_subset(pdb, sub_ind)

  expect_equal(dim(empty_db$Metadata)[1], 0L)

})
