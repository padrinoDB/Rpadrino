data(pdb)

test_that("pdb_report generates correct output", {

  skip_if_not(rmarkdown::pandoc_available(version = "1.12.3"))

  out_temp <- tempdir()
  dir.create(out_temp, FALSE)


  html_fp <- pdb_report(pdb,
                        rmd_dest = out_temp,
                        output_format = "html",
                        map = TRUE)

  pdf_fp <- pdb_report(pdb,
                       rmd_dest = out_temp,
                       output_format = "pdf",
                       map = TRUE)

  date <- gsub("-", "", Sys.Date())

  test_pats <- paste0(out_temp, "/",
                      paste0("Rpadrino_report_", date,
                             c(".html", ".pdf", ".Rmd")))

  actual_pats <- list.files(out_temp, full.names = TRUE)

  expect_true(all(test_pats %in% actual_pats))

  unlink(out_temp, recursive = TRUE, force = TRUE)

})

test_that("full filenames work too", {

  skip_if_not(rmarkdown::pandoc_available(version = "1.12.3"))

  out_temp <- tempdir()
  dir.create(out_temp, FALSE)

  test_pat <- paste0(out_temp, "/test_report.Rmd")

  html_fp <- pdb_report(pdb,
                        rmd_dest = test_pat,
                        output_format = "html",
                        map = TRUE)

  date <- gsub("-", "", Sys.Date())

  test_pats <- paste0(out_temp, "/",
                      paste0("test_report_", date,
                             c(".html", ".Rmd")))

  actual_pats <- list.files(out_temp, full.names = TRUE)

  expect_true(all(test_pats %in% actual_pats))

  unlink(out_temp, recursive = TRUE, force = TRUE)

})
