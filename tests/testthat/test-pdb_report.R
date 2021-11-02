data(pdb)

test_that("pdb_report generates correct output", {

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
                      paste0("RPadrino_report_", date,
                             c(".html", ".pdf", ".Rmd")))

  actual_pats <- list.files(out_temp, full.names = TRUE)

  expect_true(all(test_pats %in% actual_pats))

  unlink(out_temp, recursive = TRUE, force = TRUE)

})
