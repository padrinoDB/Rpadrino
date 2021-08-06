data(pdb)

test_that("pdb_report generates correct output", {

  out_temp <- tempdir()
  dir.create(out_temp, FALSE)


  html_fp <- pdb_report(pdb,
                        rmd_dest = paste0(out_temp, "/test_html.rmd"),
                        output_format = "html",
                        map = TRUE)

  pdf_fp <- pdb_report(pdb,
                       rmd_dest = paste0(out_temp, "/test_pdf.rmd"),
                       output_format = "pdf",
                       map = TRUE)

  test_pats <- c(paste0(out_temp, "/",
                     paste0("test_",
                           c("html", "pdf"), ".rmd")),
                 paste0(out_temp, "/",
                        paste0("test_", c("html.html", "pdf.pdf"))))

  actual_pats <- list.files(out_temp, full.names = TRUE)

  expect_true(all(test_pats %in% actual_pats))

  unlink(out_temp, recursive = TRUE, force = TRUE)

})
