#' @rdname metadata_utils
#'
#' @title Access pieces of metadata from a \code{pdb} object
#'
#' @description These functions access pieces of specific pieces metadata from
#' the \code{Metadata} table of a \code{pdb} object. The exception is
#' \code{pdb_report}, which automatically generates a report with summary
#' statistics and citation information for the \code{pdb} object.
#'
#' @param pdb A Padrino Database object.
#' @param ipm_id The ID of the model. The default (\code{NULL}) returns all
#' values in the \code{pdb} object.
#'
#' @return A vector of the metadata. For \code{pdb_report}e file path to the
#' rendered output, or to the \code{.rmd} file when \code{render_output = FALSE}.
#'
#' @export

pdb_citations <- .make_pdb_accessor("apa_citation")

#' @export
#' @rdname metadata_utils

pdb_species_accepted <- .make_pdb_accessor("species_accepted")

#' @export
#' @rdname metadata_utils

pdb_species_author <- .make_pdb_accessor("species_author")

#' @export
#' @rdname metadata_utils

pdb_genus <- .make_pdb_accessor("tax_genus")

#' @export
#' @rdname metadata_utils

pdb_family <- .make_pdb_accessor("tax_family")

#' @export
#' @rdname metadata_utils

pdb_order <- .make_pdb_accessor('tax_order')

#' @export
#' @rdname metadata_utils

pdb_class <- .make_pdb_accessor("tax_class")

#' @export
#' @rdname metadata_utils

pdb_phylum <- .make_pdb_accessor("tax_phylum")

#' @export
#' @rdname metadata_utils

pdb_kingdom <- .make_pdb_accessor("kingdom")

#' @export
#' @rdname metadata_utils

pdb_org_type <- .make_pdb_accessor("organism_type")

#' @export
#' @rdname metadata_utils

pdb_dicot_monocot <- .make_pdb_accessor("dicot_monocot")

#' @export
#' @rdname metadata_utils

pdb_angio_gymon <- .make_pdb_accessor("angio_gymno")

#' @export
#' @rdname metadata_utils

pdb_authors <- .make_pdb_accessor("authors")

#' @export
#' @rdname metadata_utils

pdb_journal <- .make_pdb_accessor("journal")

#' @export
#' @rdname metadata_utils

pdb_pub_year <- .make_pdb_accessor('pub_year')

#' @export
#' @rdname metadata_utils

pdb_doi <- .make_pdb_accessor("doi")

#' @export
#' @rdname metadata_utils

pdb_comments <- .make_pdb_accessor("remark")

#' @export
#' @rdname metadata_utils

pdb_appendix_link <- .make_pdb_accessor('demog_appendix_link')

#' @export
#' @rdname metadata_utils

pdb_duration <- .make_pdb_accessor("duration")

#' @export
#' @rdname metadata_utils

pdb_start_year <- .make_pdb_accessor("start_year")

#' @export
#' @rdname metadata_utils

pdb_start_month <- .make_pdb_accessor("start_month")

#' @export
#' @rdname metadata_utils

pdb_end_year <- .make_pdb_accessor("end_year")

#' @export
#' @rdname metadata_utils

pdb_end_month <- .make_pdb_accessor("end_month")

#' @export
#' @rdname metadata_utils

pdb_periodicity <- .make_pdb_accessor("periodicity")

#' @export
#' @rdname metadata_utils

pdb_population_name <- .make_pdb_accessor("population_name")

#' @export
#' @rdname metadata_utils

pdb_number_populations <- .make_pdb_accessor("number_populations")

#' @export
#' @rdname metadata_utils

pdb_lat <- .make_pdb_accessor("lat")

#' @export
#' @rdname metadata_utils

pdb_lon <- .make_pdb_accessor("lon")

#' @export
#' @rdname metadata_utils

pdb_altitude <- .make_pdb_accessor("altitude")

#' @export
#' @rdname metadata_utils

pdb_country <- .make_pdb_accessor("country")

#' @export
#' @rdname metadata_utils

pdb_continent <- .make_pdb_accessor("continent")

#' @export
#' @rdname metadata_utils

pdb_ecoregion <- .make_pdb_accessor("ecoregion")

#' @export
#' @rdname metadata_utils

pdb_studied_sex <- .make_pdb_accessor("studied_sex")

#' @export
#' @rdname metadata_utils

pdb_eviction_used <- .make_pdb_accessor("eviction_used")

#' @export
#' @rdname metadata_utils

pdb_evict_type <- .make_pdb_accessor("evict_type")

#' @export
#' @rdname metadata_utils

pdb_treatment <- .make_pdb_accessor("treatment")

#' @export
#' @rdname metadata_utils

pdb_has_time_lag <- .make_pdb_accessor("has_time_lag")

#' @export
#' @rdname metadata_utils

pdb_has_age <- .make_pdb_accessor("has_age")


#' @rdname metadata_utils
#'
#' @param title The title for the created report.
#' @param keep_rmd Keep the un-rendered Rmd file? Useful for manual editing.
#' @param rmd_dest The folder to save the Rmd file at if \code{keep_rmd = TRUE}.
#' The default is \code{getwd()}.
#' @param output_format The output format to create. Options are "html", "pdf",
#' "word", "odt", "rtf", or "md".
#' @param render_output A logical - should the document be rendered for inspection?
#' @param map Create a map of studies included in the \code{pdb} object?
#'
#' @importFrom rmarkdown render
#' @importFrom stats complete.cases
#' @export

pdb_report <- function(pdb,
                       title = "",
                       keep_rmd = TRUE,
                       rmd_dest = getwd(),
                       output_format = "html",
                       render_output = TRUE,
                       map = TRUE) {

  rmd_dest <- .pdb_rmd_dest(rmd_dest, keep_rmd)

  output <- .pdb_rmd_header(title, output_format)

  md       <- pdb$Metadata
  ev       <- pdb$EnvironmentalVariables
  par_sets <- pdb$ParSetIndices

  any_pi   <- nrow(par_sets) > 0
  any_disc <- any(pdb$StateVariables$discrete | duplicated(pdb$StateVariables$ipm_id))
  any_env  <- nrow(ev) > 0


  output <- .pdb_rmd_summary_paragraph(md, output)

  output <- .pdb_rmd_spec_info(output, any_pi, any_env, any_disc)

  if(map) {
    output <- .pdb_rmd_map(output, pdb)

    coords <- data.frame(lat = suppressWarnings(as.numeric(pdb$Metadata$lat)),
                         lon = suppressWarnings(as.numeric(pdb$Metadata$lon)))

    coords <- coords[stats::complete.cases(coords), ]

    ev_env <- new.env()
    ev_env$coords <- coords
  }

  output <- .pdb_rmd_citations(output, pdb) %>%
    .pdb_clean_report_source()

  writeLines(output, con = rmd_dest)

  if(render_output) {
    out_path <- rmarkdown::render(rmd_dest, envir = ev_env)
  } else {
    out_path <- rmd_dest
  }

  if(!keep_rmd) unlink(rmd_dest)

  invisible(out_path)

}

#' @noRd
#' @importFrom tools file_ext

.pdb_rmd_dest <- function(rmd_dest, keep_rmd) {

  date <- gsub("-", "", Sys.Date())

  # Non-specified output directory gets a tempdir()
  if((is.null(rmd_dest) || is.na(rmd_dest) || rmd_dest == "") && keep_rmd) {

    rmd_dest <- tempfile(pattern = paste0("Rpadrino_report_", date),
                         fileext = ".Rmd")

    message("'keep_rmd = TRUE' and 'rmd_dest' is not specified! ",
            "Saving to a temporary file: \n", rmd_dest)

  } else if(file_ext(rmd_dest) == "") {

    rmd_dest <- paste0(rmd_dest, "/Rpadrino_report_", date, ".Rmd")

    file.create(rmd_dest, showWarnings = FALSE)

  } else if(tools::file_ext(tolower(rmd_dest)) == "rmd") {

    rmd_dest <- gsub("\\.rmd$", paste0("_", date, ".Rmd"),
                     rmd_dest,
                     ignore.case = TRUE)

    file.create(rmd_dest, showWarnings = FALSE)

  } else {

    stop("'rmd_dest' must either be 'NULL', the name of a folder, or a file with",
         " a .rmd file extension!")
  }

  rmd_dest
}

#' @noRd

.pdb_clean_report_source <- function(report) {

  chunk_ind <- grepl("```", report)

  report[chunk_ind] <- trimws(report[chunk_ind])

  report

}

#' @importFrom ggplot2 ggplot geom_polygon geom_point theme_bw scale_y_continuous
#' @importFrom ggplot2 map_data aes xlab ylab scale_x_continuous
#' @noRd

.pdb_rmd_map <- function(output, use_pdb) {

  mp_expr <- rlang::expr(
    ggplot2::ggplot(data = wrld_map,
                    ggplot2::aes(x = long,
                                 y = lat,
                                 group = group)) +
      ggplot2::geom_polygon(fill = NA, color = 'grey70') +
      ggplot2::geom_point(data = coords,
                 ggplot2::aes(x = lon, y = lat,),
                 inherit.aes = FALSE,
                 color = 'black',
                 # shape = 1,
                 size = 3) +
      ggplot2::theme_bw() +
      ggplot2::xlab('Longitude') +
      ggplot2::ylab('Latitude') +
      ggplot2::scale_x_continuous(
        breaks = seq(-180, 180, by = 60),
        labels = NULL
      ) +
    ggplot2::scale_y_continuous(
      breaks = seq(-90, 90, by = 60),
      labels = NULL
    )
  )

  mp_txt <- c("```{r echo = FALSE, message = FALSE, fig.height = 5, fig.width = 8}\n\n",
              'coords <- coords[complete.cases(coords), ]\n\n
               wrld_map <- ggplot2::map_data(map = "world")\n\n',
              rlang::expr_text(mp_expr),
              "\n\n```")

  header <- "\n\n# Map of studies in this `pdb` object\n\n"

  c(output, header, mp_txt)


}

#' @noRd

.pdb_rmd_citations <- function(output, pdb) {

  cit_list <- unique(pdb_citations(pdb))

  cit_list <- .pdb_rmd_append_doi_jstor(pdb, cit_list)
  cit_list <- .pdb_rmd_append_appendix_link(pdb, cit_list)

  cit_list <- paste(seq_along(cit_list), ". ", cit_list, sep = "")
  cit_list <- paste(cit_list, collapse = "\n\n")

  c(output, "\n\n# Citations included in the `pdb` object\n\n", cit_list)

}

#' @noRd
# Assumes anything without a full URL will point to a DOI. Need to check how
# this works with an actual database.

.pdb_rmd_append_appendix_link <- function(pdb, cit_list) {

  app_links <- pdb$Metadata$demog_appendix_link[!duplicated(pdb$Metadata$apa_citation)]

  for(i in seq_along(cit_list)) {

    if(is.na(app_links[i])) next

    if(!.pdb_is_http(app_links[i])) {

      if(.pdb_is_www(app_links[i])){

        app_links[i] <- paste0("https://", app_links[i])

      } else {

        app_links[i] <- paste0("https://doi.org/", app_links[i])

      }
    }

    cit_list[i] <- paste0(cit_list[i], ", [Appendix Link](", app_links[i], ")")
  }

  return(cit_list)

}

#' @noRd
# RMarkdown requires URLs to start with https://, otheriwse it will point to
# a local destination. Thus, anything starting w/ www needs to have http or
# https prepended to it.

.pdb_is_www <- function(x) {

  tst <- substr(x, 1, 3)

  if(tst == "www") return(TRUE)

  return(FALSE)
}

#' @noRd

.pdb_is_http <- function(x) {

  tst <- substr(x, 1, 5)

  if(tst == "https") return(TRUE)

  return(FALSE)

}

#' @noRd

.pdb_rmd_append_doi_jstor <- function(pdb, cit_list) {

  doi_jstor <- pdb$Metadata$doi[!duplicated(pdb$Metadata$apa_citation)]

  for(i in seq_along(cit_list)) {

    if(is.na(doi_jstor[i])) next

    doi_jstor_link <- doi_jstor[i]

    if(grepl("jstor\\.org", doi_jstor_link)) {

      cit_list[i] <- paste0(cit_list[i], ", [JSTOR Link](", doi_jstor_link,")")

    } else {

      temp <- paste0("https://doi.org/", doi_jstor_link)

      cit_list[i] <- paste0(cit_list[i], " [DOI](", temp,")")
    }

  }

  return(cit_list)

}

#' @noRd

.pdb_rmd_spec_info <- function(output, any_pi, any_env, any_disc) {

  c(
    output,
    "\n\n# Information about your version\n\n",
    paste0("**Contains models with parameter sets**: ", any_pi, "\n\n"),
    paste0("**Contains models with continuous environmental variation**: ",
           any_env,
           "\n\n"),
    paste0("**Contains general IPMs**: ", any_disc, "\n\n"),
    .pdb_explain_par_sets(),
    .pdb_explain_env_vars(),
    .pdb_explain_general())

}

#' @noRd

.pdb_explain_general <- function() {

  header <- "**Explanation of general IPMs**: "

  msg <- c(
    "General IPMs are IPMs that contain multiple continuous and/or discrete states.",
    " These usually don't take much longer to build, but there are some ",
    "subsequent analyses which may require additional attention. This is why we",
    " highlight these here.\n\n",
    "For a more complete introduction to general IPMs, see [Ellner & Rees (2006)",
    " Integral Projection Models for Species with Complex Demography. _Am Nat_",
    " 167(3): 410-428.](https://doi.org/10.1086/499438), or Ellner, Childs & Rees",
    " (2016) Data Driven Modelling of Structured Populations, Chapter 6.\n\n"

  )

  c(header, msg)
}

#' @noRd

.pdb_explain_env_vars <- function() {

  header <- "**Explanation of continuous environmental variation**: "

  msg <- c(
    "Continuous environmental variation is handled in PADRINO by sampling from",
    " random number generators corresponding to the appropriate distribution",
    " as reported by the authors. Because these models all include calls to some",
    " stochastic algorithm, they are always treated as stochastic models at build",
    " time, regardlessof whether the authors intended for them to be. To circumvent ",
    "this behavior, you can set the values of the continuouslly varying parameters",
    " manually and run a deterministic projection using either _Rpadrino_ or _ipmr_.\n\n",
    "It is also worth noting that stochastic models with continuously varying",
    " environments can take some time to run, due to extra steps of sampling the",
    " environment and then reconstructing unique kernels for each iteration. Please",
    " be patient with them!\n\n"
  )

  c(header, msg)
}

#' @noRd

.pdb_explain_par_sets <- function() {

  header <- "**Explanation of Parameter Sets**: "

  msg <- c(
    "This information is reported because models with many parameter sets may ",
    "take longer to rebuild than one might otherwise expect from looking at the",
    " model code. ",
    "Parameter sets refer to situations where a single parameter (e.g. an ",
    "intercept or slope from a regression model) may take on many values. This",
    " is often the case with vital rate regressions fit with discrete predictors,",
    " or with mixed effects models. Common examples include year or site specific",
    " effects. PADRINO, along with 'ipmr', implements a syntax that allows us to ",
    "concisely represent these models without risking typographical errors or",
    " retyping an expression many times. \n\n",
    "For example, the expression `mu_g_yr = alpha_g_yr + beta_g * z_1` may encompass",
    " many years, suffixed with `'_yr'`. In PADRINO, we store the actual values",
    " `'_yr'` can take on in a table and automatically perform the substitutions.",
    " Depending on the number of parameter set indices in the model, this may ",
    "come at virtually no time cost, or it may substantially increase computation",
    " times.\n\n")

  c(header, msg)
}

#' @noRd

.pdb_rmd_summary_paragraph <- function(md_tab, output) {

  c(output,
    "\n\n# Summary\n\n",
    "This a PADRINO database object with ",
    length(unique(md_tab$species_accepted)), " species from ",
    length(unique(md_tab$apa_citation)), " publications. Please cite all",
    " publications in either your main text or supplementary materials!",
    "The citations are included in the 'Citations' section below.")

}

#' @noRd

.pdb_rmd_header <- function(title, output_format) {

  paste("---",
        paste0("title: '", title, "'"),
        paste0("output: ", output_format, "_document"),
        paste0("date: '`r Sys.Date()`'"),
        paste0("urlcolor: blue"),

        # Other options need to be added here!

        '---\n',
        sep = "\n")
}

