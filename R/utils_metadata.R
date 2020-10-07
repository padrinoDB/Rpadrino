#' @rdname metadata_utils
#'
#' @title Access pieces of metadata from a \code{pdb} object
#'
#' @description These functions access pieces of specific pieces metadata from
#' the \code{Metadata} table of a \code{pdb} object.
#'
#' @param pdb A Padrino Database object.
#' @param ipm_id The ID of the model. The default (\code{NULL}) returns all
#' values in the \code{pdb} object.
#'
#' @return A vector
#'
#' @export

pdb_citation <- .make_pdb_accessor("apa_citation")

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

