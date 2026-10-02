#' Get a data frame of PDB chains.
#'
#' @description
#' Code for Painting the PDBs with LogFC data.
#'
#' @param pdb_code A string representing the PDB code (e.g., "7cid").
#'
#' @return A grouped `data.frame` with PDB chain information.
#' @importFrom httr GET
#' @importFrom dplyr bind_rows mutate group_by
#' @importFrom RcppSimdJson fload fparse
#' @export
fct_get_chain_df <- function(pdb_code) {
  # Make the API call to get Uniprot mappings
  r <- httr::GET(paste0("https://www.ebi.ac.uk/pdbe/api/mappings/uniprot/", pdb_code))

  # Check for successful API response
  if (r$status_code != 200) {
    stop("Failed to fetch data from the API. Check your input code or internet connection.")
  }

  # Convert response content to text and parse as JSON
  r_content <- RcppSimdJson::fparse(r$content)

  # Process the data to create a data frame with chain information
  chains <- do.call(dplyr::bind_rows,
                    lapply(names(r_content[[1]][[1]]), function(uniprot) {
                      r_content[[1]][[1]][[uniprot]] %>%
                        data.frame() %>%
                        dplyr::mutate(uniprot = uniprot)
                    })
  ) %>%
    dplyr::group_by(identifier, mappings.entity_id, uniprot) %>%
    dplyr::select(uniprot, mappings.chain_id, identifier, mappings.entity_id)

  return(chains)
}


#' Read a logFC table uploaded by the user.
#'
#' @description
#' Yeast fold-change tables come out of whatever pipeline produced them, so the
#' identifier and fold-change columns are found by name rather than by
#' position. Standard gene names (`COX4`), systematic/ORF names (`YGL187C`) and
#' UniProt accessions are all accepted; whichever is present is normalised to
#' the `genename` and `logfc` columns the painter works in.
#'
#' @param file_path Path to the file on disk. For a Shiny upload this is the
#'   temporary path, which carries no extension.
#' @param file_name Original file name, used to determine the format.
#'
#' @return A `data.frame` with a `genename` and `logfc` column, and a `uniprot`
#'   column where the file supplied accessions directly.
#' @importFrom tools file_ext
#' @export
fct_read_logfc_file <- function(file_path, file_name = file_path) {
  ext <- tolower(tools::file_ext(file_name))

  df <- switch(
    ext,
    "csv" = utils::read.csv(file_path, check.names = FALSE,
                            na.strings = c("NA", "NaN", ""),
                            stringsAsFactors = FALSE),
    "tsv" = ,
    "tab" = ,
    "txt" = as.data.frame(readr::read_tsv(
      file_path, show_col_types = FALSE, na = c("", "NA", "NaN"),
      name_repair = "minimal"
    )),
    "xlsx" = ,
    "xlsm" = ,
    "xls"  = {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("Reading Excel files needs the openxlsx package.")
      }
      as.data.frame(openxlsx::read.xlsx(file_path, sheet = 1))
    },
    # An extensionless path is the common case for a Shiny upload, so fall back
    # to CSV rather than refusing the file.
    utils::read.csv(file_path, check.names = FALSE,
                    na.strings = c("NA", "NaN", ""), stringsAsFactors = FALSE)
  )

  if (is.null(df) || nrow(df) == 0) {
    stop("The uploaded file has no rows.")
  }

  gene_col <- match_logfc_column(names(df), "gene")
  fc_col   <- match_logfc_column(names(df), "logfc")
  acc_col  <- match_logfc_column(names(df), "uniprot")

  if (is.null(fc_col)) {
    stop("No fold-change column found. Name one of the columns ",
         "'logFC', 'log2FC' or 'logfc'.")
  }
  if (is.null(gene_col) && is.null(acc_col)) {
    stop("No identifier column found. Name one of the columns 'gene', ",
         "'ORF' or 'uniprot'.")
  }

  out <- data.frame(
    genename = if (is.null(gene_col)) NA_character_ else trimws(as.character(df[[gene_col]])),
    logfc    = suppressWarnings(as.numeric(df[[fc_col]])),
    stringsAsFactors = FALSE
  )
  if (!is.null(acc_col)) {
    out$uniprot <- trimws(as.character(df[[acc_col]]))
  }

  out <- out[is.finite(out$logfc), , drop = FALSE]
  if (nrow(out) == 0) {
    stop("The fold-change column '", fc_col, "' held no numeric values.")
  }
  out
}

#' Find the column holding a given kind of value.
#'
#' @description
#' Patterns are tried in order and the first column matching one of them wins,
#' so an exact `gene` beats an incidental `gene_description`. Matching is
#' case-insensitive because column headings are not written consistently.
#'
#' @param actual_cols Column names present in the file.
#' @param target_type One of `"gene"`, `"logfc"` or `"uniprot"`.
#' @return The matching column name, or `NULL`.
#' @export
match_logfc_column <- function(actual_cols, target_type) {
  column_dictionary <- list(
    gene = c("^genename$", "^gene$", "^gene[._ ]?name$", "^symbol$",
             "^orf$", "^systematic[._ ]?name$", "^gene[._ ]?id$",
             "^PG\\.Genes$", "gene"),
    logfc = c("^logfc$", "^log2fc$", "^log2[._ ]?fold[._ ]?change$",
              "^log2FC\\(median\\)$", "^fold[._ ]?change$", "log2fc", "logfc"),
    uniprot = c("^uniprot$", "^uniprot[._ ]?id[s]?$", "^accession$",
                "^PG\\.UniProtIds$", "uniprot")
  )

  for (p in column_dictionary[[target_type]]) {
    hit <- grep(p, actual_cols, ignore.case = TRUE, value = TRUE)
    if (length(hit) > 0) return(hit[1])
  }
  NULL
}


#' Generate hex color codes for logFC values based on a given range.
#'
#' @description
#' This function takes a data frame with a 'logfc' column and
#' generates a new column with hex color codes based on a specified range.
#'
#' @param df The data frame containing the 'logfc' values.
#' @param range A numeric vector of length 2 defining the color gradient scale.
#' @return The input data frame with a new column, 'hex_from_scales'.
#' @importFrom scales col_numeric
#' @export
fct_generate_hex_colors <- function(df, range) {
  # Clamp values to the user-selected range
  clamped_values <- pmin(pmax(df$logfc, range[1]), range[2])
  # Define the color palette
  palette_func <- scales::col_numeric(c("blue", "white", "red"), domain = c(range[1], range[2]))
  # Map clamped values to colors
  df$hex_from_scales <- palette_func(clamped_values)
  return(df)
}


#' Normalise logFC values for B-factor coloring
#'
#' @description
#' This function clamps logFC values into a user-selected range
#' and rescales them (if desired) to keep values consistent across
#' structures.
#'
#' @param df A data frame containing a `logfc` column.
#' @param range A numeric vector of length 2 giving the min/max limits.
#' @param rescale Logical, whether to rescale values into [0,1]. Default = FALSE.
#'
#' @return The input data frame with an added column `bfactor_value`.
#' @export
fct_range_logfc <- function(df, range = c(-2, 2), rescale = FALSE) {
  stopifnot("logfc" %in% names(df))

  clamped <- pmin(pmax(df$logfc, range[1]), range[2])

  if (rescale) {
    normalised <- (clamped - range[1]) / (range[2] - range[1])
  } else {
    normalised <- clamped
  }

  df$bfactor_value <- normalised
  return(df)
}


# Session cache for the yeast reference set, populated on first use.
.sgd_cache <- new.env(parent = emptyenv())

#' Load the bundled S. cerevisiae reference table.
#'
#' @description
#' The reviewed *Saccharomyces cerevisiae* S288C proteome from UniProt, shipped
#' as a TSV in `inst/app/www`. One row per accession, carrying the standard
#' gene name, the systematic/ORF name and any synonyms.
#'
#' Read on first use and cached for the rest of the session. This replaces the
#' HGNC set the human app uses: the yeast proteome is around 6,700 proteins, so
#' the whole table is a few hundred kilobytes rather than tens of megabytes.
#'
#' @return A `data.frame` of `accession`, `gene`, `orf`, `synonyms`, `protein`.
#' @export
yeast_data <- function() {
  if (is.null(.sgd_cache$data)) {
    .sgd_cache$data <- utils::read.delim(
      app_sys("app/www/sgd_uniprot.tsv"),
      sep = "\t", quote = "", check.names = FALSE,
      stringsAsFactors = FALSE, colClasses = "character",
      na.strings = character(0)
    )
  }
  .sgd_cache$data
}

#' Map any yeast identifier to a UniProt accession.
#'
#' @description
#' Yeast proteins are referred to by three different names depending on where
#' the data came from: the standard gene name (`COX4`), the systematic/ORF name
#' (`YGL187C`), or the UniProt accession. All three resolve here, along with
#' historical synonyms, so a fold-change file does not have to be relabelled
#' before it can be painted.
#'
#' Matching is case-insensitive and whitespace is trimmed. Where a name is
#' ambiguous the standard gene name wins over an ORF, and an ORF over a
#' synonym.
#'
#' @param ids Character vector of gene names, ORF names or accessions.
#' @return A character vector of accessions, `NA` where unknown.
#' @export
yeastToUniprot <- function(ids) {
  if (is.null(.sgd_cache$alias_lookup)) {
    ref <- yeast_data()

    # Built least-specific first so that the more specific names overwrite
    # them: a synonym must never shadow a real gene name.
    split_map <- function(field) {
      parts <- strsplit(field, ";", fixed = TRUE)
      keep <- lengths(parts) > 0
      stats::setNames(
        rep(ref$accession[keep], lengths(parts[keep])),
        toupper(unlist(parts[keep], use.names = FALSE))
      )
    }

    lookup <- c(
      split_map(ref$synonyms),
      split_map(ref$orf),
      stats::setNames(ref$accession, toupper(ref$gene)),
      stats::setNames(ref$accession, toupper(ref$accession))
    )
    lookup <- lookup[nzchar(names(lookup))]
    # Later entries win, so keep the last of each duplicated name.
    lookup <- lookup[!duplicated(names(lookup), fromLast = TRUE)]

    .sgd_cache$alias_lookup <- lookup
  }
  unname(.sgd_cache$alias_lookup[toupper(trimws(as.character(ids)))])
}

#' Map a single gene symbol to a UniProt accession.
#'
#' @description
#' Single-value form of [yeastToUniprot()], kept because the painter's join
#' falls back to a per-row lookup when a file has no accession column.
#'
#' @param geneSymbol A gene name, ORF name or accession.
#' @return The accession, or `NA_character_`.
#' @export
fct_get_alias <- function(geneSymbol) {
  out <- yeastToUniprot(geneSymbol)[1]
  if (is.na(out)) NA_character_ else out
}

#' Map UniProt accessions back to gene names.
#'
#' @description
#' The reverse of [yeastToUniprot()]. Chain data from the PDB carries
#' accessions, not gene names, so without this only the subunits present in the
#' uploaded logFC file could ever be named in the subunit selector.
#'
#' Falls back to the systematic/ORF name for the many yeast proteins that have
#' no standard gene name, so every chain gets a usable label.
#'
#' @param accessions Character vector of UniProt accessions.
#' @return A character vector of gene names, `NA` where unknown.
#' @export
uniprotToGene <- function(accessions) {
  if (is.null(.sgd_cache$uniprot_lookup)) {
    ref <- yeast_data()
    # Around a fifth of the proteome is uncharacterised and carries no standard
    # gene name; the ORF name is what those are known by.
    label <- ifelse(nzchar(ref$gene), ref$gene, sub(";.*$", "", ref$orf))
    .sgd_cache$uniprot_lookup <- stats::setNames(label, ref$accession)
  }
  out <- unname(.sgd_cache$uniprot_lookup[as.character(accessions)])
  out[!is.na(out) & !nzchar(out)] <- NA_character_
  out
}

#' Join fold changes onto PDB chains.
#'
#' @description
#' Chains carry UniProt accessions, so the fold-change table is keyed to
#' accessions first. A file that already has an accession column is joined
#' directly; otherwise the identifiers are resolved through the bundled yeast
#' reference table.
#'
#' @param logfc A fold-change data frame with `logfc` and either `uniprot` or
#'   `genename`.
#' @param chains Output of [fct_get_chain_df()].
#' @return `chains` with the fold-change columns joined on.
#' @export
fct_join_logfc_chains <- function(logfc, chains) {
  # An accession column is the join key as it stands. Otherwise resolve the
  # identifiers in one vectorised pass rather than a lookup per row.
  if ("uniprot" %in% names(logfc) && any(nzchar(stats::na.omit(logfc$uniprot)))) {
    keyed <- dplyr::mutate(
      logfc,
      # Isoform suffixes never appear in PDBe chain mappings.
      uniprotID = sub("-\\d+$", "", trimws(.data$uniprot))
    )
    # Accessions that are not recognised may still be gene names in disguise.
    unresolved <- is.na(keyed$uniprotID) | !nzchar(keyed$uniprotID)
    if (any(unresolved) && "genename" %in% names(keyed)) {
      keyed$uniprotID[unresolved] <- yeastToUniprot(keyed$genename[unresolved])
    }
  } else {
    keyed <- dplyr::mutate(logfc, uniprotID = yeastToUniprot(.data$genename))
  }

  # One row per accession: duplicates would multiply the chain rows and paint
  # the same chain repeatedly.
  keyed <- keyed %>%
    dplyr::filter(!is.na(.data$uniprotID), nzchar(.data$uniprotID)) %>%
    dplyr::distinct(.data$uniprotID, .keep_all = TRUE)

  merged_df <- chains %>%
    dplyr::left_join(keyed, by = c("uniprot" = "uniprotID"))

  return(merged_df)
}

#' Combine and clean data to create a final color set for the viewer.
#'
#' @param joined_df A data frame from [fct_join_logfc_chains()].
#' @return A `data.frame` with 'sele' and 'colorValue' columns.
#' @importFrom dplyr mutate na_if vars
#' @export
fct_prepare_chainset <- function(joined_df) {

  # Replace missing data hex with a grey code
  namedpdblog2 <- joined_df %>%
    dplyr::mutate(hex_from_scales = dplyr::na_if(hex_from_scales, "")) %>%
    dplyr::mutate_at(dplyr::vars(hex_from_scales), ~tidyr::replace_na(., "#D3D3D3"))

  # Add ':' for calling chain
  namedpdblog2$mappings.chain_id <- paste0(":", trimws(namedpdblog2$mappings.chain_id))

  chainset <- namedpdblog2[c("mappings.chain_id", "hex_from_scales")]
  names(chainset) <- c("sele", "colorValue")

  return(chainset)
}


#' Prepare data for the legend plot.
#'
#' @param df The data frame containing the 'logfc' values.
#' @param range A numeric vector of length 2 defining the color gradient scale.
#'
#' @return A data frame with sorted logFC values and hex colors.
#' @importFrom scales col_numeric
#' @export
fct_prepare_legend_data <- function(df, range) {
  color_values <- pmin(pmax(df$logfc, range[1]), range[2])
  color_func <- scales::col_numeric(c("blue", "white", "red"), domain = c(range[1], range[2]))
  df$hex_from_scales <- color_func(color_values)
  df <- df[order(df$logfc, decreasing = FALSE), ]
  return(df)
}
