library(httr2)
library(xml2)
library(stringr)
library(dplyr)
library(tibble)

list_trendy_files <- function(
    base_url = c(
      "https://gcbo-opendata.s3.eu-west-2.amazonaws.com/",
      "https://s3.eu-west-1.wasabisys.com/gcb-2025-upload/"
    ),
    must_contain = c("S2"),
    prefix = NULL,
    max_keys = 1000L,
    on_forbidden = c("skip", "warn", "return")  # behavior for HTTP 403
) {
  on_forbidden <- match.arg(on_forbidden)

  if (!is.null(prefix) && length(prefix) == 1L && length(base_url) > 1L) {
    prefix <- rep(prefix, length(base_url))
  }
  if (!is.null(prefix) && length(prefix) != length(base_url)) {
    stop("If provided, `prefix` must be NULL, length 1, or same length as `base_url`.")
  }

  list_one_bucket <- function(one_base_url, one_prefix) {
    if (!str_ends(one_base_url, "/")) one_base_url <- paste0(one_base_url, "/")

    all_keys <- character()
    token <- NULL

    repeat {
      q <- list(
        "list-type" = "2",
        "max-keys"  = as.character(max_keys)
      )
      if (!is.null(one_prefix) && nzchar(one_prefix)) q[["prefix"]] <- one_prefix
      if (!is.null(token) && nzchar(token))           q[["continuation-token"]] <- token

      req <- request(one_base_url) |>
        req_url_query(!!!q) |>
        # don't throw on 4xx/5xx; we handle statuses ourselves
        req_error(is_error = function(resp) FALSE)

      resp <- req_perform(req)
      st <- resp_status(resp)

      if (st == 403) {
        msg <- paste0("403 Forbidden (public LIST not allowed) for ", one_base_url)
        if (on_forbidden == "warn") warning(msg, call. = FALSE)
        if (on_forbidden == "skip") return(NULL)
        if (on_forbidden == "return") {
          return(tibble(
            source_base_url = one_base_url,
            key = character(),
            url = character(),
            destination = character(),
            model = character(),
            scenario = character(),
            error = msg
          ))
        }
      }

      if (st >= 400) {
        stop("S3 listing failed (HTTP ", st, ") for ", one_base_url, ".")
      }

      doc <- read_xml(resp_body_string(resp))

      keys_batch <- xml_text(xml_find_all(
        doc, ".//*[local-name()='Contents']/*[local-name()='Key']"
      ))
      all_keys <- c(all_keys, keys_batch)

      is_truncated_txt <- xml_text(xml_find_first(doc, ".//*[local-name()='IsTruncated']"))
      is_truncated <- identical(tolower(is_truncated_txt), "true")
      if (!is_truncated) break

      token <- xml_text(xml_find_first(doc, ".//*[local-name()='NextContinuationToken']"))
      if (is.na(token) || !nzchar(token)) break
    }

    keep <- rep(TRUE, length(all_keys))
    for (p in must_contain) keep <- keep & str_detect(all_keys, regex(p, ignore_case = TRUE))

    tibble(
      source_base_url = one_base_url,
      key = all_keys[keep],
      url = paste0(one_base_url, URLencode(all_keys[keep], reserved = TRUE))
    ) |>
      mutate(
        destination = basename(key),
        model = sapply(str_split(destination, "_"), `[[`, 1),
        scenario = sapply(str_split(destination, "_"), `[[`, 2),
        error = NA_character_
      )
  }

  res <- if (is.null(prefix)) {
    lapply(base_url, \(u) list_one_bucket(u, NULL))
  } else {
    Map(list_one_bucket, base_url, prefix)
  }

  bind_rows(Filter(Negate(is.null), res))
}
