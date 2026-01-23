
list_trendy_files <- function(base_url = "https://gcbo-opendata.s3.eu-west-2.amazonaws.com/",
                            must_contain = c("S2"),
                            prefix = NULL,
                            max_keys = 1000L) {
  # base_url should end with /
  if (!str_ends(base_url, "/")) base_url <- paste0(base_url, "/")

  all_keys <- character()
  token <- NULL

  repeat {
    req <- request(base_url) |>
      req_url_query(
        "list-type" = 2,
        "max-keys" = max_keys,
        "prefix" = prefix,
        "continuation-token" = token
      )

    resp <- req_perform(req)
    if (resp_status(resp) >= 400) {
      stop("S3 listing failed (HTTP ", resp_status(resp), "). ",
           "If you get 403, the bucket may not allow public LIST.")
    }

    doc <- read_xml(resp_body_string(resp))
    ns  <- xml_ns(doc)

    keys_batch <- xml_text(xml_find_all(doc, ".//d1:Contents/d1:Key", ns))
    all_keys <- c(all_keys, keys_batch)

    is_truncated <- tolower(xml_text(xml_find_first(doc, ".//d1:IsTruncated", ns))) == "true"
    if (!is_truncated) break

    token <- xml_text(xml_find_first(doc, ".//d1:NextContinuationToken", ns))
    if (is.na(token) || token == "") break
  }

  # filter keys that contain ALL required substrings (case-insensitive)
  keep <- rep(TRUE, length(all_keys))
  for (p in must_contain) keep <- keep & str_detect(all_keys, regex(p, ignore_case = TRUE))

  tibble(
    key = all_keys[keep],
    url = paste0(base_url, vapply(all_keys[keep], \(k) URLencode(k, reserved = TRUE), character(1)))
  ) %>%
    mutate(destination = basename(key)) %>%
    mutate(model = sapply(str_split(destination,"_"),
                          "[[",1),
           scenario = sapply(str_split(destination,"_"),
                             "[[",2))
}
