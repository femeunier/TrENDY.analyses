rm(list = ls())

# sftp user-160@trendydl.exeter.ac.uk
# iethiyai?K8jei2A

# This works
# scp "user-160@trendydl.exeter.ac.uk:/gcb2025/LAND/INPUT/crujra3/*v3.02*" /data/gent/vo/000/gvo00074/felicien/TrENDY/inputs/

# scp "user-160@trendydl.exeter.ac.uk:/gcb2025/LAND/INPUT/Radiation/*" /data/gent/vo/000/gvo00074/felicien/TrENDY/inputs/


rm(list = ls())

host       <- "user-160@trendydl.exeter.ac.uk"
password   <- "iethiyai?K8jei2A"     # <-- change if needed
remote_dir <- "/gcb2025/LAND/INPUT/crujra3"
local_dir  <- "/data/gent/vo/000/gvo00074/felicien/TrENDY/inputs/localdir"
pattern    <- "2024"                 # regex filter

dir.create(local_dir, showWarnings = FALSE, recursive = TRUE)

has_expect <- nzchar(Sys.which("expect"))

run_expect_sftp <- function(cmds, capture = TRUE) {
  # cmds: character vector of sftp commands (without newlines)
  tf <- tempfile(fileext = ".exp")
  script <- paste0(
    "set timeout 120\n",
    "log_user 1\n",
    "spawn sftp -o StrictHostKeyChecking=no ", host, "\n",
    "expect {\n",
    "  -re \".*assword:.*\" { send \"", gsub("\\\\", "\\\\\\\\", password), "\\r\"; exp_continue }\n",
    "  -re \"sftp>\" {}\n",
    "}\n",
    paste(sprintf("send \"%s\\r\"\n", cmds), collapse = ""),
    "send \"bye\\r\"\n",
    "expect eof\n"
  )
  writeLines(script, tf)
  out <- tryCatch(
    system2("expect", tf, stdout = if (capture) TRUE else "", stderr = TRUE),
    error = function(e) { attr(e, "message"); character() }
  )
  unlink(tf)
  out
}

if (!has_expect) {
  message("`expect` not found. Falling back to interactive sftp.\n",
          "You will be prompted for your password in the terminal.\n",
          "Run these commands manually if needed:\n",
          sprintf("  sftp %s", host), "\n",
          sprintf("  sftp> cd %s", remote_dir), "\n",
          "  sftp> ls -1\n",
          "  sftp> lcd ", local_dir, "\n",
          "  sftp> get -p <filename>\n",
          "  sftp> bye\n")
} else {
  # 1) List files remotely
  list_cmds <- c(sprintf("cd %s", remote_dir), "ls -1")
  lst <- run_expect_sftp(list_cmds, capture = TRUE)

  # Extract only file names from sftp output lines
  # sftp often prints like "file1\nfile2\n" plus some banner lines; keep plain names
  lines <- trimws(lst)
  # Heuristic: keep entries that do not contain spaces and do not start with "sftp" or "Connecting"
  files_remote <- lines[nzchar(lines) &
                          !grepl("^sftp>|^Connecting|^Warning|^Authenticated|^Using", lines)]
  # Remove any directory listing cruft (if your server colors/columns, adjust as needed)
  files_remote <- files_remote[!grepl("^cd |^ls -1|^bye$", files_remote)]

  # 2) Filter by pattern
  files2download <- files_remote[grepl(pattern, files_remote)]
  cat("Files matching pattern:\n")
  print(files2download)

  # 3) Download matches (batch them in one session)
  if (length(files2download)) {
    dl_cmds <- c(
      sprintf("lcd %s", local_dir),
      sprintf("cd %s", remote_dir),
      sprintf("get -p %s", files2download)
    )
    run_expect_sftp(dl_cmds, capture = FALSE)
    cat("Done. Files saved in:", local_dir, "\n")
  } else {
    message("No files matched pattern: ", pattern)
  }
}

