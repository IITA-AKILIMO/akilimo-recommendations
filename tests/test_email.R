# tests/test_email.R — Email delivery unit and integration tests
#
# Coverage:
#   1. sendEmailReport() skips silently when PDFs list is empty
#   2. .send_smtp()      warns on missing credentials (no network call)
#   3. .send_mailtrap()  warns on missing MAILTRAP_API_KEY (no network call)
#   4. .send_mailgun()   warns on missing credentials (no network call)
#   5. sendEmailReport() routes to the correct provider via EMAIL_PROVIDER env var
#   6. Attachment base64 encoding round-trips correctly
#   7. Live send (opt-in) — sends a real PDF via the configured provider
#
# Run from project root:
#   Rscript tests/test_email.R
#
# Live send (requires real credentials in .env or environment):
#   TEST_EMAIL_LIVE=true TEST_EMAIL_TO=you@example.com Rscript tests/test_email.R

akpath <- "."
setwd(akpath)

source(file.path(akpath, "R", "logging.R"))
source(file.path(akpath, "R", "markdown.R"))
source(file.path(akpath, "R", "html_helpers.R"))
source(file.path(akpath, "R", "pdf_builders.R"))
source(file.path(akpath, "R", "sms_email.R"))

# ── Temp dir ─────────────────────────────────────────────────────────────────

dir.create("temp", showWarnings = FALSE)
req_dir <- file.path("temp", paste0("test_email_", format(Sys.time(), "%Y%m%d_%H%M%S")))
dir.create(req_dir, showWarnings = FALSE)
set_temp_dir(req_dir)

# ── Helpers ───────────────────────────────────────────────────────────────────

pass <- 0L
fail <- 0L

ok <- function(label) {
  cat(sprintf("PASS  %s\n", label))
  pass <<- pass + 1L
}

fail_test <- function(label, reason) {
  cat(sprintf("FAIL  %s — %s\n", label, reason))
  fail <<- fail + 1L
}

# Capture warnings emitted during expr without stopping execution.
capture_warnings <- function(expr) {
  warns <- character(0)
  withCallingHandlers(expr, warning = function(w) {
    warns <<- c(warns, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  warns
}

# Temporarily override env vars for the duration of expr, then restore.
with_env <- function(vars, expr) {
  old <- lapply(names(vars), function(k) Sys.getenv(k, unset = NA_character_))
  names(old) <- names(vars)
  on.exit({
    for (k in names(old)) {
      if (is.na(old[[k]])) Sys.unsetenv(k) else do.call(Sys.setenv, setNames(list(old[[k]]), k))
    }
  })
  for (k in names(vars)) {
    if (is.na(vars[[k]])) Sys.unsetenv(k) else do.call(Sys.setenv, setNames(list(vars[[k]]), k))
  }
  force(expr)
}

# Minimal dummy user object for sendEmailReport.
dummy_user <- function(email = "test@example.com") {
  list(Email = email, Name = "Test User", PhoneNr = "0000000000",
       send_email = TRUE, send_SMS = FALSE)
}

# ── 1. Empty PDFs — no send, no warning ──────────────────────────────────────

cat("\n── 1. Empty PDFs guard ─────────────────────────────────────────────────\n")

tryCatch({
  warns <- capture_warnings(sendEmailReport(dummy_user(), character(0)))
  if (length(warns) == 0) {
    ok("sendEmailReport(PDFs=character(0)) returns silently with no warnings")
  } else {
    fail_test("sendEmailReport empty PDFs", paste("unexpected warnings:", paste(warns, collapse = "; ")))
  }
}, error = function(e) {
  fail_test("sendEmailReport empty PDFs", paste("threw error:", conditionMessage(e)))
})

tryCatch({
  warns <- capture_warnings(sendEmailReport(dummy_user(), NULL))
  if (length(warns) == 0) {
    ok("sendEmailReport(PDFs=NULL) returns silently with no warnings")
  } else {
    fail_test("sendEmailReport NULL PDFs", paste("unexpected warnings:", paste(warns, collapse = "; ")))
  }
}, error = function(e) {
  fail_test("sendEmailReport NULL PDFs", paste("threw error:", conditionMessage(e)))
})

# ── 2. .send_smtp() — missing credentials ────────────────────────────────────

cat("\n── 2. SMTP missing credentials ─────────────────────────────────────────\n")

with_env(list(EMAIL_HOST = NA, EMAIL_USER = NA, EMAIL_PASSWORD = NA), {
  tryCatch({
    warns <- capture_warnings(.send_smtp(
      to       = "test@example.com",
      from     = "from@example.com",
      subject  = "test",
      body_txt = "test",
      PDFs     = c("/fake/test.pdf")
    ))
    if (any(grepl("missing env vars", warns, ignore.case = TRUE))) {
      ok(".send_smtp() warns on missing EMAIL_HOST/USER/PASSWORD")
    } else {
      fail_test(".send_smtp() missing creds", paste("expected 'missing env vars' warning, got:", paste(warns, collapse = "; ")))
    }
  }, error = function(e) {
    fail_test(".send_smtp() missing creds", paste("threw error:", conditionMessage(e)))
  })
})

with_env(list(EMAIL_HOST = "smtp.example.com", EMAIL_USER = NA, EMAIL_PASSWORD = NA), {
  tryCatch({
    warns <- capture_warnings(.send_smtp("t@e.com", "f@e.com", "s", "b", c("/fake/test.pdf")))
    if (any(grepl("missing env vars", warns, ignore.case = TRUE))) {
      ok(".send_smtp() warns when only EMAIL_HOST is set (USER and PASSWORD missing)")
    } else {
      fail_test(".send_smtp() partial creds", paste("expected warning, got:", paste(warns, collapse = "; ")))
    }
  }, error = function(e) {
    fail_test(".send_smtp() partial creds", paste("threw error:", conditionMessage(e)))
  })
})

# ── 3. .send_mailtrap() — missing API key ────────────────────────────────────

cat("\n── 3. Mailtrap missing API key ─────────────────────────────────────────\n")

with_env(list(MAILTRAP_API_KEY = NA), {
  tryCatch({
    warns <- capture_warnings(.send_mailtrap(
      to       = "test@example.com",
      from     = "from@example.com",
      subject  = "test",
      body_txt = "test",
      PDFs     = c("/fake/test.pdf")
    ))
    if (any(grepl("MAILTRAP_API_KEY", warns))) {
      ok(".send_mailtrap() warns when MAILTRAP_API_KEY is not set")
    } else {
      fail_test(".send_mailtrap() missing key", paste("expected MAILTRAP_API_KEY warning, got:", paste(warns, collapse = "; ")))
    }
  }, error = function(e) {
    fail_test(".send_mailtrap() missing key", paste("threw error:", conditionMessage(e)))
  })
})

with_env(list(MAILTRAP_API_KEY = ""), {
  tryCatch({
    warns <- capture_warnings(.send_mailtrap("t@e.com", "f@e.com", "s", "b", c("/fake/test.pdf")))
    if (any(grepl("MAILTRAP_API_KEY", warns))) {
      ok(".send_mailtrap() warns when MAILTRAP_API_KEY is empty string")
    } else {
      fail_test(".send_mailtrap() empty key", paste("expected MAILTRAP_API_KEY warning, got:", paste(warns, collapse = "; ")))
    }
  }, error = function(e) {
    fail_test(".send_mailtrap() empty key", paste("threw error:", conditionMessage(e)))
  })
})

# ── 4. .send_mailgun() — missing credentials ─────────────────────────────────

cat("\n── 4. Mailgun missing credentials ──────────────────────────────────────\n")

with_env(list(MAILGUN_API_KEY = NA, MAILGUN_DOMAIN = NA), {
  tryCatch({
    warns <- capture_warnings(.send_mailgun(
      to       = "test@example.com",
      from     = "from@example.com",
      subject  = "test",
      body_txt = "test",
      PDFs     = c("/fake/test.pdf")
    ))
    if (any(grepl("missing env vars", warns, ignore.case = TRUE))) {
      ok(".send_mailgun() warns on missing MAILGUN_API_KEY/MAILGUN_DOMAIN")
    } else {
      fail_test(".send_mailgun() missing creds", paste("expected 'missing env vars' warning, got:", paste(warns, collapse = "; ")))
    }
  }, error = function(e) {
    fail_test(".send_mailgun() missing creds", paste("threw error:", conditionMessage(e)))
  })
})

with_env(list(MAILGUN_API_KEY = "key123", MAILGUN_DOMAIN = NA), {
  tryCatch({
    warns <- capture_warnings(.send_mailgun("t@e.com", "f@e.com", "s", "b", c("/fake/test.pdf")))
    if (any(grepl("missing env vars", warns, ignore.case = TRUE))) {
      ok(".send_mailgun() warns when only MAILGUN_DOMAIN is missing")
    } else {
      fail_test(".send_mailgun() missing domain", paste("expected warning, got:", paste(warns, collapse = "; ")))
    }
  }, error = function(e) {
    fail_test(".send_mailgun() missing domain", paste("threw error:", conditionMessage(e)))
  })
})

# ── 5. Provider routing via EMAIL_PROVIDER ────────────────────────────────────

cat("\n── 5. Provider routing ─────────────────────────────────────────────────\n")

# Create a small real file so sendEmailReport's length(PDFs) > 0 guard passes.
# The provider functions return early on missing creds before reading the file.
fake_pdf <- file.path(req_dir, "fake.pdf")
writeLines("%PDF-1.4 fake", fake_pdf)

with_env(list(EMAIL_PROVIDER = "smtp", EMAIL_HOST = NA, EMAIL_USER = NA, EMAIL_PASSWORD = NA), {
  tryCatch({
    warns <- capture_warnings(sendEmailReport(dummy_user(), c(fake_pdf)))
    if (any(grepl("missing env vars", warns, ignore.case = TRUE))) {
      ok("sendEmailReport routes to SMTP when EMAIL_PROVIDER=smtp")
    } else {
      fail_test("routing smtp", paste("expected smtp warning, got:", paste(warns, collapse = "; ")))
    }
  }, error = function(e) fail_test("routing smtp", conditionMessage(e)))
})

with_env(list(EMAIL_PROVIDER = "mailtrap", MAILTRAP_API_KEY = NA), {
  tryCatch({
    warns <- capture_warnings(sendEmailReport(dummy_user(), c(fake_pdf)))
    if (any(grepl("MAILTRAP_API_KEY", warns))) {
      ok("sendEmailReport routes to Mailtrap when EMAIL_PROVIDER=mailtrap")
    } else {
      fail_test("routing mailtrap", paste("expected mailtrap warning, got:", paste(warns, collapse = "; ")))
    }
  }, error = function(e) fail_test("routing mailtrap", conditionMessage(e)))
})

with_env(list(EMAIL_PROVIDER = "mailgun", MAILGUN_API_KEY = NA, MAILGUN_DOMAIN = NA), {
  tryCatch({
    warns <- capture_warnings(sendEmailReport(dummy_user(), c(fake_pdf)))
    if (any(grepl("missing env vars", warns, ignore.case = TRUE))) {
      ok("sendEmailReport routes to Mailgun when EMAIL_PROVIDER=mailgun")
    } else {
      fail_test("routing mailgun", paste("expected mailgun warning, got:", paste(warns, collapse = "; ")))
    }
  }, error = function(e) fail_test("routing mailgun", conditionMessage(e)))
})

with_env(list(EMAIL_PROVIDER = "unknown_provider", EMAIL_HOST = NA, EMAIL_USER = NA, EMAIL_PASSWORD = NA), {
  tryCatch({
    warns <- capture_warnings(sendEmailReport(dummy_user(), c(fake_pdf)))
    # Unknown provider falls through to smtp (switch default)
    if (any(grepl("missing env vars", warns, ignore.case = TRUE))) {
      ok("sendEmailReport falls back to SMTP for unknown EMAIL_PROVIDER")
    } else {
      fail_test("routing unknown provider", paste("expected smtp fallback warning, got:", paste(warns, collapse = "; ")))
    }
  }, error = function(e) fail_test("routing unknown provider", conditionMessage(e)))
})

# ── 6. Attachment base64 encoding round-trip ─────────────────────────────────

cat("\n── 6. Base64 attachment encoding ───────────────────────────────────────\n")

tryCatch({
  # Write known bytes to a temp file, encode, then decode and compare.
  bin_file <- file.path(req_dir, "encode_test.bin")
  original <- as.raw(c(0x25, 0x50, 0x44, 0x46, 0x2d, 0x31, 0x2e, 0x34))  # "%PDF-1.4"
  writeBin(original, bin_file)

  raw_in  <- readBin(bin_file, "raw", n = file.info(bin_file)$size)
  encoded <- base64enc::base64encode(raw_in)
  decoded <- base64enc::base64decode(encoded)

  if (identical(decoded, original)) {
    ok("base64 round-trip: encode then decode returns identical bytes")
  } else {
    fail_test("base64 round-trip", "decoded bytes differ from original")
  }
}, error = function(e) {
  fail_test("base64 round-trip", conditionMessage(e))
})

tryCatch({
  # Verify the attachment list structure that .send_mailtrap builds
  # (replicated here to confirm field names and types are correct).
  bin_file <- file.path(req_dir, "attach_struct_test.pdf")
  writeBin(as.raw(c(0x25, 0x50, 0x44, 0x46)), bin_file)  # "%PDF"

  raw <- readBin(bin_file, "raw", n = file.info(bin_file)$size)
  att <- list(
    content     = jsonlite::unbox(base64enc::base64encode(raw)),
    filename    = jsonlite::unbox(basename(bin_file)),
    type        = jsonlite::unbox("application/pdf"),
    disposition = jsonlite::unbox("attachment")
  )

  ok_fields <- all(c("content", "filename", "type", "disposition") %in% names(att))
  ok_type   <- att$type == "application/pdf"
  ok_disp   <- att$disposition == "attachment"
  ok_name   <- att$filename == basename(bin_file)

  if (ok_fields && ok_type && ok_disp && ok_name) {
    ok("Mailtrap attachment list has correct fields (content/filename/type/disposition)")
  } else {
    fail_test("Mailtrap attachment structure",
              sprintf("fields_ok=%s type_ok=%s disp_ok=%s name_ok=%s",
                      ok_fields, ok_type, ok_disp, ok_name))
  }
}, error = function(e) {
  fail_test("Mailtrap attachment structure", conditionMessage(e))
})

# ── 7. PDF attachment tests ───────────────────────────────────────────────────
#
# Verifies that real PDF files (WeasyPrint or minimal dummy) are:
#   a. Readable and have valid %PDF magic bytes
#   b. Base64-encoded correctly so the decoded bytes match the original file
#   c. Represented correctly in a multi-attachment list (count, filenames)
#   d. Reported in the INFO log line emitted by sendEmailReport

cat("\n── 7. PDF attachment tests ──────────────────────────────────────────────\n")

# Capture message() output (log_write calls message() for every line it writes).
capture_messages <- function(expr) {
  msgs <- character(0)
  withCallingHandlers(expr, message = function(m) {
    msgs <<- c(msgs, conditionMessage(m))
    invokeRestart("muffleMessage")
  })
  msgs
}

# Create a test PDF — real WeasyPrint output where available, minimal dummy otherwise.
make_test_pdf <- function(path, label = "Test") {
  if (nchar(Sys.which("weasyprint")) > 0) {
    html <- sprintf(
      '<!DOCTYPE html><html><head><meta charset="UTF-8">
<style>@page{size:A4;margin:2cm} body{font-family:Arial;font-size:12px}</style>
</head><body><h1>AKILIMO %s</h1><p>Attachment test PDF.</p></body></html>',
      label
    )
    tryCatch(
      render_pdf(html, path),
      error = function(e) {
        writeBin(charToRaw(sprintf("%%PDF-1.4\n%%%% %s dummy", label)), path)
      }
    )
  } else {
    writeBin(charToRaw(sprintf("%%PDF-1.4\n%%%% %s dummy", label)), path)
  }
  path
}

pdf_a <- make_test_pdf(file.path(req_dir, "attachment_a.pdf"), "Attachment A")
pdf_b <- make_test_pdf(file.path(req_dir, "attachment_b.pdf"), "Attachment B")

# 7a. Files exist and are non-empty
tryCatch({
  a_ok <- file.exists(pdf_a) && file.size(pdf_a) > 0
  b_ok <- file.exists(pdf_b) && file.size(pdf_b) > 0
  if (a_ok && b_ok) {
    ok(sprintf("Test PDFs created (%d bytes, %d bytes)", file.size(pdf_a), file.size(pdf_b)))
  } else {
    fail_test("Test PDF creation", sprintf("pdf_a exists=%s pdf_b exists=%s", a_ok, b_ok))
  }
}, error = function(e) fail_test("Test PDF creation", conditionMessage(e)))

# 7b. Magic bytes: both files start with %PDF
tryCatch({
  magic_a <- rawToChar(readBin(pdf_a, "raw", n = 4))
  magic_b <- rawToChar(readBin(pdf_b, "raw", n = 4))
  if (magic_a == "%PDF" && magic_b == "%PDF") {
    ok("Both test PDFs start with %PDF magic bytes")
  } else {
    fail_test("PDF magic bytes", sprintf("pdf_a='%s' pdf_b='%s'", magic_a, magic_b))
  }
}, error = function(e) fail_test("PDF magic bytes", conditionMessage(e)))

# 7c. Single-PDF attachment: base64 content decodes back to the original file bytes
tryCatch({
  original <- readBin(pdf_a, "raw", n = file.info(pdf_a)$size)
  encoded  <- base64enc::base64encode(original)
  decoded  <- base64enc::base64decode(encoded)
  if (identical(decoded, original)) {
    ok(sprintf("Single PDF base64 round-trip: %d bytes preserved exactly", length(original)))
  } else {
    fail_test("Single PDF base64 round-trip",
              sprintf("original %d bytes vs decoded %d bytes", length(original), length(decoded)))
  }
}, error = function(e) fail_test("Single PDF base64 round-trip", conditionMessage(e)))

# 7d. Multi-PDF attachment list: two entries, each with correct filename and content
tryCatch({
  build_attachments <- function(paths) {
    lapply(paths, function(f) {
      raw <- readBin(f, "raw", n = file.info(f)$size)
      list(
        content     = jsonlite::unbox(base64enc::base64encode(raw)),
        filename    = jsonlite::unbox(basename(f)),
        type        = jsonlite::unbox("application/pdf"),
        disposition = jsonlite::unbox("attachment")
      )
    })
  }

  atts <- build_attachments(c(pdf_a, pdf_b))
  names_ok  <- (atts[[1]]$filename == basename(pdf_a)) &&
               (atts[[2]]$filename == basename(pdf_b))
  count_ok  <- length(atts) == 2
  # Verify each decoded content matches its source file
  decoded_a <- base64enc::base64decode(atts[[1]]$content)
  decoded_b <- base64enc::base64decode(atts[[2]]$content)
  content_ok <- identical(decoded_a, readBin(pdf_a, "raw", n = file.info(pdf_a)$size)) &&
                identical(decoded_b, readBin(pdf_b, "raw", n = file.info(pdf_b)$size))

  if (count_ok && names_ok && content_ok) {
    ok("Two-PDF attachment list: count, filenames, and content all correct")
  } else {
    fail_test("Two-PDF attachment list",
              sprintf("count_ok=%s names_ok=%s content_ok=%s", count_ok, names_ok, content_ok))
  }
}, error = function(e) fail_test("Two-PDF attachment list", conditionMessage(e)))

# 7e. sendEmailReport INFO log mentions correct attachment count and filename (1 PDF)
tryCatch({
  msgs <- character(0)
  with_env(list(EMAIL_PROVIDER = "smtp", EMAIL_HOST = NA, EMAIL_USER = NA, EMAIL_PASSWORD = NA), {
    msgs <- capture_messages(
      capture_warnings(sendEmailReport(dummy_user(), c(pdf_a)))
    )
  })
  info_line <- msgs[grepl("\\[INFO\\].*attachment", msgs, ignore.case = TRUE)]
  count_ok  <- any(grepl("1 attachment", info_line))
  name_ok   <- any(grepl(basename(pdf_a), info_line, fixed = TRUE))
  if (count_ok && name_ok) {
    ok("sendEmailReport INFO log: 1 attachment with correct filename")
  } else {
    fail_test("sendEmailReport INFO log (1 PDF)",
              sprintf("count_ok=%s name_ok=%s log='%s'",
                      count_ok, name_ok, paste(info_line, collapse = "|")))
  }
}, error = function(e) fail_test("sendEmailReport INFO log (1 PDF)", conditionMessage(e)))

# 7f. sendEmailReport INFO log mentions correct attachment count (2 PDFs)
tryCatch({
  msgs <- character(0)
  with_env(list(EMAIL_PROVIDER = "smtp", EMAIL_HOST = NA, EMAIL_USER = NA, EMAIL_PASSWORD = NA), {
    msgs <- capture_messages(
      capture_warnings(sendEmailReport(dummy_user(), c(pdf_a, pdf_b)))
    )
  })
  info_line <- msgs[grepl("\\[INFO\\].*attachment", msgs, ignore.case = TRUE)]
  count_ok  <- any(grepl("2 attachment", info_line))
  both_ok   <- any(grepl(basename(pdf_a), info_line, fixed = TRUE)) &&
               any(grepl(basename(pdf_b), info_line, fixed = TRUE))
  if (count_ok && both_ok) {
    ok("sendEmailReport INFO log: 2 attachments with both filenames listed")
  } else {
    fail_test("sendEmailReport INFO log (2 PDFs)",
              sprintf("count_ok=%s both_names_ok=%s log='%s'",
                      count_ok, both_ok, paste(info_line, collapse = "|")))
  }
}, error = function(e) fail_test("sendEmailReport INFO log (2 PDFs)", conditionMessage(e)))

# ── 8. Live send (opt-in) ─────────────────────────────────────────────────────

cat("\n── 8. Live send ────────────────────────────────────────────────────────\n")

live <- identical(Sys.getenv("TEST_EMAIL_LIVE"), "true")
to   <- Sys.getenv("TEST_EMAIL_TO", unset = "")

if (!live) {
  cat("SKIP  Live send — set TEST_EMAIL_LIVE=true and TEST_EMAIL_TO=<address> to enable\n")
} else if (!nzchar(to)) {
  cat("SKIP  Live send — TEST_EMAIL_TO is not set\n")
} else {

  cat(sprintf("  Recipient : %s\n", to))
  cat(sprintf("  Provider  : %s\n", Sys.getenv("EMAIL_PROVIDER", unset = "smtp")))

  # Reuse the PDF already generated in section 7, or create a fresh one with a
  # timestamp label so the recipient can confirm exactly when the email was sent.
  live_pdf <- file.path(req_dir, "live_test_attachment.pdf")
  cat("  Generating test PDF...\n")
  make_test_pdf(live_pdf,
                sprintf("Email Test %s",
                        format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")))
  cat(sprintf("  PDF: %s (%d bytes)\n", live_pdf, file.size(live_pdf)))

  if (!file.exists(live_pdf) || file.size(live_pdf) == 0) {
    fail_test("Live send", "could not create test PDF to attach")
  } else {
    cat("  Sending email...\n")
    user_live <- dummy_user(email = to)
    tryCatch({
      warns <- capture_warnings(sendEmailReport(user_live, c(live_pdf)))
      # A warning about missing creds means the send was blocked — treat as failure.
      cred_warn <- any(grepl("missing env vars|not set|API_KEY", warns, ignore.case = TRUE))
      if (cred_warn) {
        fail_test("Live send", paste("credentials missing:", paste(warns, collapse = "; ")))
      } else if (length(warns) > 0) {
        fail_test("Live send", paste("unexpected warnings:", paste(warns, collapse = "; ")))
      } else {
        ok(sprintf("Live send via %s to %s succeeded",
                   Sys.getenv("EMAIL_PROVIDER", unset = "smtp"), to))
      }
    }, error = function(e) {
      fail_test("Live send", conditionMessage(e))
    })
  }
}

# ── Summary ───────────────────────────────────────────────────────────────────

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
cat(sprintf("Temp dir: %s\n", temp_dir()))
if (fail > 0) quit(status = 1L)
