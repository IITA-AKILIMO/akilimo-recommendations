#* Compute recommendations based on input parameters
#* Accepts a JSON payload with user and agronomic data,
#* and returns a JSON response with recommendation results.
#*
#* @tag Recommendation
#* @param req Internal request object (injected by plumber)
#* @param res Internal response object (used to customize status or headers)
#* @post /compute
#* @serializer json
function(req, res) {
  tryCatch({

	run_akilimo(req$postBody)	

  }, error = function(e) {
    res$status <- 500
    print(e)
    data <- list(
      request_token = jsonlite::unbox(request_token),
      message = jsonlite::unbox(e$message),
      trace = jsonlite::unbox(capture.output(e))
    )
    list(status = jsonlite::unbox("error"), data = data)
  })
}

