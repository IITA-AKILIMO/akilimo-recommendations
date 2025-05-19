library(plumber)
library(ggplot2)
library(jsonlite)

#* @apiTitle Enhanced R Plumber API for health
#* @apiDescription A robust RESTful API with error handling and multiple functionalities
#* @apiVersion 1.0.0
#* @apiTag Health API health check endpoint
#* @apiTag Math Mathematical operation endpoints
#* @apiTag Visualization Data visualization endpoints

#' Global error handler function
#' @param req The request object
#' @param res The response object
#' @param err The error that occurred
#' @post /error
function(req, res, err) {
  res$status <- 500
  list(
    error = TRUE,
    message = err$message,
    timestamp = Sys.time()
  )
}

#* API health check endpoint
#* @tag Health
#* @get /health
#* @response 200 API is healthy
function() {
  list(
    status = "UP",
    version = "1.0.0",
    timestamp = Sys.time(),
    environment = Sys.getenv("R_ENV", "development")
  )
}

#* Echo back the input with optional transformation
#* @tag Utility
#* @param msg The message to echo
#* @param upper Convert to uppercase (true/false)
#* @response 200 Returns the echoed message
#* @get /echo
function(msg = "Hello, World!", upper = FALSE) {
  if (is.null(msg) || msg == "") {
    res$status <- 400
    return(list(error = TRUE, message = "Message cannot be empty"))
  }
  
  result <- msg
  if (upper == TRUE || upper == "true") {
    result <- toupper(result)
  }
  
  list(
    original = msg,
    result = result,
    length = nchar(result),
    timestamp = Sys.time()
  )
}

#* Plot a histogram with customizable parameters
#* @tag Visualization
#* @param n Number of random points to generate (default: 100)
#* @param bins Number of histogram bins (default: 30)
#* @param color Color of the histogram (default: steelblue)
#* @param title Plot title (default: Random Normal Distribution)
#* @response 200 Returns a PNG image
#* @get /plot
#* @png
function(n = 100, bins = 30, color = "steelblue", title = "Random Normal Distribution") {
  # Input validation
  n <- as.numeric(n)
  bins <- as.numeric(bins)
  
  if (is.na(n) || n <= 0) {
    stop("Parameter 'n' must be a positive number")
  }
  
  if (is.na(bins) || bins <= 0) {
    stop("Parameter 'bins' must be a positive number")
  }
  
  # Generate random data
  rand <- rnorm(n)
  
  # Create a more attractive plot with ggplot2
  p <- ggplot(data.frame(x = rand), aes(x = x)) +
    geom_histogram(fill = color, color = "black", alpha = 0.7, bins = bins) +
    labs(
      title = title,
      x = "Value",
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = element_text(size = 12)
    )
  
  print(p)
}

#* Return the sum of two numbers with input validation
#* @tag Math
#* @param a The first number to add
#* @param b The second number to add
#* @response 200 Returns the sum of the two numbers
#* @response 400 Invalid input parameters
#* @get /sum
function(a, b) {
  a_num <- as.numeric(a)
  b_num <- as.numeric(b)
  
  if (is.na(a_num)) {
    res$status <- 400
    return(list(error = TRUE, message = "Parameter 'a' must be a valid number"))
  }
  
  if (is.na(b_num)) {
    res$status <- 400
    return(list(error = TRUE, message = "Parameter 'b' must be a valid number"))
  }
  
  list(
    a = a_num,
    b = b_num,
    sum = a_num + b_num,
    operation = "addition"
  )
}

#* Perform various arithmetic operations
#* @tag Math
#* @param a The first number
#* @param b The second number
#* @param op Operation to perform (add, subtract, multiply, divide)
#* @response 200 Returns the result of the operation
#* @response 400 Invalid input parameters
#* @response 422 Invalid operation or division by zero
#* @get /calculate
function(a, b, op = "add") {
  a_num <- as.numeric(a)
  b_num <- as.numeric(b)
  
  # Input validation
  if (is.na(a_num)) {
    res$status <- 400
    return(list(error = TRUE, message = "Parameter 'a' must be a valid number"))
  }
  
  if (is.na(b_num)) {
    res$status <- 400
    return(list(error = TRUE, message = "Parameter 'b' must be a valid number"))
  }
  
  result <- NULL
  operation <- NULL
  
  # Perform the requested operation
  if (op == "add") {
    result <- a_num + b_num
    operation <- "addition"
  } else if (op == "subtract") {
    result <- a_num - b_num
    operation <- "subtraction"
  } else if (op == "multiply") {
    result <- a_num * b_num
    operation <- "multiplication"
  } else if (op == "divide") {
    if (b_num == 0) {
      res$status <- 422
      return(list(error = TRUE, message = "Division by zero is not allowed"))
    }
    result <- a_num / b_num
    operation <- "division"
  } else {
    res$status <- 422
    return(list(
      error = TRUE,
      message = "Invalid operation. Supported operations: add, subtract, multiply, divide"
    ))
  }
  
  list(
    a = a_num,
    b = b_num,
    operation = operation,
    result = result
  )
}

#* Get summary statistics for a JSON array of numbers
#* @tag Math
#* @param data JSON array of numbers
#* @post /stats
function(req) {
  # Parse the JSON data from the request body
  data <- tryCatch({
    jsonlite::fromJSON(req$postBody)
  }, error = function(e) {
    res$status <- 400
    return(list(error = TRUE, message = "Invalid JSON data format"))
  })
  
  # Validate that data is a numeric vector
  if (!is.numeric(data)) {
    res$status <- 400
    return(list(error = TRUE, message = "Data must be an array of numbers"))
  }
  
  if (length(data) == 0) {
    res$status <- 400
    return(list(error = TRUE, message = "Data array cannot be empty"))
  }
  
  # Calculate summary statistics
  list(
    count = length(data),
    min = min(data),
    max = max(data),
    mean = mean(data),
    median = median(data),
    sum = sum(data),
    std_dev = sd(data),
    variance = var(data),
    quantiles = as.list(quantile(data, probs = c(0.25, 0.5, 0.75)))
  )
}

#* Generate a scatter plot from provided data here
#* @tag Visualization
#* @post /scatter
#* @png
function(req) {
  # Parse the JSON data from the request body
  data <- tryCatch({
    jsonlite::fromJSON(req$postBody)
  }, error = function(e) {
    stop("Invalid JSON data format")
  })
  
  # Validate the data structure
  if (!is.data.frame(data) || !("x" %in% names(data)) || !("y" %in% names(data))) {
    stop("Data must be a JSON object with 'x' and 'y' arrays")
  }
  
  if (!is.numeric(data$x) || !is.numeric(data$y) || length(data$x) != length(data$y)) {
    stop("'x' and 'y' must be numeric arrays of the same length")
  }
  
  # Create a scatter plot
  df <- data.frame(x = data$x, y = data$y)
  
  ggplot(df, aes(x = x, y = y)) +
    geom_point(color = "darkblue", alpha = 0.7, size = 3) +
    geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
    labs(
      title = "Scatter Plot with Trend Line here",
      x = "X Values",
      y = "Y Values"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = element_text(size = 12)
    )
}