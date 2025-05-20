library(plumber)
library(jsonlite)
library(redux)
library(uuid)

# Load .env
dotenv::load_dot_env()

# Initialize Redis client globally
# Configuration
redis_host     <- Sys.getenv("REDIS_HOST")
redis_port     <- as.integer(Sys.getenv("REDIS_PORT"))
redis_password <- Sys.getenv("REDIS_PASSWORD")

# Create config
config <- redis_config(
  host = redis_host,
  port = redis_port,
  password = redis_password
)

r <- redux::hiredis(config = config)

#* @post /compute
#* @json
function(req, res) {
  input <- jsonlite::fromJSON(req$postBody)
  job_id <- UUIDgenerate()

  job <- list(
    id = job_id,
    input = input
  )

  # Push job as JSON string to Redis queue "jobs"
  r$LPUSH("jobs", jsonlite::toJSON(job, auto_unbox = TRUE))

  # Return immediately with job ID
  res$status <- 202 # Accepted
  list(status = "queued", job_id = job_id)
}
