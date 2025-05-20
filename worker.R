library(redux)
library(jsonlite)
library(dotenv)

# Load .env and env vars (same way as your API)
dotenv::load_dot_env()

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

cat("Worker started, waiting for jobs...\n")

while(TRUE) {
  # BRPOP blocks until a job arrives on the "jobs" list
  job_data <- r$BRPOP("jobs", 0)[[2]]  # returns list(queue_name, job_json)
  cat("Recommendation job received:", substr(job_data, 1, 60), "...\n")

  # Parse job JSON
  job <- fromJSON(job_data)
  job_id <- job$id
  input <- job$input

  # --- Do your long-running computation here ---

  # Example: sum values from input$values
  result <- list(
    status = "done",
    job_id = job_id,
    computed = sum(unlist(input$values)),
    finished_at = Sys.time()
  )

  # Save result back to Redis using key "result:<job_id>"
  r$SET(paste0("result:", job_id), toJSON(result, auto_unbox = TRUE))

  cat("Job", job_id, "completed and result stored.\n")
}
