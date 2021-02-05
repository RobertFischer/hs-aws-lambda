variable "lambda_name" {
  type        = string
  description = "The name of the lambda to create"
}

variable "repo_url" {
  type        = string
  description = "The URL of the ECR repo containing the implementation images"
}


