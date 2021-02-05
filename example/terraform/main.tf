module "ecr" {
  source      = "./ecr"
  lambda_name = var.lambda_name
}

module "lambda" {
  source      = "./lambda"
  lambda_name = var.lambda_name
  repo_url    = module.ecr.repo_url
}
