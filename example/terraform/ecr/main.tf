resource aws_ecr_repository repo {
  name = var.lambda_name
  image_tag_mutability = "MUTABLE"
  image_scanning_configuration {
    scan_on_push = true
  }
}

resource aws_ecr_lifecycle_policy cleanup {
  repository = aws_ecr_repository.repo.name
  policy = jsonencode({
    rules = [
      {
        rulePriority = 1
        description = "clean up old untagged images"
        selection = {
          tagStatus = "untagged"
          countType = "sinceImagePushed"
          countUnit = "days"
          countNumber = 1
        }
        action = {
          type = "expire"
        }
      }
    ]
  })
}
