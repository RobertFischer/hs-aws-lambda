output "repo_url" {
  value = aws_ecr_repository.repo.repository_url
}

output "repo_server" {
  value = split("/", aws_ecr_repository.repo.repository_url)[0]
}
