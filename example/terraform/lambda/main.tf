locals {
  image_sha = split(":", data.aws_ecr_image.lambda.id)[1]
}

data aws_iam_policy_document assume_role {
  statement {
    actions = [ "sts:AssumeRole" ]
    principals {
      type = "Service"
      identifiers = [ "lambda.amazonaws.com" ]
    }
  }
}

resource aws_iam_role lambda {
  name_prefix = "${var.lambda_name}_"
  assume_role_policy = data.aws_iam_policy_document.assume_role.json
}

resource aws_cloudwatch_log_group lambda {
  name = "/aws/lambda/${var.lambda_name}"
  retention_in_days = 1
}

data aws_iam_policy_document permissions {
  statement {
    actions = [ "logs:CreateLogStream", "logs:PutLogEvents" ]
    resources = [
      "${aws_cloudwatch_log_group.lambda.arn}:*"
    ]
  }
}

resource aws_iam_role_policy permissions {
  name_prefix = "${var.lambda_name}_"
  role = aws_iam_role.lambda.id
  policy = data.aws_iam_policy_document.permissions.json
}

data aws_ecr_image lambda {
  repository_name = var.lambda_name
  image_tag = "latest"
}

resource aws_lambda_function lambda {
  image_uri = "${var.repo_url}@${data.aws_ecr_image.lambda.id}"
  package_type = "Image"
  function_name = var.lambda_name
  role = aws_iam_role.lambda.arn
  depends_on = [
    aws_iam_role_policy.permissions
  ]
}
