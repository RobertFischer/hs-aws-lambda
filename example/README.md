# Example Lambda That Executes Compiled Haskell Code

This provides a simple application that executes as a Lambda using the Runtime API library.  You can build and deploy it by executing
`./deploy.sh`: it's documented for you with the intent of being a minimal model of implementing Haskell Lambdas.

The Docker image is built using the standard Docker command-line tools.

The cloud resources are all managed by [Terraform](https://www.terraform.io/intro/index.html), as defined in the `terraform` directory.
