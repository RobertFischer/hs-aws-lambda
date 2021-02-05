#!/bin/bash

set -o pipefail
set -exu

LAMBDA_NAME="${1:-hs_lambda_example}"

cd "$(dirname "$0")"
EXAMPLEDIR="$(pwd)"
cd ..
ROOTDIR="$(pwd)"
cd "$EXAMPLEDIR/terraform"
TFDIR="$(pwd)"

#####################
# Utility Functions #
#####################
function tryBrew() {
	which brew
	brew install "$1" || brew cask install "$1"
	export PATH="$(brew --prefix "$1")/bin:$PATH"
}

function dependencyFailure() {
	echo "Please install the '$1' command $2"
	echo "(And then PLEASE PLEASE PLEASE submit a PR so that it is automatically installed on systems like yours.)"
	exit -1
}

#######################
# Verify Dependencies #
#######################
# TODO Add in support for other package managers
( which terraform && ( terraform -v | grep 'v0.14' ) ) || tryBrew 'terraform@0.14' || dependencyFailure "terraform" 'at version ~> 0.14'
which aws || tryBrew "awscli" || dependencyFailure 'aws'
which docker || tryBrew "docker" || dependencyFailure 'docker'
which jq || tryBrew "jq" || dependencyFailure 'jq'

####################
# Verify Terraform #
####################
cd "$TFDIR"
terraform fmt
terraform init
terraform validate

##################################
# Clean up the build directories #
##################################
for DIR in "$ROOTDIR" "$EXAMPLEDIR"
do
	cd "$DIR"
	stack clean --full
	rm -v *.cabal
done

###################
# Create ECR Repo #
###################
cd "$TFDIR"
echo "lambda_name=\"$LAMBDA_NAME\"" > lambda_name.auto.tfvars
terraform apply -target=module.ecr
ECR_REPO_URL="$(terraform output -raw ecr_repo_url)"  # Capture the ECR Repo URL from Terraform (we'll use it later)
ECR_REPO_SERVER="$(terraform output -raw ecr_repo_server)"  # Capture the ECR Repo server from Terraform (we'll use it later)

##########################
# Build the Docker image #
##########################
cd "$ROOTDIR"
docker build --compress --file "$EXAMPLEDIR/Dockerfile" . --build-arg "lambda_name=$LAMBDA_NAME" --tag "$LAMBDA_NAME:latest"

##############
# Push Image #
##############
cd "$ROOTDIR"
aws ecr get-login-password --region us-east-1 | docker login --username AWS --password-stdin "$ECR_REPO_URL"
docker tag "$LAMBDA_NAME:latest" "$ECR_REPO_URL:latest"
docker push "$ECR_REPO_URL:latest"

#################
# Create Lambda #
#################
cd "$TFDIR"
terraform apply
