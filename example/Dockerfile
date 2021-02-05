# This is intended to be run from the root dir of the repo.
#
# See the sibling file 'deploy.sh' for details

FROM haskell:buster AS build

COPY ./ ./
RUN stack upgrade && stack update

WORKDIR example
RUN stack setup
RUN stack build .

RUN cp -v "$(stack path --local-install-root)"/bin/* /hs-lambda

FROM debian:buster
ARG lambda_name

COPY --from=build /hs-lambda /$lambda_name
RUN ln -s /$lambda_name /hs-lambda
WORKDIR /var/run/$lambda_name
ENTRYPOINT [ "/hs-lambda" ]
