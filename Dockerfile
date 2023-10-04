# syntax=docker/dockerfile:1
FROM ubuntu:latest

ARG DEBIAN_FRONTEND=noninteractive

# GHCup Install start
RUN \
    apt-get update -y && \
    apt-get install -y --no-install-recommends \
        curl \
        libnuma-dev \
        zlib1g-dev \
        libgmp-dev \
        libgmp10 \
        git \
        wget \
        lsb-release \
        software-properties-common \
        gnupg2 \
        apt-transport-https \
        gcc \
        autoconf \
        automake \
        build-essential

# install gpg keys
ARG GPG_KEY=7D1E8AFD1D4A16D71FADA2F2CCC85C0E40C06A8C
RUN gpg --batch --keyserver keys.openpgp.org --recv-keys $GPG_KEY

# install ghcup
RUN \
    if [ "$TARGETARCH" = "amd64" ]; then ARCHITECTURE="x86_64"; elif [ "$TARGETARCH" = "arm/v7" ]; then ARCHITECTURE="armv7"; elif [ "$TARGETARCH" = "arm64" ]; then ARCHITECTURE="aarch64"; else ARCHITECTURE="x86_64"; fi && \
    curl https://downloads.haskell.org/~ghcup/${ARCHITECTURE}-linux-ghcup > /usr/bin/ghcup && \
    chmod +x /usr/bin/ghcup && \
    ghcup config set gpg-setting GPGStrict
# GHCup Install end

# Install Stack
ARG STACK=2.11.1
RUN ghcup -v install stack --isolate /usr/bin --force ${STACK}
RUN apt-get install -y libz3-dev libtinfo-dev tree

# Compile the tests, so they're available in the image!
WORKDIR /app
COPY . .
RUN stack test --no-run-tests

ENTRYPOINT bash
