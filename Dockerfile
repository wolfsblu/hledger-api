# Multi-stage build for hledger-api
# Stage 1: Build the application
FROM debian:bookworm-slim AS builder

# Install build dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        bash \
        ca-certificates \
        curl \
        gcc \
        g++ \
        gnupg \
        libc6-dev \
        libffi-dev \
        libgmp-dev \
        libnuma-dev \
        libtinfo-dev \
        netbase \
        zlib1g-dev \
        make \
        xz-utils \
        git && \
    rm -rf /var/lib/apt/lists/*

# Install Stack
RUN curl -sSL https://get.haskellstack.org/ | bash

# Set working directory
WORKDIR /build

# Copy stack configuration files first (better layer caching)
COPY stack.yaml stack.yaml.lock package.yaml ./

# Let Stack handle GHC installation
RUN stack setup

# Download dependencies
RUN stack build --only-dependencies

# Copy source code
COPY . .

# Build with optimizations
RUN stack build \
    --ghc-options='-O2' \
    --copy-bins \
    --local-bin-path /build/bin

# Strip debug symbols to reduce binary size
RUN strip /build/bin/hledger-api-exe

# Stage 2: Minimal runtime image
FROM debian:bookworm-slim

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        libgmp10 \
        libtinfo6 \
        libffi8 \
        libnuma1 && \
    rm -rf /var/lib/apt/lists/*

COPY --from=builder /build/bin/hledger-api-exe /usr/local/bin/hledger-api

# Set working directory and ensure it's owned by nobody
WORKDIR /data
RUN chown nobody:nogroup /data

# Run as nobody user (UID 65534)
USER nobody

ENV LEDGER_FILE=/data/ledger.journal

EXPOSE 8080

HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD timeout 2 bash -c "</dev/tcp/localhost/8080" || exit 1

ENTRYPOINT ["/usr/local/bin/hledger-api"]
CMD ["--port", "8080", "--host", "0.0.0.0"]
