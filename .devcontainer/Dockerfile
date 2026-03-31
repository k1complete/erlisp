FROM erlang:latest

# Install additional dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    git \
    ssh \
    openssh-client \
    vim \
    nano \
    && rm -rf /var/lib/apt/lists/*

# Install rebar3
RUN curl -fsSL https://s3.amazonaws.com/rebar3/rebar3 -o /usr/local/bin/rebar3 && \
    chmod +x /usr/local/bin/rebar3

# Create a non-root user for development (optional, adjust if needed)
# RUN useradd -m -s /bin/bash erldev

WORKDIR /workspace
