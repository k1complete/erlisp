FROM erlang:latest

ARG USERNAME=vscode

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
#RUN useradd -m -s /bin/bash erldev
ARG USERNAME=vscode
RUN useradd -m -s /bin/bash $USERNAME && \
    mkdir /home/$USERNAME/.ssh && \
    chown $USERNAME /home/$USERNAME/.ssh

## install sudo and sudoers no passsword
RUN apt-get update && apt-get install -y sudo \
    && echo "$USERNAME ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/$USERNAME \
    && chmod 0440 /etc/sudoers.d/$USERNAME

WORKDIR /workspaces/erlisp
