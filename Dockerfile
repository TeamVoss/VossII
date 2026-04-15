# FROM debian:bookworm
FROM gdnmhr/yosys:debian-bookworm-v0.62

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
	apt-get -y install --no-install-recommends \
	build-essential \
	gawk git make python3 lld bison clang flex \
	libffi-dev libfl-dev libreadline-dev pkg-config tcl-dev tk-dev zlib1g-dev \
	graphviz xdot \
	# rumi 260415 for BSD Editline
	libedit-dev && \
	apt-get clean && \
	rm -rf /var/lib/apt/lists/*

VOLUME /vossroot
WORKDIR /vossroot/src