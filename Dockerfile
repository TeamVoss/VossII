FROM debian:buster

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get -y install gcc g++ doxygen flex bison gawk \
                       libz-dev tcl-dev tk-dev libc6-dev imagemagick \
                       clang libreadline-dev python3 # for yosys

VOLUME /vossroot
WORKDIR /vossroot/src
CMD make package
