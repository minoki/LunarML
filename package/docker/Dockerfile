# syntax=docker/dockerfile:1
FROM ubuntu:24.04

# The 'mlton' package is only available on amd64.
# Instead of the apt package, we use the binary distribution from GitHub.
# mlton's dependencies are: libc6, libc6-dev, libgmp10, libgmp-dev, gcc.
RUN apt-get update && apt-get install -y libc6 libc6-dev libgmp10 libgmp-dev gcc make lua5.3 patch wget

ARG TARGETARCH
# ADD https://github.com/MLton/mlton/releases/download/on-20241230-release/mlton-20241230-1.amd64-linux.ubuntu-24.04_glibc2.39.tgz mlton-release.tgz
RUN if [ "$TARGETARCH" = "amd64" ]; then \
      wget -O mlton-release.tgz https://github.com/MLton/mlton/releases/download/on-20241230-release/mlton-20241230-1.amd64-linux.ubuntu-24.04_glibc2.39.tgz ; \
    elif [ "$TARGETARCH" = "arm64" ]; then \
      wget -O mlton-release.tgz https://github.com/MLton/mlton/releases/download/on-20241230-release/mlton-20241230-1.arm64-linux.ubuntu-24.04-arm_glibc2.39.tgz ; \
    fi
RUN mkdir mlton-release && tar -x --strip-components=1 -C mlton-release -f mlton-release.tgz
# Install to /usr/local
RUN make -C mlton-release

WORKDIR /work
COPY Makefile version.mk ./
COPY src/ src/
COPY pluto/ pluto/
COPY util/ util/
COPY lib/ lib/
COPY thirdparty/ thirdparty/

RUN mkdir -p bin && make
WORKDIR /work/thirdparty
RUN make install

FROM ubuntu:24.04
ENV PATH="/opt/lunarml/bin:$PATH"
COPY --from=0 /work/bin /opt/lunarml/lib/lunarml/bin
COPY --from=0 /work/lib /opt/lunarml/lib
COPY --chmod=755 package/docker/lunarml /opt/lunarml/bin/
COPY LICENSE /opt/lunarml/
