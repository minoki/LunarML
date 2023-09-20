# syntax=docker/dockerfile:1
FROM ubuntu:23.04

# mlton's binary is only available on amd64
RUN apt-get update && apt-get install -y mlton make lua5.3 patch

WORKDIR /work
COPY Makefile .
COPY src/ src/
COPY pluto/ pluto/
COPY util/ util/
COPY lib/ lib/
COPY thirdparty/ thirdparty/

RUN mkdir -p bin && make
WORKDIR /work/thirdparty
RUN make install

FROM ubuntu:23.04
ENV PATH="/opt/lunarml/bin:$PATH"
COPY --from=0 /work/bin /opt/lunarml/lib/lunarml/bin
COPY --from=0 /work/lib /opt/lunarml/lib
COPY --chmod=755 package/docker/lunarml /opt/lunarml/bin/
COPY LICENSE /opt/lunarml/
