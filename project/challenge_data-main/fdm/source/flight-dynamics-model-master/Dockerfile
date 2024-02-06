ARG REGISTRY=git.isis.vanderbilt.edu:5050
ARG REPO=/swri/
ARG TAG=master

FROM ${REGISTRY}${REPO}fdm-builder:${TAG} as builder

WORKDIR /source

COPY . .

RUN autoreconf -if
RUN ./configure
RUN make

FROM ${REGISTRY}${REPO}fdm-builder:${TAG} as runner

WORKDIR /source
RUN --mount=type=bind,from=builder,source=/source,target=/source make install

WORKDIR /app/run
VOLUME ["/app/run"]

ENTRYPOINT ["new_fdm"]
#CMD ["param1", "param2"] # Can add default parameters to new_fdm
