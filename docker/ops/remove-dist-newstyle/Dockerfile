FROM debian

ARG TAG

RUN --mount=type=cache,id=${TAG}-contract-dist-newstyle,target=/defi/dist-newstyle,sharing=locked \
    rm -rf /defi/dist-newstyle/*

RUN --mount=type=cache,id=${TAG}-app-dist-newstyle,target=/defi/dist-newstyle,sharing=locked \
    rm -rf /defi/dist-newstyle/*
