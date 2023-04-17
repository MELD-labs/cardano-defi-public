# syntax=docker/dockerfile:1.4
FROM contract-builder AS contract-unit-tests
RUN --mount=type=ssh \
    --mount=type=secret,id=gitconfig,readonly \
    --mount=type=cache,id=${TAG}-contract-dist-newstyle,target=/defi/dist-newstyle,sharing=locked \
    --mount=type=cache,target=/root/.cabal \
    build/contract.sh test all

FROM app-builder-lib AS app-unit-tests
RUN --mount=type=ssh \
    --mount=type=secret,id=gitconfig,readonly \
    --mount=type=cache,id=${TAG}-app-dist-newstyle,target=/defi/dist-newstyle,sharing=locked \
    --mount=type=cache,target=/root/.cabal \
    TASTY_PATTERN='!/[Integ]/' build/app.sh test all

FROM scratch
COPY --from=contract-unit-tests this_is_to_make_sure_it_run* .
COPY --from=app-unit-tests this_is_to_make_sure_it_run* .
