FROM public.ecr.aws/z2l4u8l9/cardano-private-testnet-base:1.35.5

RUN apk add --no-cache yq

COPY ./entrypoint.sh .
COPY ./fund-wallet.sh .

STOPSIGNAL SIGTERM

# We forward the socket to the host machine for the tests to query manager, etc.
ENTRYPOINT ["./entrypoint.sh"]
