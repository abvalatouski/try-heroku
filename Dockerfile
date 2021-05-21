ARG resolver=lts-16.20
FROM fpco/stack-build:${resolver} AS build
WORKDIR /opt/try-heroku
COPY stack.yaml package.yaml ./
RUN stack build --only-dependencies
COPY ./app/ ./app/
RUN stack install

FROM ubuntu:18.04 AS release
RUN apt-get update \
 && apt-get install -y \
    postgresql-client \
    libpq-dev
WORKDIR /opt/try-heroku
COPY ./style.css ./setup.sql ./
COPY --from=build /root/.local/bin/todo-list ./
CMD ["./todo-list"]
