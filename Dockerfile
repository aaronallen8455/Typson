FROM flipstone/stack:v2-2.1.0.3-rc

RUN apt-get update &&\
    apt-get install -y --no-install-recommends libpq5 libpq-dev &&\
    apt-get clean

