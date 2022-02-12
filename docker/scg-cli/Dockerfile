FROM alpine:3.17.2

RUN apk add build-base
RUN apk add cmake

WORKDIR /home

ADD gpmetis/source .
RUN tar -xvf metis-5.1.0.tar.gz
RUN cd metis-5.1.0 && make config cc=gcc && make && make install

RUN cd ..
ADD patoh/source .
RUN tar -xvf patoh-Linux-x86_64.tar.gz
ENV PATH="$PATH:/home/build/Linux-x86_64"

RUN mkdir /data
WORKDIR /data

CMD ["tail", "-f", "/dev/null"]