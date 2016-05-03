FROM fpco/stack-run:lts-5

RUN apt-get update && \
    apt-get install -y \
      curl build-essential

RUN curl -o /root/librdkafka-0.9.0.99.tar.gz -SL https://github.com/edenhill/librdkafka/archive/0.9.0.99.tar.gz && \
    tar -xzf /root/librdkafka-0.9.0.99.tar.gz -C /root && \
    cd /root/librdkafka-0.9.0.99 && ./configure && make && make install && cd / && \
    tar czf librdkafka-0.9.0.99.tar.gz usr/local/include/librdkafka usr/local/lib/librdkafka*