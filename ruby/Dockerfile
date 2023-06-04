FROM ruby:3.2
RUN apt-get update
WORKDIR /deez
COPY Gemfile* .
RUN bundle install
COPY . .
CMD make clean && \
    make ready
