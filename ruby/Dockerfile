FROM ruby:3.2

WORKDIR /deez-app

COPY ./src ./src
COPY ./test ./test
COPY ./Gemfile .
COPY ./Gemfile.lock .

RUN bundle install

CMD ["rspec", "test/lexer.rb"]