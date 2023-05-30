FROM ruby:3.2

WORKDIR /deez-app

COPY lexer.rb .
COPY test.rb .
COPY Gemfile .
COPY Gemfile.lock .

RUN bundle install

CMD ["rspec", "test.rb"]