FROM composer:2.5.7 as test

COPY . .
RUN composer install --no-interaction

CMD [ "composer", "test", "tests" ]
