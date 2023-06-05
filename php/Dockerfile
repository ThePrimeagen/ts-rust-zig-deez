FROM php:8.2-cli AS test

COPY --from=mlocati/php-extension-installer:2.1.28 /usr/bin/install-php-extensions /usr/local/bin/

RUN install-php-extensions @composer

COPY . .

RUN composer install --no-interaction

CMD [ "composer", "test", "tests" ]