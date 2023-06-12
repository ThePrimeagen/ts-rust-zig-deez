# PHP Interpreter

## Requirements

* PHP 8.2
* Composer 2.5.7

## Install

Clone the repository and execute composer to install the dependencies

```bash
composer install
```

## Run tests

You can use the php-cli to run the tests

```bash
vendor/bin/phpunit tests
```

Or you can use docker

```bash
docker build -t php-test --target test .
docker run php-test
```