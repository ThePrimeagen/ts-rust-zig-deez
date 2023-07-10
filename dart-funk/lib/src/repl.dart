import 'dart:async';
import 'dart:convert';
import 'dart:io';

import 'package:mason_logger/mason_logger.dart';
import 'package:monkeydart/monkeydart.dart';

// coverage:ignore-start

const monkeyFace = r'''
            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
''';

// ignore: camel_case_types
sealed class R_E_P_L {
  R_E_P_L({
    Logger? logger,
    String? prompt,
    Stdin? stdIn,
  })  : _logger = logger ?? Logger(),
        _prompt = prompt ?? '>> ',
        _stdin = stdIn ?? stdin;

  Future<void> run() async {
    _logger
      ..info(monkeyFace)
      ..info('Welcome to the Monkey Programming Language')
      ..info('Feel free to type in commands');

    try {
      // keep reading the stdin until you receive an 'exit' command
      while (true) {
        stdout.write(_prompt);
        final line = _stdin.readLineSync(encoding: utf8);
        await evalLine(line);
      }
    } catch (e) {
      _logger.alert('Error: $e');
    }
  }

  Future<void> evalLine(String? line);

  late final Logger _logger;
  late final String _prompt;
  late final Stdin _stdin;
}

class RLPL extends R_E_P_L {
  RLPL({
    super.logger,
    super.prompt,
    super.stdIn,
  });

  @override
  Future<void> run() {
    logger.alert('This implementation returns the list of lexed tokens');
    return super.run();
  }

  @override
  Future<void> evalLine(String? line) async {
    switch (line?.trim().toLowerCase()) {
      case null:
        break;
      case 'exit':
        _logger.warn('Bye!');
        exit(0);
      default:
        final tokens = tokenGenerator(line!);
        for (final token in tokens) {
          _logger.info(token.toString());
        }
    }
  }
}

class RPPL extends R_E_P_L {
  RPPL({
    super.logger,
    super.prompt,
    super.stdIn,
  });

  @override
  Future<void> run() {
    logger.alert('This implementation returns the list of parsed statements');
    return super.run();
  }

  @override
  Future<void> evalLine(String? line) async {
    switch (line?.trim().toLowerCase()) {
      case null:
        break;
      case 'exit':
        _logger.warn('Bye!');
        exit(0);
      default:
        final parser = Parser(tokenGenerator(line!));
        final program = parse(parser);
        if (program.errors.isNotEmpty) {
          _logger.alert('Woops! We ran into some monkey business here!');
          for (final error in program.errors) {
            _logger.alert(error.toString());
          }
          return;
        }
        _logger.info('Program has ${program.statements.length} statements');
        for (final statement in program.statements) {
          _logger.info('\t$statement');
        }
    }
  }
}

class REPL extends R_E_P_L {
  REPL({
    super.logger,
    super.prompt,
    super.stdIn,
  });

  final env = Environment.create();

  @override
  Future<void> evalLine(String? line) async {
    switch (line?.trim().toLowerCase()) {
      case null:
        break;
      case 'exit':
        _logger.warn('Bye!');
        exit(0);
      default:
        final parser = Parser(tokenGenerator(line!));
        final program = parse(parser);
        if (program.errors.isNotEmpty) {
          _logger.alert('Woops! We ran into some monkey business here!');
          for (final error in program.errors) {
            _logger.alert(error.toString());
          }
        }
        final evaluation = eval(program, env);
        // if (evaluation is! Let) {
        _logger.info(evaluation.inspect());
      // }
      // if (evaluation is! NullThing) {
      //   _logger.info(evaluation.inspect());
      // }
    }
  }
}

// coverage:ignore-end
