import 'package:mason_logger/mason_logger.dart';
import 'package:monkeydart/monkeydart.dart';

void main(List<String> arguments) async {
  final logger = Logger();
  switch (arguments) {
    case _ when arguments.isEmpty:
      print('Usage: dart run bin/repl.dart < -RLPL | -RPPL | -REPL >');
      return;
    case _ when arguments[0].toUpperCase() == '-RLPL':
      final rlpl = RLPL(logger: logger);
      await rlpl.run();
      return;
    case _ when arguments[0].toUpperCase() == '-RPPL':
      final rppl = RPPL(logger: logger);
      await rppl.run();
      return;
    case _ when arguments[0].toUpperCase() == '-REPL':
      final repl = REPL(logger: logger);
      await repl.run();
      return;
    default:
      print('Usage: dart run bin/repl.dart < -RLPL | -RPPL | -REPL >');
      return;
  }
}
