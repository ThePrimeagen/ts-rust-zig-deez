import 'dart:io';
import 'package:mason_logger/mason_logger.dart';

void main(List<String> arguments) async {
  final logger = Logger();

  if (arguments.isEmpty) {
    logger.info('Usage: dart run bin/mkay.dart <filename>');
    return;
  }

  final fileName = arguments[0];
  final file = File(fileName);

  if (!file.existsSync()) {
    logger.info('File not found: $fileName');
    return;
  }
}
