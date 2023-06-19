// ignore_for_file: avoid_positional_boolean_parameters

import 'package:equatable/equatable.dart';
import 'package:monkeydart/monkeydart.dart';

enum ThingType {
  integer,
  boolean,
  retval,
  function,
  error,
  _let,
}

sealed class Thing extends Equatable {
  const Thing(this.type);

  final ThingType type;
  String inspect();

  @override
  List<Object?> get props => [type, inspect()];
}

class Integer extends Thing {
  const Integer(this.value) : super(ThingType.integer);

  final int value;

  @override
  String inspect() => value.toString();
}

class Boolean extends Thing {
  const Boolean(this.value) : super(ThingType.boolean);

  final bool value;

  @override
  String inspect() => value.toString();
}

class RetVal extends Thing {
  const RetVal(this.value) : super(ThingType.retval);

  final Thing value;

  @override
  String inspect() => value.inspect();
}

class Fun extends Thing {
  const Fun(this.parameters, this.body, this.env) : super(ThingType.function);

  final List<Identifier> parameters;
  final BlockStatement body;
  final Environment env;

  @override
  String inspect() {
    final buffer = StringBuffer()
      ..write('fn(')
      ..writeAll(parameters, ', ')
      ..write(') {\n')
      ..write(body.toString())
      ..write('\n}');
    return buffer.toString();
  }
}

class Let extends Thing {
  const Let(this.value) : super(ThingType._let);

  final Thing value;

  @override
  String inspect() => value.inspect();
}

class Error extends Thing {
  const Error(this.message) : super(ThingType.error);

  final String message;

  @override
  String inspect() => 'ERROR: $message';
}

class NullThing extends Thing {
  const NullThing() : super(ThingType.integer);

  @override
  String inspect() => 'not my 🙈-🙉-🙊, not my circus';
}
