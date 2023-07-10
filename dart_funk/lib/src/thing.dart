// ignore_for_file: avoid_positional_boolean_parameters

import 'package:equatable/equatable.dart';
import 'package:monkeydart/monkeydart.dart';

enum ThingType {
  integer,
  boolean,
  builtin,
  retval,
  function,
  string,
  array,
  quote,
  error,
  _let,
}

typedef BuiltinFunction = Thing Function(List<Thing> args);

sealed class Thing extends Equatable {
  const Thing(this.type);

  final ThingType type;
  String inspect();

  @override
  List<Object?> get props => [type, inspect()];
}

class Quote extends Thing {
  const Quote(this.node) : super(ThingType.quote);

  final Node node;

  @override
  String inspect() => 'QUOTE($node)';
}

class Hash extends Thing {
  const Hash(this.pairs) : super(ThingType._let);

  final Map<Thing, Thing> pairs;

  @override
  String inspect() {
    final buffer = StringBuffer()..write('{');
    for (final key in pairs.keys) {
      buffer.write('${key.inspect()}: ${pairs[key]!.inspect()}, ');
    }
    buffer.write('}');
    return buffer.toString();
  }
}

class Array extends Thing {
  const Array(this.elements) : super(ThingType.array);

  final List<Thing> elements;

  @override
  String inspect() {
    final buffer = StringBuffer()..write('[');
    for (final element in elements) {
      if (element == elements.last) {
        buffer.write(element.inspect());
        break;
      }
      buffer.write('${element.inspect()}, ');
    }
    buffer.write(']');
    return buffer.toString();
  }
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

class BuiltIn extends Thing {
  const BuiltIn(this.fn) : super(ThingType.builtin);

  final BuiltinFunction fn;

  @override
  String inspect() => 'builtin function';
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

class Stringy extends Thing {
  const Stringy(this.value) : super(ThingType.string);

  final String value;

  @override
  String inspect() => value;
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

extension MapThingThingExt on Map<Thing, Thing> {
  Thing? get(Thing key) {
    return switch (key.type) {
      ThingType.integer || ThingType.boolean || ThingType.string => this[key],
      _ => Error('unusable as hash key: ${key.type}'),
    };
  }
}
