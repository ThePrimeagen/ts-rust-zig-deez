// ignore_for_file: avoid_positional_boolean_parameters

import 'package:equatable/equatable.dart';

enum ThingType {
  integer,
  boolean,
  retval,
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

class NullThing extends Thing {
  const NullThing() : super(ThingType.integer);

  @override
  String inspect() => 'not my 🙈-🙉-🙊, not my circus';
}
