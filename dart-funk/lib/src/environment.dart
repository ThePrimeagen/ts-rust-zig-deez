// ignore_for_file: prefer_constructors_over_static_methods

import 'package:monkeydart/monkeydart.dart';

class Environment {
  Environment([Map<String, Thing>? store, Environment? outer])
      : _store = store ?? <String, Thing>{},
        _outer = outer;

  static Environment create({Environment? outer}) {
    return Environment({}, outer);
  }

  static Environment createClosure(Environment outer) {
    return Environment.create(outer: outer);
  }

  (Thing, bool) get(String name) {
    final retVal = _store[name] ?? const NullThing();
    if (retVal is NullThing && _outer != null) {
      return _outer!.get(name);
    }

    return (retVal, retVal is! NullThing);
  }

  Thing set(String name, Thing value) {
    _store[name] = value;
    return value;
  }

  final Environment? _outer;
  final Map<String, Thing> _store;
}
