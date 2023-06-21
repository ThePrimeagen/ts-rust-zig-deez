// ignore: lines_longer_than_80_chars
// ignore_for_file: constant_identifier_names, avoid_positional_boolean_parameters

import 'dart:io';

import 'package:monkeydart/monkeydart.dart';

const TRUE = Boolean(true);
const FALSE = Boolean(false);
const NULL = NullThing();

final builtins = <String, BuiltinFunction>{
  'len': (args) => const BuiltIn(binLen),
  'car': (args) => const BuiltIn(binFirst),
  'first': (args) => const BuiltIn(binFirst),
  'last': (args) => const BuiltIn(binLast),
  'rest': (args) => const BuiltIn(binCdr),
  'cdr': (args) => const BuiltIn(binCdr),
  'push': (args) => const BuiltIn(binPush),
  'puts': (args) => const BuiltIn(binPuts),
};

Thing push(Array array, Thing thing) {
  return Array([...array.elements, thing]);
}

Thing binPuts(List<Thing> args) {
  for (final arg in args) {
    stdout.writeln(arg.inspect());
  }
  return const NullThing();
}

Thing binPush(List<Thing> args) {
  return switch (args) {
    _ when args.length != 2 =>
      newError('wrong number of monkeys. got=${args.length}, want=2'),
    _ when args[0] is Array => push(args[0] as Array, args[1]),
    _ => newError('invalid argument type; push accepts arrays and new element')
  };
}

Thing cdr(Array array) {
  if (array.elements.isEmpty) {
    return const NullThing();
  }
  return Array(array.elements.sublist(1));
}

Thing binCdr(List<Thing> args) {
  return switch (args) {
    _ when args.length != 1 =>
      newError('wrong number of monkeys. got=${args.length}, want=1'),
    _ when args[0] is Array => cdr(args[0] as Array),
    _ => newError('invalid argument type; cdr accepts arrays')
  };
}

Thing binFirst(List<Thing> args) {
  return switch (args) {
    _ when args.length != 1 =>
      newError('wrong number of monkeys. got=${args.length}, want=1'),
    _ when args[0] is Array => (args[0] as Array).elements.isNotEmpty
        ? (args[0] as Array).elements[0]
        : newError('array is empty'),
    _ => newError('invalid argument type; first accepts arrays')
  };
}

Thing binLast(List<Thing> args) {
  return switch (args) {
    _ when args.length != 1 =>
      newError('wrong number of monkeys. got=${args.length}, want=1'),
    _ when args[0] is Array => (args[0] as Array).elements.isNotEmpty
        ? (args[0] as Array).elements[(args[0] as Array).elements.length - 1]
        : newError('array is empty'),
    _ => newError('invalid argument type; last accepts arrays')
  };
}

Thing binLen(List<Thing> args) {
  return switch (args) {
    _ when args.length != 1 =>
      newError('wrong number of monkeys. got=${args.length}, want=1'),
    _ when args[0] is Stringy => Integer((args[0] as Stringy).value.length),
    _ when args[0] is Array => Integer((args[0] as Array).elements.length),
    _ => newError('invalid argument type; len accepts strings and arrays')
  };
}

Thing eval(Node node, Environment env) {
  switch (node.runtimeType) {
    case Program:
      return evalProgram(node as Program, env);
    case ExpressionStatement:
      return eval((node as ExpressionStatement).expression, env);
    case StringLiteral:
      return Stringy((node as StringLiteral).value);
    case IntegerLiteral:
      return Integer((node as IntegerLiteral).value);
    case BooleanLiteral:
      return nativeBoolToBoolean((node as BooleanLiteral).value);
    case HashLiteral:
      return evalHashLiteral(node as HashLiteral, env);
    case ArrayLiteral:
      final elements = evalExpressions((node as ArrayLiteral).elements, env);
      if (elements.length == 1 && isError(elements[0])) {
        return elements[0];
      }
      return Array(elements);
    case PrefixExpression:
      final n = node as PrefixExpression;
      final right = eval(n.right, env);
      if (isError(right)) {
        return right;
      }
      return evalPrefixExpression(n.operator, right);
    case Identifier:
      return evalIdentifier(node as Identifier, env);
    case IndexExpression:
      final left = eval((node as IndexExpression).left, env);
      if (isError(left)) {
        return left;
      }
      final index = eval(node.index, env);
      if (isError(index)) {
        return index;
      }
      return evalIndexExpression(left, index);
    case InfixExpression:
      final n = node as InfixExpression;
      final left = eval(n.left, env);
      if (isError(left)) {
        return left;
      }
      final right = eval(n.right, env);
      if (isError(right)) {
        return right;
      }
      return evalInfixExpression(n.operator, left, right);
    case BlockStatement:
      return evalBlockStatement(node as BlockStatement, env);
    case IfExpression:
      return evalIfExpression(node as IfExpression, env);
    case ReturnStatement:
      final retVal = eval((node as ReturnStatement).returnValue, env);
      if (isError(retVal)) {
        return retVal;
      }
      return RetVal(retVal);
    case LetStatement:
      final val = eval((node as LetStatement).value, env);
      if (isError(val)) {
        return val;
      }
      env.set(node.name.value, val);
      return Let(val);
    case FunctionLiteral:
      final params = (node as FunctionLiteral).parameters;
      final body = node.body;
      return Fun(params, body, env);
    case CallExpression:
      final function = eval((node as CallExpression).function, env);
      if (isError(function)) {
        return function;
      }
      final args = evalExpressions(node.arguments, env);
      if (args.length == 1 && isError(args.first)) {
        return args.first;
      }
      return applyFunction(function, args);
    default:
      return const NullThing();
  }
}

Thing evalHashLiteral(HashLiteral node, Environment env) {
  final pairs = <Thing, Thing>{};

  for (var i = 0; i < node.pairs.length; i++) {
    final key = node.pairs.keys.elementAt(i);
    final value = node.pairs.values.elementAt(i);
    final keyThing = eval(key, env);
    if (isError(keyThing)) {
      return keyThing;
    }

    final valueThing = eval(value, env);
    if (isError(valueThing)) {
      return valueThing;
    }

    pairs[keyThing] = valueThing;
  }

  return Hash(pairs);
}

Thing evalIndexExpression(Thing left, Thing index) {
  switch (left.runtimeType) {
    case Array:
      return evalArrayIndexExpression(left as Array, index);
    case Hash:
      return evalHashIndexExpression(left as Hash, index);
    default:
      return newError('index operator not supported: ${left.type.name}');
  }
}

Thing evalHashIndexExpression(Hash left, Thing index) {
  final pair = left.pairs[index];
  if (pair == null) {
    return NULL;
  }
  return pair;
}

Thing evalArrayIndexExpression(Array left, Thing index) {
  final i = (index as Integer).value;
  final max = left.elements.length - 1;
  if (i < 0 || i > max) {
    return NULL;
  }
  return left.elements[i];
}

Thing applyFunction(Thing function, List<Thing> args) {
  switch (function.runtimeType) {
    case BuiltIn:
      return (function as BuiltIn).fn(args);
    case Fun:
      final extendedEnv = extendFunctionEnv(function as Fun, args);
      final evaluated = eval(function.body, extendedEnv);
      return unwrapReturnValue(evaluated);
    default:
      return newError('not a function: ${function.type.name}');
  }
}

Thing unwrapReturnValue(Thing evaluated) {
  if (evaluated is RetVal) {
    return evaluated.value;
  }
  return evaluated;
}

Environment extendFunctionEnv(Fun function, List<Thing> args) {
  final env = Environment.createClosure(function.env);
  for (var i = 0; i < function.parameters.length; i++) {
    env.set(function.parameters[i].value, args[i]);
  }
  return env;
}

List<Thing> evalExpressions(List<Expression> arguments, Environment env) {
  final retVal = <Thing>[];
  for (final arg in arguments) {
    final evaluated = eval(arg, env);
    if (isError(evaluated)) {
      return [evaluated];
    }
    retVal.add(evaluated);
  }
  return retVal;
}

Thing evalIdentifier(Identifier node, Environment env) {
  final (val, ok) = env.get(node.value);
  if (ok) {
    return val;
  }
  final bltIn = builtins[node.value];
  if (bltIn != null) {
    return bltIn([]);
  }
  return newError('identifier not found: ${node.value}');
}

bool isTruthy(Thing input) {
  switch (input) {
    case NULL:
      return false;
    case TRUE:
      return true;
    case FALSE:
      return false;
    default:
      return true;
  }
}

Thing evalIfExpression(IfExpression node, Environment env) {
  final condition = eval(node.condition, env);
  if (isError(condition)) {
    return condition;
  }
  if (isTruthy(condition)) {
    return eval(node.consequence, env);
  } else if (node.alternative != null) {
    return eval(node.alternative!, env);
  } else {
    return const NullThing();
  }
}

Thing evalInfixExpression(String operator, Thing left, Thing right) {
  if (left.type == ThingType.integer && right.type == ThingType.integer) {
    return evalIntegerInfixExpression(operator, left, right);
  }
  if (left.type == ThingType.boolean && right.type == ThingType.boolean) {
    return evalBooleanInfixExpression(operator, left, right);
  }
  if (left.type == ThingType.string && right.type == ThingType.string) {
    return evalStringInfixExpression(operator, left, right);
  }
  if (left.type != right.type) {
    return newError(
      'type mismatch: ${left.type.name.toUpperCase()} '
      '$operator ${right.type.name.toUpperCase()}',
    );
  }
  return newError('unknown operator: ${left.type.name.toUpperCase()} '
      '$operator ${right.type.name.toUpperCase()}');
}

Thing evalStringInfixExpression(String operator, Thing left, Thing right) {
  if (operator != '+') {
    return newError('unknown operator: '
        '${left.type.name.toUpperCase()} $operator '
        '${right.type.name.toUpperCase()}');
  }
  final leftVal = (left as Stringy).value;
  final rightVal = (right as Stringy).value;
  return Stringy(leftVal + rightVal);
}

Thing evalBooleanInfixExpression(String operator, Thing left, Thing right) {
  final leftVal = (left as Boolean).value;
  final rightVal = (right as Boolean).value;
  switch (operator) {
    case '==':
      return nativeBoolToBoolean(leftVal == rightVal);
    case '!=':
      return nativeBoolToBoolean(leftVal != rightVal);
    default:
      return newError('unknown operator: '
          '${left.type.name.toUpperCase()} $operator '
          '${right.type.name.toUpperCase()}');
  }
}

Thing evalIntegerInfixExpression(String operator, Thing left, Thing right) {
  final leftVal = (left as Integer).value;
  final rightVal = (right as Integer).value;
  switch (operator) {
    case '+':
      return Integer(leftVal + rightVal);
    case '-':
      return Integer(leftVal - rightVal);
    case '*':
      return Integer(leftVal * rightVal);
    case '/':
      return Integer(leftVal ~/ rightVal);
    case '<':
      return nativeBoolToBoolean(leftVal < rightVal);
    case '>':
      return nativeBoolToBoolean(leftVal > rightVal);
    case '==':
      return nativeBoolToBoolean(leftVal == rightVal);
    case '!=':
      return nativeBoolToBoolean(leftVal != rightVal);
    default:
      return newError('unknown operator: ${left.type.name.toUpperCase()} '
          '$operator ${right.type.name.toUpperCase()}');
  }
}

Thing evalPrefixExpression(String operator, Thing right) {
  switch (operator) {
    case '!':
      return evalBangOperatorExpression(right);
    case '-':
      return evalMinusPrefixOperatorExpression(right);
    default:
      return newError('unknown operator: '
          '$operator${right.type.name.toUpperCase()}');
  }
}

Thing evalMinusPrefixOperatorExpression(Thing right) {
  if (right is! Integer) {
    return newError('unknown operator: -${right.type.name.toUpperCase()}');
  }
  final value = right.value;
  return Integer(-value);
}

Thing evalBangOperatorExpression(Thing right) {
  switch (right) {
    case TRUE:
      return FALSE;
    case FALSE:
      return TRUE;
    case NULL:
      return TRUE;
    default:
      return FALSE;
  }
}

Thing evalBlockStatement(BlockStatement node, Environment env) {
  final statements = node.statements;
  Thing retVal = const NullThing();
  for (final statement in statements) {
    retVal = eval(statement, env);
    if (retVal is! NullThing) {
      if (retVal.type == ThingType.retval || retVal.type == ThingType.error) {
        return retVal;
      }
    }
  }
  return retVal;
}

Thing evalProgram(Program program, Environment env) {
  final statements = program.statements;
  Thing retVal = const NullThing();
  for (final statement in statements) {
    retVal = eval(statement, env);

    switch (retVal.type) {
      case ThingType.retval:
        return (retVal as RetVal).value;
      case ThingType.error:
        return retVal;
      case _:
        break;
    }
  }
  return retVal;
}

Boolean nativeBoolToBoolean(bool input) {
  return input ? TRUE : FALSE;
}

Error newError(String message) {
  return Error(message);
}

bool isError(Thing input) {
  if (input is! NullThing) {
    return input.type == ThingType.error;
  }
  return false;
}
