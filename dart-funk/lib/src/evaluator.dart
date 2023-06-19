// ignore: lines_longer_than_80_chars
// ignore_for_file: constant_identifier_names, avoid_positional_boolean_parameters

import 'package:monkeydart/monkeydart.dart';

const TRUE = Boolean(true);
const FALSE = Boolean(false);
const NULL = NullThing();

Thing eval(Node node, Environment env) {
  switch (node.runtimeType) {
    case Program:
      return evalProgram(node as Program, env);
    case ExpressionStatement:
      return eval((node as ExpressionStatement).expression, env);
    case IntegerLiteral:
      return Integer((node as IntegerLiteral).value);
    case BooleanLiteral:
      return nativeBoolToBoolean((node as BooleanLiteral).value);
    case PrefixExpression:
      final n = node as PrefixExpression;
      final right = eval(n.right, env);
      if (isError(right)) {
        return right;
      }
      return evalPrefixExpression(n.operator, right);
    case Identifier:
      return evalIdentifier(node as Identifier, env);
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

Thing applyFunction(Thing function, List<Thing> args) {
  if (function is! Fun) {
    return newError('not a function: ${function.type.name}');
  }
  final extendedEnv = extendFunctionEnv(function as Fun, args);
  final evaluated = eval(function.body, extendedEnv);
  return unwrapReturnValue(evaluated);
  // switch (function.runtimeType) {
  //   case Fun:
  //     final function = Fun();

  //   // case Builtin:
  //   //   final fn = function as Builtin;
  //   //   return fn.fn(args);
  //   default:
  //     return newError('not a function: ${function.type.name}');
  // }
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
  if (left.type != right.type) {
    return newError(
      'type mismatch: ${left.type.name.toUpperCase()} '
      '$operator ${right.type.name.toUpperCase()}',
    );
  }
  return newError('unknown operator: ${left.type.name.toUpperCase()} '
      '$operator ${right.type.name.toUpperCase()}');
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
