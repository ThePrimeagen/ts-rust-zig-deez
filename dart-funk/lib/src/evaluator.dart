// ignore_for_file: constant_identifier_names, avoid_positional_boolean_parameters

import 'package:monkeydart/monkeydart.dart';

const TRUE = Boolean(true);
const FALSE = Boolean(false);
const NULL = NullThing();

Thing eval(Node node) {
  switch (node.runtimeType) {
    case Program:
      return evalProgram(node as Program);
    // return evalStatements((node as Program).statements);
    case ExpressionStatement:
      return eval((node as ExpressionStatement).expression);
    case IntegerLiteral:
      return Integer((node as IntegerLiteral).value);
    case BooleanLiteral:
      return nativeBoolToBoolean((node as BooleanLiteral).value);
    case PrefixExpression:
      final n = node as PrefixExpression;
      final right = eval(n.right);
      return evalPrefixExpression(n.operator, right);
    case InfixExpression:
      final n = node as InfixExpression;
      final left = eval(n.left);
      final right = eval(n.right);
      return evalInfixExpression(n.operator, left, right);
    case BlockStatement:
      return evalBlockStatement(node as BlockStatement);
    case IfExpression:
      return evalIfExpression(node as IfExpression);
    case ReturnStatement:
      final retVal = eval((node as ReturnStatement).returnValue);
      return RetVal(retVal);
    // case LetStatement:
    //   return evalLetStatement(node as LetStatement);
    // case FunctionLiteral:
    //   return evalFunctionLiteral(node as FunctionLiteral);
    // case CallExpression:
    //   return evalCallExpression(node as CallExpression);
    default:
      return const NullThing();
  }
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

Thing evalIfExpression(IfExpression node) {
  final condition = eval(node.condition);
  if (isTruthy(condition)) {
    return eval(node.consequence);
  } else if (node.alternative != null) {
    return eval(node.alternative!);
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
  return const NullThing();
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
      return const NullThing();
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
      return const NullThing();
  }
}

Thing evalPrefixExpression(String operator, Thing right) {
  switch (operator) {
    case '!':
      return evalBangOperatorExpression(right);
    case '-':
      return evalMinusPrefixOperatorExpression(right);
    default:
      return const NullThing();
  }
}

Thing evalMinusPrefixOperatorExpression(Thing right) {
  if (right is! Integer) {
    return const NullThing();
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

Thing evalBlockStatement(BlockStatement node) {
  final statements = node.statements;
  Thing retVal = const NullThing();
  for (final statement in statements) {
    retVal = eval(statement);
    if (retVal is! NullThing && retVal is RetVal) {
      return retVal;
    }
  }
  return retVal;
}

Thing evalProgram(Program program) {
  final statements = program.statements;
  Thing retVal = const NullThing();
  for (final statement in statements) {
    retVal = eval(statement);
    if (retVal is RetVal) {
      return retVal.value;
    }
  }
  return retVal;
}

Boolean nativeBoolToBoolean(bool input) {
  return input ? TRUE : FALSE;
}
