type Fn<TArg = any, TReturn = any> = (arg: TArg) => TReturn;

export const pipe = <TArg, TFirstFn extends Fn<TArg>, TLastFn extends Fn>(
  arg: TArg,
  firstFn: TFirstFn,
  ...fns: [...Fn[], TLastFn]
): ReturnType<TLastFn> => fns.reduce((arg, fn) => fn(arg), firstFn(arg));
