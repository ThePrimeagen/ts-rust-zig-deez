import { ForwardedRef, forwardRef, useImperativeHandle, useReducer } from 'react';
import { reducer } from './reducer';
import { Token } from './token';

type Props = {
  input: string;
};

export type TokenizerRef = {
  token?: Token;
  hasNext: () => boolean;
  pop: () => void;
};

const Tokenizer = forwardRef(({ input }: Props, ref: ForwardedRef<TokenizerRef>) => {
  const [state, dispatch] = useReducer(reducer, { buffer: input + '\0' });

  useImperativeHandle(
    ref,
    () => ({
      token: state.token,
      hasNext: () => {
        return !!state.buffer;
      },
      pop: () => {
        dispatch();
      },
    }),
    [state]
  );

  return null;
});

export default Tokenizer;
