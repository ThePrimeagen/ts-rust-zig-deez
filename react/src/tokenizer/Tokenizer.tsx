import { ForwardedRef, forwardRef, useImperativeHandle } from 'react';

import { Token } from 'utils/token';

import useTokenizer from './useTokenizer';

type Props = {
  input: string;
};

export type TokenizerRef = {
  token?: Token;
  hasNext: boolean;
  popNext: () => void;
};

const Tokenizer = forwardRef(({ input }: Props, ref: ForwardedRef<TokenizerRef>) => {
  const [token, popNext, hasNext] = useTokenizer({ input: input + '\0' });

  useImperativeHandle(
    ref,
    () => ({
      token,
      hasNext,
      popNext,
    }),
    [token, popNext, hasNext]
  );

  return null;
});

export default Tokenizer;
