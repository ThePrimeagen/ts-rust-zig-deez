import { useEffect, useState } from 'react';
import useTokenizer from './useTokenizer';
import { Token } from './token';

type Props = {
  input: string;
};

const useLexer = ({ input }: Props) => {
  const [token, popNext, hasNext] = useTokenizer({ input: input + '\0' });
  const [tokens, setTokens] = useState<Token[]>([]);

  useEffect(() => {
    if (!hasNext) return;

    setTokens(tokens => (token ? [...tokens, token] : [...tokens]));
    popNext();
  }, [token, hasNext, popNext]);

  return tokens;
};

export default useLexer;
