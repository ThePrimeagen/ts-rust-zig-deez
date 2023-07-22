import { Fragment, useEffect, useRef, useState } from 'react';
import Tokenizer, { TokenizerRef } from './Tokenizer';
import { Token } from './token';

type Props = {
  input: string;
};

const Lexer = ({ input }: Props) => {
  const tokenizerRef = useRef<TokenizerRef>(null);
  const [tokens, setTokens] = useState<Token[]>([]);

  useEffect(() => {
    const tokenizer = tokenizerRef.current;
    if (!tokenizer?.hasNext) return;

    setTokens(tokens => (tokenizer.token ? [...tokens, tokenizer.token] : [...tokens]));
    tokenizer.popNext();
  }, [tokens]);

  return (
    <>
      <Tokenizer ref={tokenizerRef} input={input} />
      {tokens.map((token, i) => (
        <Fragment key={i}>
          ({token.type}: {token.literal})
        </Fragment>
      ))}
    </>
  );
};

export default Lexer;
