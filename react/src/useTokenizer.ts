import { useReducer } from 'react';
import { reducer } from './reducer';
import { TokenType } from './token';

type Props = {
  input: string;
};

const useTokenizer = ({ input }: Props) => {
  const [state, dispatch] = useReducer(reducer, { buffer: input + '\0' });

  return [state.token, dispatch, state.token?.type !== TokenType.Eof] as const;
};

export default useTokenizer;
