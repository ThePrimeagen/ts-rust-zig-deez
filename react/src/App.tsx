import Lexer from './Lexer';

const input = `2 + 2`;

const App = () => {
  return (
    <>
      <div>{input}</div>
      <Lexer input={input} />;
    </>
  );
};

export default App;
