type t;

let init: string => t;
let next_token: t => (t, Token.t);
