(ns monkey.lexer
  (:require [monkey.token :refer [token new-token] :as tok]))

(defn letter? [ch]
  (or (Character/isLetter ch) (= ch \_)))

(defn digit? [ch]
  (Character/isDigit ch))

(defn whitespace? [ch]
  (Character/isWhitespace ch))

(defprotocol PLexer
  (read-char [l])
  (peek-char [l])
  (skip-whitespace [l])
  (read-identifier [l])
  (read-number [l])
  (read-two-letter-op [l])
  (assoc-token [l ch])
  (next-token [l]))

(defrecord Lexer [input position read-position ch]
  PLexer

  (read-char [l]
    (-> (if (>= (:read-position l) (count (:input l)))
          (assoc l :ch (char 0))
          (assoc l :ch (nth (:input l) (:read-position l))))
        (assoc :position (:read-position l))
        (update :read-position inc))) 

  (peek-char [l]
    (if (>= (:read-position l) (count (:input l)))
      (char 0)
      (nth (:input l) (:read-position l))))

  (skip-whitespace [l]
    (if (whitespace? (:ch l))
      (skip-whitespace (read-char l))
      l))

  (read-identifier [l]
    (if (letter? (:ch l))
      (-> (read-char l)
          (update :start #(or % (:position l)))
          read-identifier)
      l))
  
  (read-number [l]
    (if (digit? (:ch l))
      (-> (read-char l)
          (update :start #(or % (:position l)))
          read-number)
      l))

  (read-two-letter-op [l]
    (-> (assoc l :start (:position l))
        read-char
        read-char))

  (assoc-token [l ch]
    (let [tok (case ch
                \= (if (= (peek-char l) \=)
                     :equal
                     (new-token (:assign token) (str ch)))
                \+ (new-token (:plus token) (str ch))
                \- (new-token (:minus token) (str ch))
                \! (if (= (peek-char l) \=)
                     :not-equal
                     (new-token (:bang token) (str ch)))
                \/ (new-token (:slash token) (str ch))
                \* (new-token (:asterisk token) (str ch))
                \< (new-token (:lt token) (str ch))
                \> (new-token (:gt token) (str ch))
                \; (new-token (:semicolon token) (str ch))
                \, (new-token (:comma token) (str ch))
                \( (new-token (:lparen token) (str ch))
                \) (new-token (:rparen token) (str ch))
                \{ (new-token (:lbrace token) (str ch))
                \} (new-token (:rbrace token) (str ch))
                (cond
                  (letter? ch) :letter
                  (digit? ch) :digit
                  (= ch (char 0)) (tok/->Token (:eof token) "")
                  :else (new-token (:illegal token) (str ch))))]
      (case tok            
        :letter
        (let [l (read-identifier (dissoc l :start))
              literal (subs (:input l) (:start l) (:position l))
              type (or ((keyword literal) token) (:ident token))]
          (assoc l :token (new-token type literal)))

        :digit
        (let [l (read-number (dissoc l :start))
              literal (subs (:input l) (:start l) (:position l))]
          (assoc l :token (new-token (:int token) literal)))

        :equal
        (let [l (read-two-letter-op l)
              literal (subs (:input l) (:start l) (:position l))]
          (assoc l :token (new-token (:eq token) literal)))

        :not-equal
        (let [l (read-two-letter-op l)
              literal (subs (:input l) (:start l) (:position l))]
          (assoc l :token (new-token (:not-eq token) literal)))

        (-> (read-char l)
            (assoc :token tok)))))
  
  (next-token [l]
    (as-> l $
      (skip-whitespace $)
      (assoc-token $ (:ch $)))))

(defn lexer [input]
  {:pre [string?]}
  (read-char (->Lexer input -1 0 nil)))
