{
module Bensalem.Markup.BensalemML.Lexer
  ( lexToken
  , lexTokens
  , alexScan )
where


import Control.Monad.State (MonadState(..))
import Bensalem.Markup.BensalemML.LexerActions
import Bensalem.Markup.BensalemML.ParserDefs
import Bensalem.Markup.BensalemML.Token
import qualified Data.Text as T
}

$specialChar = [ \[ \] \{ \} = \ \n @ ]
$plainText = $printable # $specialChar
$whitespace = [\  \n]

$identish = $printable # [ \[ \] \{ \} \  \n ]
$inlineStarter = $identish # [@ \; \# &]

@blanks = (\n | \ )*
@indent = \n @blanks

tokens :-

<0> {
  -- we are a little strict here. if there is no white space we recover in
  -- lexToken.
  (\n | \ )+ { textTok Blanks `thenCode` plainTextBeginLine}
}

<plainTextBeginLine> {
  $plainText+ { textTok PlainText `thenCode` plainTextMidLine }

  "{" { doStartBraceGroup `thenCode` plainTextMidLine }
  "}" { doEndBraceGroup `thenCode` plainTextMidLine }
  "[" { plainTok StartAttrSet `thenCode` plainTextMidLine }
  "]" { plainTok EndAttrSet `thenCode` plainTextMidLine }
  "=" { plainTok Equals `thenCode` plainTextMidLine }

  "@@" { plainTok LiteralAt `thenCode` plainTextMidLine }
  "@;;" .* { doLineComment `thenCode` plainTextMidLine }
  "@&" $identish+ { doLayoutTag `thenCode` plainTextMidLine }
  "@" $inlineStarter $identish* { doInlineTag `thenCode` plainTextMidLine }
  "@" "#"+ $identish+ { doLevelTag `thenCode` plainTextMidLine }
}

<plainTextMidLine> {
  $plainText+ { textTok PlainText }

  \ + { \toklen spn _ -> pure $ Located spn $ LineSpace toklen }
  @indent { doBlanks `thenCode` plainTextBeginLine }

  "{" { doStartBraceGroup }
  "}" { doEndBraceGroup }
  "[" { plainTok StartAttrSet }
  "]" { plainTok EndAttrSet }
  "=" { plainTok Equals }

  "@@" { plainTok LiteralAt `thenCode` plainTextMidLine }
  "@;;" .* { doLineComment }
  "@&" $identish+ { doLayoutTag `thenCode` plainTextMidLine }
  "@" "#"+ $identish+ { doLevelTag `thenCode` plainTextMidLine }
  "@" $inlineStarter $identish* { doInlineTag `thenCode` plainTextMidLine }
}

{
-- | Parse a single token from the input stream
lexToken :: Parser (Located Token)
lexToken = do
  parsestate@(ParseState sc inp toks _ _ _ _) <- get
  case toks of
    (t:toks') -> put (parsestate { parseStatePendingTokens = toks' }) *> pure t
    _ -> case alexScan inp sc of
            AlexEOF -> doEOF
            AlexError ai
              | sc == 0 -> do
                  put $ parsestate {parseStateStartCode = plainTextBeginLine}
                  lexToken
              | otherwise -> do
                  let nm = alexInputSrcName ai
                  let pos = alexInputSrcPos ai
                  throwLexError (SrcSpan nm pos pos) NoToken
            AlexSkip inp' _ -> setInput inp' *> lexToken
            AlexToken inp' toklen act -> do
              let initPos = alexInputSrcPos inp
              let finalPos = alexInputSrcPos inp'
              let name = alexInputSrcName inp
              let posSpan = SrcSpan name initPos finalPos
              setInput inp' *> act toklen posSpan (T.take toklen $ alexInputText inp)

-- | Tokenize the entire input stream
lexTokens :: Parser [Located Token]
lexTokens = go id
  where
    go acc = do
      tok <- lexToken
      case locatedVal tok of
        TokenEOF -> pure $ acc []
        _ -> go (acc . (tok :))
}
