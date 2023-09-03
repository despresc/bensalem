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

$plainPlainText = $printable # [ \{ \} \  \n @ ]
$afterTagPlainText = $printable # [ \[ \] \{ \} = \ \n @ ]

$identish = $printable # [ \[ \] \{ \} \  \n ]
$inlineStarter = $identish # [@ \; \# &]

$attrKeyish = $printable # [ \[ \] \{ \} \  \n = ]
-- happens to be the same for now
$attrKeyStarter = $attrKeyish

@blanks = (\n | \ )*
@indent = \n @blanks

tokens :-

<0> {
  -- we are a little strict here. if there is no white space we recover in
  -- lexToken.
  (\n | \ )+ { textTok Blanks `thenCode` plainText }
}

<plainText> {
  $plainPlainText+ { textTok PlainText }
  \ + { \toklen spn _ -> pure $ Located spn $ LineSpace toklen }
  @indent { doBlanks }

  "{" { doStartBraceGroup plainText }
  "}" { doEndBraceGroup }

  "@@" { plainTok LiteralAt }
  "@;;" .* { doLineComment }

  "@" $inlineStarter $identish* { doInlineTag `thenCode` afterTag }
  "@&" $identish+ { doLayoutTag plainText `thenCode` afterTag }
  "@" "#"+ $identish+ { doLevelTag plainText `thenCode` afterTag }
}

-- sadly repetitive - I could probably do some kind of recovery trick in
-- lexToken, but I'd rather not right now

<afterTag> {
  "[" { doStartAttrSet plainText `thenCode` attrSet }

  $afterTagPlainText+ { textTok PlainText `thenCode` plainText }
  \ + { (\toklen spn _ -> pure $ Located spn $ LineSpace toklen) `thenCode` plainText }
  @indent { doBlanks `thenCode` plainText }
  "{" { doStartBraceGroup plainText `thenCode` plainText }
  "}" { doEndBraceGroup `thenCode` plainText }

  "@@" { plainTok LiteralAt `thenCode` plainText }
  "@;;" .* { doLineComment `thenCode` plainText }

  "@" $inlineStarter $identish* { doInlineTag `thenCode` afterTag }
  "@&" $identish+ { doLayoutTag plainText `thenCode` afterTag }
  "@" "#"+ $identish+ { doLevelTag plainText `thenCode` afterTag }
}

-- observe that we don't need to lex '}' here, as that's implicitly handled by
-- plainTextMidLine - when we get to the end brace we simply restore the correct
-- start code

-- we also use doBlanks, which has the effect of created some space inside the
-- attr set that needs to be stripped out by the parser. the issue is that we
-- can't simply skip line endings, because we need to enforce a lack of
-- deindents inside attribute sets, and I don't think alex allows a "monadic
-- skip", meaning we'd have to hack one together ourselves. happily, we can at
-- least skip line space.

<attrSet> {
  -- TODO: should really add some checking here!
  $attrKeyStarter $attrKeyish* { textTok AttrKey }
  "=" { plainTok Equals }
  "{" { doStartBraceGroup attrSet `thenCode` plainText }
  "[" { doStartAttrSet attrSet }
  "]" { doEndAttrSet }
  \ + ;
  @indent { doBlanks }
}

{
-- | Parse a single token from the input stream
lexToken :: Parser (Located Token)
lexToken = do
  parsestate@(ParseState sc inp toks _ _ _) <- get
  case toks of
    (t:toks') -> put (parsestate { parseStatePendingTokens = toks' }) *> pure t
    _ -> case alexScan inp sc of
            AlexEOF -> doEOF
            AlexError ai
              | sc == 0 -> do
                  put $ parsestate {parseStateStartCode = plainText }
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
