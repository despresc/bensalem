{
module Scriba.Markup.ScribaML.OtherLexer
  ( lexToken
  , lexTokens
  , alexScan )
where


import Control.Monad.Except (throwError)
import Data.Sequence (Seq((:<|)))
import Control.Monad.State (MonadState(..), modify)
import Data.Text (Text)
import Scriba.Markup.ScribaML.ParserUtils
import Scriba.Markup.ScribaML.Token
import qualified Data.Text as T
}

-- does the backslash need to be escaped?
$specialchar = [\\ \[ \]  \{ \} = \  \n \,]
$whitespace = [\  \n]
$plaintext = $printable # $specialchar
$identish = $printable # [ \[ \] \{ \} ]
$verbatimPlain = [^ \  \n `]
$backslash = [\\]
$inlineStarter = $identish # [` \% # &]

@blanks = (\n | \ )*
@indent = \n @blanks

tokens :-

<0> {
  -- we are a little strict here. if there is no white space we recover in
  -- lexToken
  (\n | \ )+ { textTok Indent `thenCode` plainText}
}

<plainText> {
  $plaintext+ { textTok PlainText }

  \ + { \toklen spn _ -> pure $ Located spn $ LineSpace toklen }
  @indent { doIndent }

  "{" { doStartBraceGroup }
  "}" { doEndBraceGroup }
  "[" { doStartAttrSet }
  "]" { doEndAttrSet }
  "=" { plainTok Equals }

  "\\" { plainTok $ Escape EscBackslash }
  "\{" { plainTok $ Escape EscLbrace }
  "\}" { plainTok $ Escape EscRbrace }
  "\[" { plainTok $ Escape EscLbracket }
  "\]" { plainTok $ Escape EscRbracket }

  "\`" { plainTok StartVerbatim `thenCode` verbatimPlain }
  "\%" .* { doLineComment }

  "\" "#"+ $identish+ { doLevelTag }
  "\&" $identish+ { doLayoutTag }
  "\" $inlineStarter $identish* { doInlineTag }
}

<verbatimPlain> {
  $verbatimPlain+ { textTok VerbatimPlainText }
  @indent { doIndent }
  "``" { plainTok $ VerbatimBacktick }
  "`/" { plainTok EndVerbatim `thenCode` plainText }
}

{

-- | Parse a single token from the input stream
lexToken :: Parser (Located Token)
lexToken = do
  parsestate@(ParseState sc inp toks _ _) <- get
  case toks of
    (t:toks') -> put (parsestate { parseStatePendingTokens = toks' }) *> pure t
    _ -> case alexScan inp sc of
            AlexEOF -> doEOF
            AlexError ai
              | sc == 0 -> do
                  put $ parsestate {parseStateStartCode = plainText}
                  lexToken
              | otherwise -> throwError $ LexerError $ NoToken $ getAlexInputSrcPos ai
            AlexSkip inp' _ -> setInput inp' >> lexToken
            AlexToken inp' toklen act -> do
              let initPos = getAlexInputSrcPos inp
              let finalPos = getAlexInputSrcPos inp'
              let name = getAlexInputSrcName inp
              let posSpan = SrcSpan name initPos finalPos
              setInput inp' >> act toklen posSpan (T.take toklen $ getAlexInputText inp)

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
