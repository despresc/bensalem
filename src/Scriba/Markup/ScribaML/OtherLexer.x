{
module Scriba.Markup.ScribaML.OtherLexer
  ( lexToken
  , lexTokens
  , alexScan )
where


import Control.Monad.Except (throwError)
import Control.Monad.State (MonadState(..), modify)
import Data.Text (Text)
import Scriba.Markup.ScribaML.ParserUtils
import Scriba.Markup.ScribaML.Token
import qualified Data.Text as T
}

-- does the backslash need to be escaped?
$specialchar = [\\ & \[ \]  \{ \} = \  \n]
$whitespace = [\  \n]
$plaintext = $printable # $specialchar
$identish = $plaintext # [ \[ \{ \} ]
$verbatimPlain = [^ \  \n `]
$backslash = [\\]

@blanks = (\n | \ )*
@indent = \n @blanks

tokens :-

<0> {
  @blanks { withCode plainText $ textTok Indent }
}

<plainText> {
  $plaintext+ { textTok PlainText }

  \ + { \toklen spn _ -> pure $ Located spn $ LineSpace toklen }
  @indent { textTok Indent }

  "{" { plainTok Lbrace }
  "}" { plainTok Rbrace }
  "[" { plainTok Lbracket }
  "]" { plainTok Rbracket }
  "=" { plainTok Equals }

  "\\" { plainTok $ Escape EscBackslash }
  "\&" { plainTok $ Escape EscAnd }
  "\{" { plainTok $ Escape EscLbrace }
  "\}" { plainTok $ Escape EscRbrace }
  "\[" { plainTok $ Escape EscLbracket }
  "\]" { plainTok $ Escape EscRbracket }

  "\`" { withCode verbatimPlain $ plainTok StartVerbatim }
  "\%" .* { dropTextTok 2 LineComment }

  -- TODO: identifier checking here
  "\" $identish+ { dropTextTok 1 BackslashTag }
  "\" "#"+ $identish+ { doNumberSignTag }
  "&" $identish+ { dropTextTok 1 AmpTag }
}

<verbatimPlain> {
  $verbatimPlain+ { textTok VerbatimPlainText }
  @indent { textTok Indent }
  "``" { plainTok $ VerbatimBacktick }
  "`/" { withCode plainText $ plainTok EndVerbatim }
}

{
type AlexAction = Int -> SrcSpan -> Text -> Parser (Located Token)

plainTok :: Token -> AlexAction
plainTok tok _ sp _ = pure $ Located sp tok

textTok :: (Text -> Token) -> AlexAction
textTok tok _ sp t = pure $ Located sp $ tok t

dropTextTok :: Int -> (Text -> Token) -> AlexAction
dropTextTok n tok = textTok (tok . T.drop n)

setInput :: AlexInput -> Parser ()
setInput ai = modify $ \ps -> ps { parseStateInput = ai }

setCode :: Int -> Parser ()
setCode n = modify $ \ps -> ps { parseStateStartCode = n }

withCode :: Int -> AlexAction -> AlexAction
withCode cd f n sp t = f n sp t <* setCode cd

doNumberSignTag :: AlexAction
doNumberSignTag _ sp t = pure $ Located sp tok
  where
    (numt, tagt) = T.span (== '#') t
    tok = NumberSignTag (T.length numt) tagt

-- | Parse a single token from the input stream
lexToken :: Parser (Located Token)
lexToken = do
  ParseState sc inp <- get
  case alexScan inp sc of
    AlexEOF -> pure $ Located srcSpan TokenEOF
      where
        srcSpan = SrcSpan (getAlexInputSrcName inp) sp sp
        sp = getAlexInputSrcPos inp
    AlexError _ -> throwError ParseError
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
