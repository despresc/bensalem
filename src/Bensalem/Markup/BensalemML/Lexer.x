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

-- Characters that are significant in plain text
$plainSignificant = [ \{ \} \  \n \\ ]

-- Insignificant characters in plain text
$plainPlainText = $printable # $plainSignificant

-- Characters that can be escaped
$escapeChars = [ \\ \{ \} \[ \] ]

-- Characters that are significant after a backslash but (roughly) wouldn't be
-- the start of an inline tag
$notInlineStarterButSig = [ $escapeChars [ \% \# \& ] ]

-- Characters that (very roughly) might start an inline tag
$tagStarter = $plainPlainText # $notInlineStarterButSig

-- Characters that (again, very roughly) could continue a tag
$tagContinue = $tagStarter

-- Characters that, when they occur immediately after a tag, are plain text
$afterTagPlainText = $plainPlainText # [ \[ ]

-- Characters that might start an attribute key
$attrKeyStarter = $tagStarter # [ = ]

-- Characters that might continue an attribute key. This happens to be the same
-- as $attrKeyStarter for the moment
$attrKeyContinue = $attrKeyStarter

-- A sequence of any amount of spaces or newlines
@anyWhitespace = (\n | \ )*

-- Any whitespace following a newline (with that leading newline) though of
-- course only the terminal run of line space is really significant for layout
-- purposes
@indent = \n @anyWhitespace

-- A full tag name
@tagName = $tagStarter $tagContinue*

-- A full attribute key
@attrKey = $attrKeyStarter $attrKeyContinue*

tokens :-

<0> {
  -- we are a little strict here. if there is no white space we recover in
  -- lexToken.
  (\n | \ )+ { textTok Indent `thenCode` plainText }
}

<plainText> {
  $plainPlainText+ { textTok PlainText }
  \ + { \toklen spn _ -> pure $ Located spn $ LineSpace toklen }
  @indent { doIndent }

  "{" { doStartBraceGroup plainText }
  "}" { doEndBraceGroup }

  "\\" { plainTok (TokenEscape EscapeBackslash) }
  "\{" { plainTok (TokenEscape EscapeOpenBrace) }
  "\}" { plainTok (TokenEscape EscapeCloseBrace) }
  "\[" { plainTok (TokenEscape EscapeOpenBracket) }
  "\]" { plainTok (TokenEscape EscapeCloseBracket) }
  "\%%" .* { doLineComment }

  \\ @tagName { doInlineTag `thenCode` afterTag }
  \\& @tagName { doLayoutTag plainText `thenCode` afterTag }
  "\#" "#"* @tagName { doLevelTag plainText `thenCode` afterTag }
}

-- sadly repetitive - I could probably do some kind of recovery trick in
-- lexToken, but I'd rather not right now

-- TODO: I'm being quite silly - I think I can in fact do a <plainText,afterTag>
-- block of rules - I just have to query the lexer state and save the exact
-- start code I want. Unsure why I thought I needed to do it this way. Now that
-- I think about it, I can't really do that with the level and layout tags,
-- because I want them to restore plainText on scope completion. Oh, but then
-- that's fine, because they just unconditionally restore plainText. Maybe I
-- should just keep what the codes are in the lexer state...

<afterTag> {
  "[" { doStartAttrSet plainText `thenCode` attrSet }

  $afterTagPlainText+ { textTok PlainText `thenCode` plainText }
  \ + { (\toklen spn _ -> pure $ Located spn $ LineSpace toklen) `thenCode` plainText }
  @indent { doIndent `thenCode` plainText }

  "{" { doStartBraceGroup plainText `thenCode` plainText }
  "}" { doEndBraceGroup `thenCode` plainText }

  "\\" { plainTok (TokenEscape EscapeBackslash) `thenCode` plainText }
  "\{" { plainTok (TokenEscape EscapeOpenBrace) `thenCode` plainText }
  "\}" { plainTok (TokenEscape EscapeCloseBrace) `thenCode` plainText }
  "\[" { plainTok (TokenEscape EscapeOpenBracket) `thenCode` plainText }
  "\]" { plainTok (TokenEscape EscapeCloseBracket) `thenCode` plainText }
  "\%%" .* { doLineComment }

  \\ @tagName { doInlineTag }
  \\& @tagName { doLayoutTag plainText  }
  "\#" "#"* @tagName { doLevelTag plainText }
}

-- observe that we don't need to lex '}' here, as that's implicitly handled by
-- plainTextMidLine - when we get to the end brace we simply restore the correct
-- start code

-- we also use doIndent, which has the effect of created some space inside the
-- attr set that needs to be stripped out by the parser. the issue is that we
-- can't simply skip line endings, because we need to enforce a lack of
-- deindents inside attribute sets, and I don't think alex allows a "monadic
-- skip", meaning we'd have to hack one together ourselves. happily, we can at
-- least skip line space.

<attrSet> {
  -- TODO: should really add some checking here!
  @attrKey { textTok AttrKey }
  "=" { plainTok Equals }
  "{" { doStartBraceGroup attrSet `thenCode` plainText }
  "[" { doStartAttrSet attrSet }
  "]" { doEndAttrSet }
  \ + ;
  @indent { doIndent }
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
