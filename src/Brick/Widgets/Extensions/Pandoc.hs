{-# LANGUAGE OverloadedStrings #-}
module Brick.Widgets.Extensions.Pandoc where
import qualified Brick.Types as Brick
import qualified Brick.AttrMap as BrickAttr
import Brick.Widgets.Core (withAttr, vBox, hBox, emptyWidget)
import qualified Brick.Widgets.Core as W
import qualified Skylighting.Syntax as S
import qualified Text.Pandoc.Definition as Pandoc
import Text.Pandoc.Definition

smallcaps, superscript, subscript :: BrickAttr.AttrName
italic      = "italic"
bold        = "bold"
strikeout   = "strikeout"
smallcaps   = "smallcaps"
superscript = "superscript"
subscript   = "subscript"
quoted      = "quoted"
citation    = "citation"
code        = "code"
math        = "math"
image       = "image"
paragraph   = "paragraph"
blockquote  = "blockquote"
codeBlock   = "codeBlock"

-- | Convert 'Text.Pandoc.Definition.Inline' into 'Brick.Type.Widget'
--
-- Some of them are unable to convert properly (e.g. TeX, image), and some of them are not completely implemented
inline :: Pandoc.Inline -> Brick.Widget n
inline (Str         txt)                   = W.txt txt
inline (Emph        inlines)               = withAttr italic $ hBox $ map inline inlines
-- TODO: 'Underline' data constructor is introduced in pandoc-types-1.21, but currently I only can use 1.20 because of Stackage.
-- inline (Underline   inlines)               = withAttr underline $ hBox $ map inline inlines
inline (Strong      inlines)               = withAttr bold $ hBox $ map inline inlines
inline (Strikeout   inlines)               = withAttr strikeout $ hBox $ map inline inlines
inline (Superscript inlines)               = withAttr superscript $ hBox $ W.txt "^" : map inline inlines ++ [W.txt "^"]  -- Superscript isn't supported on terminal, therefore I use pandoc markdown
inline (Subscript   inlines)               = withAttr subscript $ hBox $ W.txt "~" : map inline inlines ++ [W.txt "~"]  -- Subscript   isn't supported on terminal, therefore I use pandoc markdown
inline (SmallCaps   inlines)               = withAttr smallcaps $ hBox $ map inline inlines -- TODO: What is proper way to represent SmallCaps?
inline (Quoted      quoteType inlines)     = let quote = case quoteType of
                                                                    SingleQuote -> "'"
                                                                    DoubleQuote -> "\""
                                                    in withAttr quoted $ hBox $ W.txt quote : map inline inlines ++ [W.txt quote]
inline (Cite        citations  inlines)    = withAttr citation $ hBox $ map inline inlines -- TODO: Somehow refer the citations
inline (Code        attr txt)              = withAttr code $ W.txt txt -- TODO: Use attr e.g. highlighting
inline Space                               = emptyWidget  -- TODO: Should we distinguish this?
inline SoftBreak                           = emptyWidget  -- TODO: Should we distinguish this?
inline LineBreak                           = emptyWidget  -- TODO: Should we distinguish this?
inline (Math        mathType txt)          = withAttr math $ W.txt txt    -- TODO: Render TeX; insert linebreak if it's 'DisplayMath'; I don't know how to render TeX in CUI
inline (RawInline   format txt)            = W.txt txt    -- TODO: Research what the 'format' means and what is 'RawInline'
inline (Link attr inlines (link, title))   = W.hyperlink link $ hBox $ map inline inlines   -- TODO: Use 'attr' and 'title'
inline (Image attr inlines (link, title))  = withAttr image $ W.hyperlink link  $ hBox $ map inline inlines   -- TODO: Use 'attr', 'title'; support showing images with some terminal?
inline (Note        blocks)                = hBox $ map block blocks  -- TODO: is this correct usage of 'Note'?
inline (Span        attr inlines)          = hBox $ map inline inlines -- todo: use 'attr'; should we add new 'attrname' for span?


block :: Pandoc.Block -> Brick.Widget n
block (Plain inlines)                                                = vBox $ map inline inlines
block (Para inlines)                                                 = withAttr paragraph $ vBox $ map inline inlines
block (LineBlock inlineses)                                          = withAttr lineblock $ hBox . map inline <$> inlineses
block (CodeBlock attr txt)                                           = withAttr codeBlock $ highlight (getLanguage attr) txt
block (RawBlock format txt)                                          = withAttr rawBlock  $ B.txt txt  -- TODO: use format somehow
block (BlockQuote blocks)                                            = withAttr blockquote $ vBox $ map block blocks
block (OrderedList listAttr blockses)                                = withAttr orderedlist  $ zipWith zipFunc (createNumber listAttr) $ fmap block blockses
    where
        numberlist DefaultStyle = numberlist Decimal
        numberlist Example      = numberlist Decimal
        numberlist Decimal      = show <$> [0,1..]
        numberlist LowerRoman   = iterate (++ "i") "i"
        numberlist UpperRoman   = iterate (++ "I") "I"
        numberlist LowerAlpha   = pure <$> ['a'..'z']
        numberlist UpperAlpha   = pure <$> ['A'..'Z']
        -- | Create infinite list of list head
        createNumber :: ListAttribute -> [Text]
        createNumber (start, style, delim) = let (delim1, delm2) = case delim of
                                                            DefaultDelim -> ("",".")
                                                            Period       -> ("",".")
                                                            OneParen     -> ("",")")
                                                            TwoParens    -> ("(",")")
                                             in fmap (\i -> delim1 ++ i ++ delim2) $ drop (start - 1) (numberlist style)
        -- | zip two
        zipFunc :: (Text, Widget n) -> Widget n
        zipFunc (t, w) = hBox [B.txt t, w]
block (BulletList blockses)                                          = withAttr bulletList $ vBox fmap (hBox $ B.txt "- " : block) blockses


-- | Make all keys lower case so that we can look up easily
defaultSyntaxMap' :: SyntaxMap
defaultSyntaxMap' = mapKeys . map toLower $ defaultSyntaxMap

-- | Get language Syntax from Pandoc's CodeBlock attribute
--
-- Note that this isn't completed yet, because there will be some inconsistent spellings
-- and this doesn't support it yet.
getLanguage :: P.Attr -> Maybe S.Syntax
getLanguage (_, cls, _) = foldl1 (\x y -> if isJust y then y else x) $ map (flip lookup defaultSyntaxMap' . toLower) cls

