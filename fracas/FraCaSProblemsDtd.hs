module FraCaSProblemsDTD where

import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.OneOfN
import Char (isSpace)


{-Type decls-}

newtype Fracas_problems = Fracas_problems (List1 Fracas_problems_) 		deriving (Eq,Show)
data Fracas_problems_ = Fracas_problems_Comment Comment
                      | Fracas_problems_Problem Problem
                      deriving (Eq,Show)
data Comment = Comment Comment_Attrs String
             deriving (Eq,Show)
data Comment_Attrs = Comment_Attrs
    { commentClass :: (Maybe Comment_class)
    } deriving (Eq,Show)
data Comment_class = Comment_class_intro  |  Comment_class_section
                      |  Comment_class_subsection  |  Comment_class_subsubsection
                   deriving (Eq,Show)
data Problem = Problem Problem_Attrs (List1 P) (Maybe Q) H
                       (Maybe A) (Maybe Why) (Maybe Note)
             deriving (Eq,Show)
data Problem_Attrs = Problem_Attrs
    { problemId :: String
    , problemFracas_answer :: (Maybe Problem_fracas_answer)
    , problemFracas_nonstandard :: (Maybe Problem_fracas_nonstandard)
    } deriving (Eq,Show)
data Problem_fracas_answer = Problem_fracas_answer_yes  | 
                             Problem_fracas_answer_no  |  Problem_fracas_answer_unknown  | 
                             Problem_fracas_answer_undef
                           deriving (Eq,Show)
data Problem_fracas_nonstandard = Problem_fracas_nonstandard_true
                                deriving (Eq,Show)
data P = P P_Attrs String
       deriving (Eq,Show)
data P_Attrs = P_Attrs
    { pIdx :: P_idx
    } deriving (Eq,Show)
data P_idx = P_idx_1  |  P_idx_2  |  P_idx_3  |  P_idx_4  | 
             P_idx_5
           deriving (Eq,Show)
newtype Q = Q String 		deriving (Eq,Show)
newtype H = H String 		deriving (Eq,Show)
newtype A = A String 		deriving (Eq,Show)
newtype Why = Why String 		deriving (Eq,Show)
newtype Note = Note String 		deriving (Eq,Show)


{-Instance decls-}

instance XmlContent Fracas_problems where
    fromElem (CElem (Elem "fracas-problems" [] c0):rest) =
        (\(a,ca)->
           (Just (Fracas_problems a), rest))
        (definite fromElem "fracas-problems+" "fracas-problems" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Fracas_problems a) =
        [CElem (Elem "fracas-problems" [] (toElem a))]
instance XmlContent Fracas_problems_ where
    fromElem c0 =
        case (fromElem c0) of
        (Just a,rest) -> (Just (Fracas_problems_Comment a), rest)
        (_,_) ->
                case (fromElem c0) of
                (Just a,rest) -> (Just (Fracas_problems_Problem a), rest)
                (_,_) ->
                    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Fracas_problems_Comment a) = toElem a
    toElem (Fracas_problems_Problem a) = toElem a
instance XmlContent Comment where
    fromElem (CElem (Elem "comment" as c0):rest) =
        (\(a,ca)->
           (Just (Comment (fromAttrs as) a), rest))
        (definite fromText "text" "comment" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Comment as a) =
        [CElem (Elem "comment" (toAttrs as) (toText a))]
instance XmlAttributes Comment_Attrs where
    fromAttrs as =
        Comment_Attrs
          { commentClass = possibleA fromAttrToTyp "class" as
          }
    toAttrs v = catMaybes 
        [ maybeToAttr toAttrFrTyp "class" (commentClass v)
        ]
instance XmlAttrType Comment_class where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "intro" = Just Comment_class_intro
            translate "section" = Just Comment_class_section
            translate "subsection" = Just Comment_class_subsection
            translate "subsubsection" = Just Comment_class_subsubsection
            translate _ = Nothing
    toAttrFrTyp n Comment_class_intro = Just (n, str2attr "intro")
    toAttrFrTyp n Comment_class_section = Just (n, str2attr "section")
    toAttrFrTyp n Comment_class_subsection = Just (n, str2attr "subsection")
    toAttrFrTyp n Comment_class_subsubsection = Just (n, str2attr "subsubsection")
instance XmlContent Problem where
    fromElem (CElem (Elem "problem" as c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (\(c,cc)->
                 (\(d,cd)->
                    (\(e,ce)->
                       (\(f,cf)->
                          (Just (Problem (fromAttrs as) a b c d e f), rest))
                       (fromElem ce))
                    (fromElem cd))
                 (fromElem cc))
              (definite fromElem "<h>" "problem" cb))
           (fromElem ca))
        (definite fromElem "p+" "problem" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Problem as a b c d e f) =
        [CElem (Elem "problem" (toAttrs as) (toElem a ++ maybe [] toElem b
                                             ++ toElem c ++ maybe [] toElem d ++ maybe [] toElem e
                                             ++ maybe [] toElem f))]
instance XmlAttributes Problem_Attrs where
    fromAttrs as =
        Problem_Attrs
          { problemId = definiteA fromAttrToStr "problem" "id" as
          , problemFracas_answer = possibleA fromAttrToTyp "fracas_answer" as
          , problemFracas_nonstandard = possibleA fromAttrToTyp "fracas_nonstandard" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "id" (problemId v)
        , maybeToAttr toAttrFrTyp "fracas_answer" (problemFracas_answer v)
        , maybeToAttr toAttrFrTyp "fracas_nonstandard" (problemFracas_nonstandard v)
        ]
instance XmlAttrType Problem_fracas_answer where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "yes" = Just Problem_fracas_answer_yes
            translate "no" = Just Problem_fracas_answer_no
            translate "unknown" = Just Problem_fracas_answer_unknown
            translate "undef" = Just Problem_fracas_answer_undef
            translate _ = Nothing
    toAttrFrTyp n Problem_fracas_answer_yes = Just (n, str2attr "yes")
    toAttrFrTyp n Problem_fracas_answer_no = Just (n, str2attr "no")
    toAttrFrTyp n Problem_fracas_answer_unknown = Just (n, str2attr "unknown")
    toAttrFrTyp n Problem_fracas_answer_undef = Just (n, str2attr "undef")
instance XmlAttrType Problem_fracas_nonstandard where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "true" = Just Problem_fracas_nonstandard_true
            translate _ = Nothing
    toAttrFrTyp n Problem_fracas_nonstandard_true = Just (n, str2attr "true")
instance XmlContent P where
    fromElem (CElem (Elem "p" as c0):rest) =
        (\(a,ca)->
           (Just (P (fromAttrs as) a), rest))
        (definite fromText "text" "p" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (P as a) =
        [CElem (Elem "p" (toAttrs as) (toText a))]
instance XmlAttributes P_Attrs where
    fromAttrs as =
        P_Attrs
          { pIdx = definiteA fromAttrToTyp "p" "idx" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "idx" (pIdx v)
        ]
instance XmlAttrType P_idx where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "1" = Just P_idx_1
            translate "2" = Just P_idx_2
            translate "3" = Just P_idx_3
            translate "4" = Just P_idx_4
            translate "5" = Just P_idx_5
            translate _ = Nothing
    toAttrFrTyp n P_idx_1 = Just (n, str2attr "1")
    toAttrFrTyp n P_idx_2 = Just (n, str2attr "2")
    toAttrFrTyp n P_idx_3 = Just (n, str2attr "3")
    toAttrFrTyp n P_idx_4 = Just (n, str2attr "4")
    toAttrFrTyp n P_idx_5 = Just (n, str2attr "5")
instance XmlContent Q where
    fromElem (CElem (Elem "q" [] c0):rest) =
        (\(a,ca)->
           (Just (Q a), rest))
        (definite fromText "text" "q" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Q a) =
        [CElem (Elem "q" [] (toText a))]
instance XmlContent H where
    fromElem (CElem (Elem "h" [] c0):rest) =
        (\(a,ca)->
           (Just (H a), rest))
        (definite fromText "text" "h" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (H a) =
        [CElem (Elem "h" [] (toText a))]
instance XmlContent A where
    fromElem (CElem (Elem "a" [] c0):rest) =
        (\(a,ca)->
           (Just (A a), rest))
        (definite fromText "text" "a" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (A a) =
        [CElem (Elem "a" [] (toText a))]
instance XmlContent Why where
    fromElem (CElem (Elem "why" [] c0):rest) =
        (\(a,ca)->
           (Just (Why a), rest))
        (definite fromText "text" "why" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Why a) =
        [CElem (Elem "why" [] (toText a))]
instance XmlContent Note where
    fromElem (CElem (Elem "note" [] c0):rest) =
        (\(a,ca)->
           (Just (Note a), rest))
        (definite fromText "text" "note" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Note a) =
        [CElem (Elem "note" [] (toText a))]


{-Done-}
