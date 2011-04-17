module Search.Xapian.Stem where

-- * stemmer

data Stem = Danish
          | Dutch
          | DutchKraaijPohlmann -- ^ A different Dutch stemmer
          | English       -- ^ Martin Porter's 2002 revision of his stemmer
          | EnglishLovins -- ^ Lovin's stemmer
          | EnglishPorter -- ^ Porter's stemmer as described in his 1980 paper
          | Finnish
          | French
          | German
          | German2 -- ^ Normalises umlauts and ß
          | Hungarian
          | Italian
          | Norwegian
          | Portuguese
          | Romanian
          | Russian
          | Spanish
          | Swedish
          | Turkish
          deriving (Show, Eq)
