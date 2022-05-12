module Common.Specifiers (

) where


data Purity = Pure | Impure | Unsafe
    deriving (Show, Eq)

data Mutab = Mut | Imut
    deriving (Show, Eq)

data Visib = Export | Intern
    deriving (Show, Eq)


instance Pretty Purity where
    pretty Pure = "pure"
    pretty Impure = "impure"
    pretty Unsafe = "unsafe"
    detailed = show

instance Pretty Mutab where
    pretty Mut = "mut"
    pretty Imut = ""
    detailed = show

instance Pretty Visib where
    terse Export = "ex"
    terse Intern = "in"
    pretty Export = "export"
    pretty Intern = "intern"
    detailed = show
