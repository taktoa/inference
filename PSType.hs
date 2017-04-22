module PSType where


data PSType ann
  = TyVar
    { _tyAnn :: ann
    , _tyVar :: Reference ann
    }
  | TyCon
    { _tyAnn :: ann
    , _tyCon :: Reference ann
    }
  | TyString
    { _tyAnn    :: ann
    , _tyString :: Text
    }
  | TyWildcard
    { _tyAnn :: ann }
  | TyREmpty
    { _tyAnn :: ann }
  | TyRCons
    { _tyAnn      :: ann
    , _tyRowLabel :: Label
    , _tyRowType  :: PSType ann
    , _tyRest     :: PSType ann
    }
  | TyConstraint
    { _tyAnn        :: ann
    , _tyConstraint :: Constraint
    , _tyBody       :: Binder ann
    }
  | TyUniversal
    { _tyAnn :: ann
    , _foo   :: Int }

type Reference ann = Name (PSType ann)
type Binder ann = Bind (Reference ann) (PSType ann)
