(type type-val ()
  BoolType CharType DoubleType IntType StringType SymbolType VoidType
  (AbstractType string (cons type-val))
  (FunctionType type-val type-val)
  (ArrayType type-val)
  (AnyType string))

(type pair ('a 'b) (Pair 'a 'b))
(type option ('a) Nothing (Just 'a))
