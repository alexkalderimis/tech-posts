sed 's/haskell ignore/haskell/' | awk '/<!--/ {a=1}; !a; /--!>/ && a { a -- }'  | runhaskell dejust.hs
