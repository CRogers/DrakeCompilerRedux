module Desugar where

import Data.Monoid ((<>))
import Tree

desugarNamespace :: Namespace -> [DeclInfo]
desugarNamespace (Namespace n ds) = map cn ds
	where j s = n <> Name "::" <> s
	      cn (DeclInfo n' d) = DeclInfo (j n') d

desugarNamespaces :: [Namespace] -> [DeclInfo]
desugarNamespaces = concatMap desugarNamespace