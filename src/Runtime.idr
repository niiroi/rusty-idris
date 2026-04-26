module Runtime

import Language.Reflection

%language ElabReflection

export
runtime : String
runtime = %runElab do
  Just str <- readFile CurrentModuleDir "runtime.rs"
    | Nothing => fail "Can not read file `runtime.rs`"
  pure str
