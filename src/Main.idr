module Main

import Compiler.Common
import Idris.Driver
import Rust

main : IO ()
main = mainWithCodegens [("rust", rustCodegen)]
