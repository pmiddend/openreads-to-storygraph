module Main where

import Data.Version (showVersion)
import Paths_openreads_to_storygraph (version)

openreadsToStorygraphVersion = showVersion version -- VERSIONSTRING

main = putStrLn "hello world"
