{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Data.List
import           Data.Text                         hiding ( intercalate
                                                          , map
                                                          )
import           Servant.Elm                              ( DefineElm(DefineElm)
                                                          , Proxy(Proxy)
                                                          , defaultOptions
                                                          , defElmImports
                                                          , defElmOptions
                                                          , deriveBoth
                                                          , generateElmModuleWith
                                                          )
import           Api
import           Models

main :: IO ()
main = generateElmModuleWith
  defElmOptions
  ["Api"]
  defElmImports
  "client"
  [
   -- DefineElm (Proxy :: Proxy Project),
    DefineElm (Proxy :: Proxy Item),
    DefineElm (Proxy :: Proxy ItemId)
  ]
  (Proxy :: Proxy Api)
