module Main where

import Cloud.Azure.Storage (storageAccount)
import Cloud.Azure.Storage.Table (runStorageTable, tables)

main :: IO ()
main = do
    let acc = storageAccount
            "welmokpilog"
            "LfXCQBPcj4u313vfz+mx+pGC2fWwnhAo+2UW5SVAnAqIjYBEPt76oievOM3LpV35BwYCYi6ufeSBRZCs/h3c8Q=="
    runStorageTable acc tables >>= print
