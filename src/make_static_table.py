if __name__ == "__main__":
    with open('src/StaticTable.hs', mode='w')  as hf:
        hf.write("""
{-# LANGUAGE OverloadedStrings #-}
module StaticTable
(
   staticTable
) where

import Data.List as L
import Data.BitVector as BV

staticTable :: [(Int, Text, Text)]
staticTable = [
""")
        with open('src/static_table.txt') as st:
            x = ''
            for line in st:
                _, num, key, val, *args = line.split('|')
                hf.write(f'  {x}({num.strip().rstrip()}, "{key.strip().rstrip()}", "{val.strip().rstrip()}")\n')
                x = '  ,'
            hf.write(' ]')