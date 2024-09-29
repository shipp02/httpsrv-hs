import os.path
USER_CODE_LINE = """-- space for user code<<"""
if __name__ == '__main__':
    if os.path.isfile('src/HuffmanTable.hs'):

        with open ('src/HuffmanTable.hs', mode = 'r') as hf:
            lines = hf.readlines()
            for i,l in enumerate(lines):
                if USER_CODE_LINE in l:
                    uc = lines[i+1:]
    else:
        uc = ''      
    with open('src/HuffmanTable.hs', mode='w')  as hf:
        hf.write("""
{-# LANGUAGE OverloadedStrings #-}
module HuffmanTable
(
   huffmanTable,
   huffmanTree,
   HuffmanTree(..),
   Bit(..)
) where

import Data.List as L
import Data.BitVector as BV


data Bit = T | F deriving (Show, Eq)
huffmanTable :: [(Int, [Bit])]
huffmanTable = [
""")
        with open('src/huffman_table.txt', mode='r') as file:
            y = ''
            for line in file:
                hf.write(f'{y}({line[1:4]},[')
                sbits = line[5:51].strip().rstrip()
                x = ''
                for b in sbits:
                   
                    if b == '|':
                        continue
                    elif b == '1':
                        hf.write(f'{x}T')
                        x = ','
                    elif b == '0':
                        hf.write(f'{x}F')
                        x = ' ,'
                hf.write(f'])\n')
                y = ' ,'
            hf.write(' ]\n')
        hf.write(f'{USER_CODE_LINE}\n')
        [hf.write(u) for u  in  uc]

"""
data HuffmanTree =  
    Nil
    | Leaf Int
    | Internal HuffmanTree HuffmanTree
    deriving (Show,  Eq)

addSymbol :: HuffmanTree -> (Int, [Bit]) -> HuffmanTree
addSymbol Nil (!x, T:bits) = Internal Nil (addSymbol Nil (x, bits))
addSymbol Nil (!x, F:bits) = Internal (addSymbol Nil (x, bits)) Nil
addSymbol (Internal !l r) (!x, T:bits) = Internal l (addSymbol r (x, bits))
addSymbol (Internal !l r) (!x, F:bits) = Internal (addSymbol l (x, bits)) r
addSymbol x@(Leaf _) _ = x
addSymbol _ (x, []) = Leaf x

huffmanTree :: HuffmanTree
huffmanTree = L.foldr (flip addSymbol) Nil huffmanTable

decodeBit :: HuffmanTree -> [Bit] -> (Int, [Bit])
decodeBit (Internal l r) (T:bits) = decodeBit r bits
decodeBit (Internal l r) (F:bits) = decodeBit l bits
decodeBit (Leaf x) left = (x,left)

boolToBit :: Bool -> Bit
boolToBit True = T
boolToBit False = F

bitToBool ::  Bit -> Bool
bitToBool T =  True
bitToBool F = False
decodeBV :: HuffmanTree -> BV.BV -> (Int, BV.BV)
decodeBV t bv = 
    let
        (i, l) = decodeBit t (L.map boolToBit . toBits $ bv)
    in
        (i, fromBits (L.map bitToBool l))
"""