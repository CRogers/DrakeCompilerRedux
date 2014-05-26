{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module LLVMBuilder.Numbers where

import Data.Singletons
import LLVMBuilder.Types

type P0 n = 'Succ n
type P1 n = P0 (P0 n)
type P2 n = P1 (P1 n)
type P3 n = P2 (P2 n)
type P4 n = P3 (P3 n)
type P5 n = P4 (P4 n)
type P6 n = P5 (P5 n)

type N0 = 'Zero
type SN0 = Sing N0

type N1 = P0 'Zero
type SN1 = Sing N1

type N2 = P1 'Zero
type SN2 = Sing N2

type N4 = P2 'Zero
type SN4 = Sing N4

type N8 = P3 'Zero
type SN8 = Sing N8

type N16  = P4 'Zero
type SN16 = Sing N16

type N32 = P5 'Zero
type SN32 = Sing N32

p0 n = SSucc n
p1 n = p0 (p0 n)
p2 n = p1 (p1 n)
p3 n = p2 (p2 n)
p4 n = p3 (p3 n)
p5 n = p4 (p4 n)
p6 n = p5 (p5 n)

sn0 :: SN0
sn0 = SZero

sn1 :: SN1
sn1 = p0 SZero

sn2 :: SN2
sn2 = p1 SZero

sn4 :: SN4
sn4 = p2 SZero

sn8 :: SN8
sn8 = p3 SZero

sn16 :: SN16
sn16 = p4 SZero

sn32 :: SN32
sn32 = p5 SZero