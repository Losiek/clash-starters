-- | Adder
module Example.Adder where

import Clash.Explicit.Prelude

adder ::
    (KnownDomain dom, KnownNat n) =>    -- Constraints of the circuit
    Clock dom ->    -- Clock signal of the circuit
    Reset dom ->    -- Reset signal of the circuit
    Enable dom ->   -- Enable signal of the circuit
    Signal dom (Unsigned n) ->  -- First input signal
    Signal dom (Unsigned n) ->  -- Second input signal
    Signal dom (Unsigned n)     -- Output signal
adder clk rst ena a b = c
    where
        c = a' + b'
        a' = register clk rst ena 0 a
        b' = register clk rst ena 0 b


topEntity ::
    Clock System ->
    Reset System ->
    Enable System ->
    Signal System (Unsigned 8) ->
    Signal System (Unsigned 8) ->
    Signal System (Unsigned 8)
topEntity = adder

{-# ANN topEntity
    (Synthesize
        { t_name = "adder"
        , t_inputs =
            [ PortName "clk"
            , PortName "rst"
            , PortName "ena"
            , PortName "a"
            , PortName "b"
            ]
        , t_output = PortName "c"
        }) #-}

