module NewBitwise exposing (..)

import Bitwise


leftShift =
    flip Bitwise.shiftLeft


rightShift =
    flip Bitwise.shiftRight


logicalRightShift =
    flip Bitwise.shiftRightLogical
