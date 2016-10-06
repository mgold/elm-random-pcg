module NewBitwise exposing (..)

import Bitwise


shiftLeftBy =
    flip Bitwise.shiftLeft


shiftRightBy =
    flip Bitwise.shiftRight


shiftRightZfBy =
    flip Bitwise.shiftRightLogical
