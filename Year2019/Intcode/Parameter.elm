module Year2019.Intcode.Parameter exposing (Parameter(..))


type Parameter
    = Immediate Int
    | Position Int
    | Relative Int
