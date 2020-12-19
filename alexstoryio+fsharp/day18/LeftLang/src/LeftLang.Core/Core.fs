namespace LeftLang.Core

module Types =
    type Number = int64

module Operator =
    let addition x y: int64 = x + y
    let multiplication x y: int64 = x * y
    let division x y: int64 = x / y
    let subtraction x y: int64 = x / y