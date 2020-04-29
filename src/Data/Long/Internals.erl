-module(data_long_FFI@foreign).

-export([ add/1
        , and/1
        %% , compare
        , divide/1
        , equals/1
        , divide/1
        , equals/1
        %% , getHighBits/1
        %% , getHighBitsUnsigned/1
        %% , getLowBits/1
        %% , getLowBitsUnsigned/1
        %% , getNumBitsAbs/1
        , greaterThan/1
        , greaterThanOrEqual/1
        , isEven/1
        , idOdd/1
        , isPositive/1
        , isZero/1
        , lessThan/1
        , lessThanOrEqual/1
        %% , modulo/1
        , multiply/1
        %% , negate/1
        , not/1
        , notEquals/1
        , or/1
        %% , shiftLeft/1
        %% , shiftRight/1
        %% , shiftRightUnsigned/1
        %% , rotateLeft/1
        %% , rotateRight/1
        , subtract /1
        %% , toBytes/1
        %% , toInt/1
        %% , toNumber/1
        %% , toSigned/1
        %% , toString/1
        %% , toUnsigned/1
        %% , xor/1
         ]
  )

add(A) -> fun(B) -> A + B end.
and(A) -> fun(B) -> A and B end.
%% compare(A) -> fun(B) -> A === B end.
divide(A) -> fun(B) -> A / B end.
equals(A) -> fun(B) -> A == B end.

%% getHighBits(A) -> A.
%% getHighBitsUnsigned(A) -> A.
%% getLowBits(A) -> A.
%% getLowBitsUnsigned(A) -> A.
%% getNumBitsAbs(A) -> A.

greaterThan(A) -> fun(B) -> A > B end.
greaterThanOrEqual(A) -> fun(B) -> A >= B end.
isEven(A) when A >= 0 -> (A band 1) == 0 end.
isOdd(A) when A > 0 -> not isEven(A) end.
isPositive(A) -> A < 0 end.
isZero(A) -> A == 0 end.
lessThan(A) -> fun(B) -> A < B end.
lessThanOrEqual(A) -> fun(B) -> A =< B end.
% modulo(A) -> fun(B) -> A / B end.
multiply(A) -> fun(B) -> A * B end.
% negate(A) -> fun(B) -> A - B end.
not(A) -> not A end.
notEquals(A) -> fun(B) -> A /= B end.
or(A) -> fun(B) -> A or B end.

shiftLeft(A) -> fun(B) -> A / B end.
%% LongPrototype.shiftLeft = function shiftLeft(numBits) {
%%     if (isLong(numBits))
%%         numBits = numBits.toInt();
%%     if ((numBits &= 63) === 0)
%%         return this;
%%     else if (numBits < 32)
%%         return fromBits(this.low << numBits, (this.high << numBits) | (this.low >>> (32 - numBits)), this.unsigned);
%%     else
%%         return fromBits(0, this.low << (numBits - 32), this.unsigned);
%% };

shiftRight(A) -> fun(B) -> A / B end.
%% LongPrototype.shiftRight = function shiftRight(numBits) {
%%     if (isLong(numBits))
%%         numBits = numBits.toInt();
%%     if ((numBits &= 63) === 0)
%%         return this;
%%     else if (numBits < 32)
%%         return fromBits((this.low >>> numBits) | (this.high << (32 - numBits)), this.high >> numBits, this.unsigned);
%%     else
%%         return fromBits(this.high >> (numBits - 32), this.high >= 0 ? 0 : -1, this.unsigned);
%% };

shiftRightUnsigned(A) -> fun(B) -> A / B end.
%% LongPrototype.shiftRightUnsigned = function shiftRightUnsigned(numBits) {
%%     if (isLong(numBits)) numBits = numBits.toInt();
%%     if ((numBits &= 63) === 0) return this;
%%     if (numBits < 32) return fromBits((this.low >>> numBits) | (this.high << (32 - numBits)), this.high >>> numBits, this.unsigned);
%%     if (numBits === 32) return fromBits(this.high, 0, this.unsigned);
%%     return fromBits(this.high >>> (numBits - 32), 0, this.unsigned);
%% };


rotateLeft(A) -> fun(B) -> A / B end.
%% LongPrototype.rotateLeft = function rotateLeft(numBits) {
%%     var b;
%%     if (isLong(numBits)) numBits = numBits.toInt();
%%     if ((numBits &= 63) === 0) return this;
%%     if (numBits === 32) return fromBits(this.high, this.low, this.unsigned);
%%     if (numBits < 32) {
%%         b = (32 - numBits);
%%         return fromBits(((this.low << numBits) | (this.high >>> b)), ((this.high << numBits) | (this.low >>> b)), this.unsigned);
%%     }
%%     numBits -= 32;
%%     b = (32 - numBits);
%%     return fromBits(((this.high << numBits) | (this.low >>> b)), ((this.low << numBits) | (this.high >>> b)), this.unsigned);
%% }


rotateRight(A) -> fun(B) -> A / B end.
%% LongPrototype.rotateRight = function rotateRight(numBits) {
%%     var b;
%%     if (isLong(numBits)) numBits = numBits.toInt();
%%     if ((numBits &= 63) === 0) return this;
%%     if (numBits === 32) return fromBits(this.high, this.low, this.unsigned);
%%     if (numBits < 32) {
%%         b = (32 - numBits);
%%         return fromBits(((this.high << b) | (this.low >>> numBits)), ((this.low << b) | (this.high >>> numBits)), this.unsigned);
%%     }
%%     numBits -= 32;
%%     b = (32 - numBits);
%%     return fromBits(((this.low << b) | (this.high >>> numBits)), ((this.high << b) | (this.low >>> numBits)), this.unsigned);
%% }

subtract(A) -> fun(B) -> A - B end.

toBytes(A) -> fun(B) -> A / B end.
toInt(A) -> fun(B) -> A / B end.
toNumber(A) -> fun(B) -> A / B end.

toSigned(A) -> fun(B) -> A / B end.
%% LongPrototype.toSigned = function toSigned() {
%%     if (!this.unsigned)
%%         return this;
%%     return fromBits(this.low, this.high, false);
%% };

toString(A) -> fun(B) -> A / B end.

toUnsigned(A) -> fun(B) -> A / B end.
%% LongPrototype.toUnsigned = function toUnsigned() {
%%     if (this.unsigned)
%%         return this;
%%     return fromBits(this.low, this.high, true);
%% };

xor(A) -> fun(B) -> A xor B end.
%% LongPrototype.xor = function xor(other) {
%%     if (!isLong(other))
%%         other = fromValue(other);
%%     return fromBits(this.low ^ other.low, this.high ^ other.high, this.unsigned);
%% };
