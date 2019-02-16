var L = require("long");

// Constants
exports._zero     = L.ZERO;
exports._one      = L.ONE;
exports._negOne   = L.NEG_ONE;
exports._maxValue = L.MAX_VALUE;
exports._minValue = L.MIN_VALUE;

// Utilities
exports._fromBits = L.fromBits;
exports.fromInt = L.fromInt;
exports.fromNumber = L.fromNumber;
exports.fromString = L.fromString;

// Methods
exports._add = function (l) { return l.add; };
exports._and = function (l) { return l.and; };
exports._compare = function (l) { return l.compare; };
exports._divide = function (l) { return l.divide; };
exports._equals = function (l) { return l.equals; };
exports._getHighBits = function (l) { return l.getHighBits(); };
exports._getLowBits = function (l) { return l.getLowBits(); };
exports._getNumBitsAbs = function (l) { return l.getNumBitsAbs(); };
exports._greaterThan = function (l) { return l.greaterThan; };
exports._greaterThanOrEqual = function (l) { return l.greaterThanOrEqual; };
exports._isEven = function (l) { return l.isEven(); };
exports._isNegative = function (l) { return l.isNegative(); };
exports._isOdd = function (l) { return l.isOdd(); };
exports._isPositive = function (l) { return l.isPositive(); };
exports._isZero = function (l) { return l.isZero(); };
exports._lessThan = function (l) { return l.lessThan; };
exports._lessThanOrEqualTo = function (l) { return l.lessThanOrEqualTo; };
exports._modulo = function (l) { return l.modulo; };
exports._multiply = function (l) { return l.multiply; };
exports._negate = function (l) { return l.negate; };
exports._not = function (l) { return l.note; };
exports._notEquals = function (l) { return l.notEquals; };
exports._or = function (l) { return l.or; };
exports._shiftLeft = function (l) { return l.shiftLeft; };
exports._shiftRight = function (l) { return l.shiftRight; };
exports._shiftRightUnsigned = function (l) { return l.shiftRightUnsigned; };
exports._rotateLeft = function (l) { return l.rotateLeft; };
exports._rotateRight = function (l) { return l.rotateRight; };
exports._subtract = function (l) { return l.subtract; };
exports._toBytes = function (l) { return l.toBytes(); };
exports._toInt = function (l) { return l.toInt(); };
exports._toNumber = function (l) { return l.toNumber(); };
exports._toString = function (l) { return l.toString(); };
exports._xor = function (l) { return l.xor; };
