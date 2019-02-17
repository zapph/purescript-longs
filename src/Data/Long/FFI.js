var L = require("long");

// Constants
exports.zero     = L.ZERO;
exports.one      = L.ONE;
exports.negOne   = L.NEG_ONE;
exports.uzero    = L.UZERO;
exports.uone     = L.UONE;
exports.maxValue = L.MAX_VALUE;
exports.maxUnsignedValue = L.MAX_UNSIGNED_VALUE;
exports.minValue = L.MIN_VALUE;

// Utilities
exports.isLong = L.isLong;
exports.fromBits = L.fromBits;
exports.fromBytes = L.fromBytes;
exports.fromBytesLE = L.fromBytesLE;
exports.fromBytesBE = L.fromBytesBE;
exports.fromInt = L.fromInt;
exports.fromNumber = L.fromNumber;
exports.fromString = L.fromString;
exports.fromValue = L.fromValue;

// Methods
exports.add = function (l) { return l.add; };
exports.and = function (l) { return l.and; };
exports.compare = function (l) { return l.compare; };
exports.divide = function (l) { return l.divide; };
exports.equals = function (l) { return l.equals; };
exports.getHighBits = function (l) { return l.getHighBits(); };
exports.getHighBitsUnsigned = function (l) { return l.getHighBitsUnsigned(); };
exports.getLowBits = function (l) { return l.getLowBits(); };
exports.getLowBitsUnsigned = function (l) { return l.getLowBitsUnsigned(); };
exports.getNumBitsAbs = function (l) { return l.getNumBitsAbs(); };
exports.greaterThan = function (l) { return l.greaterThan; };
exports.greaterThanOrEqual = function (l) { return l.greaterThanOrEqual; };
exports.isEven = function (l) { return l.isEven(); };
exports.isNegative = function (l) { return l.isNegative(); };
exports.isOdd = function (l) { return l.isOdd(); };
exports.isPositive = function (l) { return l.isPositive(); };
exports.isZero = function (l) { return l.isZero(); };
exports.lessThan = function (l) { return l.lessThan; };
exports.lessThanOrEqualTo = function (l) { return l.lessThanOrEqualTo; };
exports.modulo = function (l) { return l.modulo; };
exports.multiply = function (l) { return l.multiply; };
exports.negate = function (l) { return l.negate(); };
exports.not = function (l) { return l.not(); };
exports.notEquals = function (l) { return l.notEquals; };
exports.or = function (l) { return l.or; };
exports.shiftLeft = function (l) { return l.shiftLeft; };
exports.shiftRight = function (l) { return l.shiftRight; };
exports.shiftRightUnsigned = function (l) { return l.shiftRightUnsigned; };
exports.rotateLeft = function (l) { return l.rotateLeft; };
exports.rotateRight = function (l) { return l.rotateRight; };
exports.subtract = function (l) { return l.subtract; };
exports.toBytes = function (l) { return l.toBytes(); };
exports.toInt = function (l) { return l.toInt(); };
exports.toNumber = function (l) { return l.toNumber(); };
exports.toString = function (l) { return l.toString; };
exports.toUnsigned = function (l) { return l.toUnsigned(); };
exports.xor = function (l) { return l.xor; };
