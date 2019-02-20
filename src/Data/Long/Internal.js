exports.numberBitsToInt = function(x) {
  return x|0;
};

var Long = require("long");

var radixCheckers = {};

function RadixChecker(radix) {
  var digits;
  if (radix < 11) {
    digits = "[0-" + (radix - 1).toString() + "]";
  } else if (radix === 11) {
    digits = "[0-9a]";
  } else {
    digits = "[0-9a-" + String.fromCharCode(86 + radix) + "]";
  }

  this.baseRE = new RegExp("^" + digits + "+$", "i");

  this.maxNegSignedBase = Long.MIN_VALUE.toString(radix).substring(1);
  this.maxPosSignedBase = Long.MAX_VALUE.toString(radix);
  this.maxUnsignedBase = Long.MAX_UNSIGNED_VALUE.toString(radix);
}

function hasValidDigits(isNegative, isUnsigned, base, radix) {
  radixCheckers[radix] = radixCheckers[radix] || new RadixChecker(radix);
  var rc = radixCheckers[radix];

  var maxBase;
  if (isUnsigned) {
    maxBase = rc.maxUnsignedBase;
  } else if (isNegative) {
    maxBase = rc.maxNegSignedBase;
  } else {
    maxBase = rc.maxPosSignedBase;
  }

  return ((base.length < maxBase.length)
          || (base.length === maxBase.length && base <= maxBase)
         ) && rc.baseRE.test(base);
}

exports._safeReadLong = function(s, isUnsigned, radix) {
  var isNegative = s.startsWith("-");
  var hasPositivePrefix = s.startsWith("+");

  var base = (isNegative || hasPositivePrefix) ? s.substring(1) : s;

  // remove preceding zeros
  var lastNdx = 0;
  while (lastNdx < base.length - 1 && base[lastNdx] === "0") { lastNdx++; }
  base = base.substring(lastNdx);

  var signPrefix = isNegative ? "-" : "";

  if (base === "") {
    // long.js return 0 for empty string
    throw "Invalid long string - empty";
  } else if (isNegative && isUnsigned && base !== "0") {
    // long.js coerces negative values to their unsigned counterparts
    throw "Invalid long string - negative for unsigned";
  } else if (!hasValidDigits(isNegative, isUnsigned, base, radix)) {
    throw "Invalid long string - found invalid characters";
  } else {
    return Long.fromString(signPrefix + base, isUnsigned, radix);
  }
};
