"use strict";

exports.toHexString = function toHexString (buf) {
    return buf.toString('hex');
};

exports.toUtf8String = function toUtf8String (buf) {
    return buf.toString('utf8');
};

exports.fromHexString = function fromHexString (s) {
    return Buffer.from(s,'hex');
};

exports.fromUtf8String = function fromUtf8String (s) {
    return Buffer.from(s,'utf8');
};
