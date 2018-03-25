"use strict";

var babylon = require('babylon');

// (String, Options | null) -> Foreign
exports._parse = babylon.parse;

// (String, Options | null) -> Foreign
exports._parseExpression = babylon.parseExpression;

