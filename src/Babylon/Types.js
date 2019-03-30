'use strict';

var parser = require('@babel/parser');

// (String, Options | null) -> Foreign
exports._parse = parser.parse;

// (String, Options | null) -> Foreign
exports._parseExpression = parser.parseExpression;
