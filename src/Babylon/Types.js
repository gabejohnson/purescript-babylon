"use strict";

var babylon = require('babylon');

// (String, Options | null) -> AST
exports._parseExpression = babylon.parseExpression;

