/* global exports */
"use strict";

// module Routing.Bob

exports.encodeURIComponent = encodeURIComponent;
exports.decodeURIComponent = decodeURIComponent;

exports.camelsToHyphens = function(s) {
  return s.replace(/([A-Z])/g, function($1, _, i){
    if(i > 0) {
      return "-"+$1.toLowerCase();
    } else {
      return $1.toLowerCase();
    }
  });
};
