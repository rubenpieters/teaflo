"use strict";

exports.unsafeEmit=function(data) {
  return function(socket) {
    return function() {
      socket.send(data);
    };
  };
};
