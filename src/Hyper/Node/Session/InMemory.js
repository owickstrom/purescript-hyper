"use strict";

export function generatedSessionID() {
  return String((new Date()).getTime() + Math.random());
}
