#!/usr/bin/env bash

set -euo pipefail;

BOLD='\033[1m'
RED='\033[0;31m'
RESET='\033[0m'

get-timestamp() {
  date '+%Y-%m-%d %H:%M:%S%:z'
}

log-error() {
  if [ -z "$1" ]; then
    log-error "log-error was called without a message."
    return 1
  fi

  echo -e "${RED}[ERROR][$(get-timestamp)] $1${RESET}" >&2
}

log-info() {
  if [ -z "$1" ]; then
    log-error "log-info was called without a message."
    return 1
  fi
  
  echo -e "${BOLD}[INFO][$(get-timestamp)] $1${RESET}"
}
