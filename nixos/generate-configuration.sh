#!/usr/bin/env bash

# Hardlink the files for a host in hosts/ as local machine configuration

HOST="`hostname`"
if [[ ! -z "$1" ]]; then
  HOST="$1"
fi

if [[ ! -f configuration.nix ]]; then
  if [[ ! -e $HOST/"configuration.nix" ]] || [[ ! -e $HOST/"hardware-configuration.nix" ]]; then
    echo "missing $HOST/configuration.nix or $HOST/hardware-configuration.nix" >&2
    exit 1
  fi

  echo "Activating host $HOST."
  echo "{ ... }: { imports = [ ./common.nix ./$HOST/configuration.nix ]; }" > configuration.nix
fi
