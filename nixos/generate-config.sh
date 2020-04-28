#!/usr/bin/env bash

# Hardlink the files for a host in hosts/ as local machine configuration

HOST="`hostname`"
if [[ ! -z "$1" ]]; then
  HOST="$1"
fi

if [[ ! -f wifi.nix ]]; then
  echo "Wifi configuration not found, generating template."
  cat > wifi.nix <<EOF
{ config, pkgs, ... }:

{
  networking.wireless = {
    enable = true;
    # Add wifi network keys here
    networks = {
      # the-gibson.psk = "hunter2";
    };
  };
}
EOF
fi

if [[ ! -f configuration.nix ]]; then
  if [[ ! -e "$HOST.nix" ]] || [[ ! -e "hardware-configuration/$HOST.nix" ]]; then
    echo "missing $HOST.nix or hardware-configuration/$HOST.nix" >&2
    exit 1
  fi

  echo "Activating host $HOST."
  echo "{ ... }: { imports = [ ./$HOST.nix ]; }" > configuration.nix
fi
