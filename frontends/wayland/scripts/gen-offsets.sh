#!/bin/sh
# Regenerate src/offsets.lisp from the installed wlroots 0.19 headers.
#
# The wayland-protocols package does not install xdg-shell-protocol.h, which
# wlroots' xdg-shell header includes.  Scan it into a temporary include
# directory just for this probe.

set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
WAYLAND_DIR=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
BUILD_DIR=${TMPDIR:-/tmp}/lem-wayland-offsets-$$
trap 'rm -rf "$BUILD_DIR"' EXIT HUP INT TERM
mkdir -p "$BUILD_DIR"

PROTO_DIR=$(pkg-config --variable=pkgdatadir wayland-protocols 2>/dev/null || true)
if [ -z "$PROTO_DIR" ]; then
    PROTO_DIR=/usr/share/wayland-protocols
fi

wayland-scanner client-header \
    "$PROTO_DIR/stable/xdg-shell/xdg-shell.xml" \
    "$BUILD_DIR/xdg-shell-protocol.h"

# shellcheck disable=SC2046
cc -std=gnu11 -O0 -Wno-unused-value \
    $(pkg-config --cflags wlroots-0.19) \
    -I"$BUILD_DIR" \
    "$SCRIPT_DIR/gen-offsets.c" \
    -o "$BUILD_DIR/gen-offsets"

"$BUILD_DIR/gen-offsets" > "$WAYLAND_DIR/src/offsets.lisp"
