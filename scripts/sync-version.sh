#!/bin/sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cabal_file="$repo_root/hodatime.cabal"
mode=${1:-}

if [ "$mode" = "--set" ]; then
  version=${2:?Usage: scripts/sync-version.sh --set VERSION}
  case "$version" in
    *[!0-9.]* | .* | *. | *..*)
      echo "Invalid Cabal version: $version" >&2
      exit 1
      ;;
  esac
else

  version_tag=$(git -C "$repo_root" tag --points-at HEAD --sort=-version:refname |
    grep -E '^v[0-9]+\.[0-9]+\.[0-9]+(\.[0-9]+)?$' |
    head -n 1 || true)

  if [ -n "$version_tag" ]; then
    version=${version_tag#v}
  else
    version_tag=$(git -C "$repo_root" describe --tags --abbrev=0 --match 'v[0-9]*' 2>/dev/null || true)
    if [ -z "$version_tag" ]; then
      echo "No reachable vX.Y.Z or vX.Y.Z.W tag found" >&2
      exit 1
    fi

    tagged_version=${version_tag#v}
    old_ifs=$IFS
    IFS=.
    set -- $tagged_version
    IFS=$old_ifs

    case $# in
      3) version="$1.$2.$3.1" ;;
      4) version="$1.$2.$3.$(($4 + 1))" ;;
      *)
        echo "Unsupported version tag: $version_tag" >&2
        exit 1
        ;;
    esac
  fi
fi

current_version=$(awk -F: '/^version:/ { sub(/^[[:space:]]+/, "", $2); print $2; exit }' "$cabal_file")

if [ "$mode" = "--check" ]; then
  if [ "$current_version" != "$version" ]; then
    echo "hodatime.cabal has version $current_version; Git requires $version" >&2
    exit 1
  fi
else
  temporary_file="$cabal_file.tmp"
  awk -v version="$version" '
    /^version:/ && !updated { print "version:        " version; updated = 1; next }
    { print }
  ' "$cabal_file" > "$temporary_file"
  mv "$temporary_file" "$cabal_file"
fi

echo "$version"
