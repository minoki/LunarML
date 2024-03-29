#!/usr/bin/env sh
echo '(* -*- mode: sml; mode: read-only -*- *)' > src/version.sml
echo '(* This file was generated by set-version.sh *)' >> src/version.sml
echo "structure LunarMLVersion = struct" >> src/version.sml
echo '  val version = "'"$1"'"' >> src/version.sml
echo 'end;' >> src/version.sml
sed -i.bak -E 's/"version": "[^"]*"/"version": "'"$1"'"/' package/npm/package.json
echo "VERSION = $1" > version.mk
