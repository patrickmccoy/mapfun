#!/usr/bin/env bash
# Download an OpenStreetMap extract into ./data.
#
#   ./scripts/fetch-osm.sh                 # default: Rhode Island (~30 MB)
#   ./scripts/fetch-osm.sh monaco          # tiny (~1 MB), good for a first run
#   ./scripts/fetch-osm.sh <geofabrik-url> # anything from download.geofabrik.de
#
# Start small. A US state extract is a few million nodes, which is plenty to
# make the difference between Dijkstra and A* obvious, and small enough that a
# full import still finishes in under a minute.
set -euo pipefail

cd "$(dirname "$0")/.."
mkdir -p data

case "${1:-rhode-island}" in
  monaco)       URL="https://download.geofabrik.de/europe/monaco-latest.osm.pbf" ;;
  rhode-island) URL="https://download.geofabrik.de/north-america/us/rhode-island-latest.osm.pbf" ;;
  delaware)     URL="https://download.geofabrik.de/north-america/us/delaware-latest.osm.pbf" ;;
  vermont)      URL="https://download.geofabrik.de/north-america/us/vermont-latest.osm.pbf" ;;
  massachusetts) URL="https://download.geofabrik.de/north-america/us/massachusetts-latest.osm.pbf" ;;
  http*)        URL="$1" ;;
  *)
    echo "Unknown extract '$1'." >&2
    echo "Use monaco | rhode-island | delaware | vermont | massachusetts, or a full Geofabrik URL." >&2
    exit 1
    ;;
esac

DEST="data/$(basename "$URL")"
echo "Fetching $URL"
curl -fL --progress-bar -o "$DEST" "$URL"
ls -lh "$DEST"
echo
echo "Now try:"
echo "  sbt \"cli/run import --pbf $DEST\""
