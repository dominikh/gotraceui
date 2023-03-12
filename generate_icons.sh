#!/bin/sh

# The logo was drawn in 32x32, scaling without interpolation to smaller sizes looks terrible. If the DM wants to scale
# with interpolation, it can do so on its own. Thus we only generate sizes larger than 32x32.
for size in 32x32 48x48 64x64 72x72 96x96 128x128 192x192 256x256 512x512 1024x1024; do
	mkdir -p share/icons/hicolor/$size/apps
	convert images/icon.png -sample $sizex$size share/icons/hicolor/$size/apps/gotraceui.png
done
