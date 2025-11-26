SRC = src/sources.mlb
OUT = sources.lua

# Default target
all: build

# Build step
build:
	lunarml compile --luajit --lib --output=$(OUT) $(SRC)

# Run with LÖVE
run: build
	love .

# Clean generated files
clean:
	rm -f $(OUT)
