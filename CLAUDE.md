# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

FreeType is a freely available software library to render fonts, written in C and designed to be small, efficient, highly customizable, and portable while producing high-quality output of most vector and bitmap font formats.

## Current Focus

- **Primary Goal**: Develop a Fortran port of the FreeType library
- Work will be conducted in the `fortran/` subdirectory
- Existing C implementation serves as reference and validation source
- Refer to `fortran/README.md` and `fortran/TODO.md` for specific implementation guidelines

## Build Commands

### Quick Build (GNU Make - Primary Build System)
```bash
# For git repositories, first run:
./autogen.sh

# Standard build:
./configure
make

# Debug build with logging:
./configure CFLAGS="-DFT_DEBUG_LEVEL_TRACE -DFT_DEBUG_LEVEL_ERROR -DFT_DEBUG_LOGGING"
make
```

### Alternative Build Systems

**CMake:**
```bash
cmake -B build -D CMAKE_BUILD_TYPE=Release
cmake --build build
```

**Meson (Recommended for testing):**
```bash
meson setup build --fatal-meson-warnings
meson compile -C build
```

## Test Commands

```bash
# Download test fonts (required before running tests):
python3 tests/scripts/download-test-fonts.py

# Build with tests enabled:
meson setup out -Dtests=enabled
meson compile -C out

# Run all tests:
meson test -C out

# Run tests with verbose output:
meson test -C out -v

# Run specific test suite:
meson test -C out --suite regression
```

## Code Style

FreeType uses clang-format with specific settings (.clang-format file):
```bash
clang-format -i <file.c>
```

Key style points:
- 80 column limit
- Allman brace style
- Aligned declarations and assignments
- Spaces in parentheses

## Architecture Overview

FreeType follows a modular, layered architecture:

### Core Structure (`/src/`)

**Font Format Modules** - Each format is a self-contained module:
- `truetype/` - TrueType fonts (.ttf)
- `type1/` - PostScript Type 1 fonts
- `cff/` - CFF/OpenType fonts
- `sfnt/` - Shared SFNT table support for TrueType/OpenType

**Base Layer** (`base/`):
- Core functionality: memory management, I/O, math operations
- Platform abstractions
- Public API implementation

**Rendering Pipeline**:
1. Font drivers parse font files
2. `autofit/` or format-specific hinters process outlines
3. `smooth/` (anti-aliased) or `raster/` (monochrome) renders glyphs
4. `cache/` optionally stores processed glyphs

**Module System**:
- Configured via `modules.cfg`
- Each module has: headers, implementation, error codes, build rules
- Modules register themselves with the core library

### Key Design Patterns

1. **Object-Oriented C**: Uses structs with function pointers for polymorphism
2. **Service Framework**: Modules provide services accessed through interfaces
3. **Error Handling**: Consistent error code system across all modules
4. **Memory Management**: Custom allocators with debugging support

### Common Development Tasks

**Adding a new feature to existing module:**
1. Check module structure in `src/<module>/`
2. Follow existing patterns for error codes, function naming
3. Update module's `rules.mk` if adding new files
4. Test with relevant font files

**Debugging:**
```bash
# Enable trace output:
export FT2_DEBUG="any:7 memory:5"

# Log to file:
export FT_LOGGING_FILE="/tmp/freetype2.log"
```

**Working with specific font formats:**
- TrueType: Focus on `src/truetype/` and `src/sfnt/`
- OpenType: Also involves `src/cff/` for CFF outlines
- Type 1: Check `src/type1/` and `src/psaux/`

### Important Files

- `include/freetype/config/ftoption.h` - Compile-time configuration
- `modules.cfg` - Controls which modules are built
- Public API headers in `include/freetype/`
- Module interfaces in `include/freetype/internal/`