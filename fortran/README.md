# FreeType Fortran Port

This directory contains the Fortran port of the FreeType library.

## Build System

This project uses the Fortran Package Manager (fpm) for building and testing.

**Important**: Always work from the `fortran/` directory as your current working directory when running fpm commands.

## Build Commands

```bash
# Change to the fortran directory first
cd fortran/

# Build the project
fpm build

# Build with release optimizations
fpm build --profile release

# Build with debug flags
fpm build --profile debug
```

## Test Commands

```bash
# Run all tests
fpm test

# Run tests with verbose output
fpm test --verbose

# Run a specific test
fpm test test_name
```

## Development Workflow

1. Always ensure you're in the `fortran/` directory before running any fpm commands
2. Run tests frequently to ensure bit-perfect compatibility with C FreeType
3. Use the validation framework to compare outputs between C and Fortran implementations

## Project Structure

- `src/` - Fortran source code
- `test/` - Test suites (unit, integration, validation)
- `app/` - Example applications and validation tools
- `fpm.toml` - Package manifest and dependencies

For detailed development plans and architecture, see `TODO.md`.
