# Copilot Instructions for erlisP

## Project Overview

**erlisP** is an Erlang-based Lisp interpreter and transpiler. It implements a Lisp-to-Erlang compiler that:
- Scans Lisp source code (`.elisp` files)
- Parses s-expressions into abstract syntax trees
- Transpiles Lisp to Erlang AST
- Compiles to BEAM bytecode
- Provides macro support, local function definitions, and pattern matching

The project is an OTP application with a REPL interface.

## Build & Test Commands

### Build
```bash
./rebar3 compile
```
Compiles all Erlang and Lisp source files to BEAM bytecode.

### Run All Tests
```bash
./rebar3 eunit
```
Executes 190+ EUnit tests across the codebase.

### Run Single Test Module
```bash
./rebar3 eunit -m module_name
```
Example: `./rebar3 eunit -m interprete_test` runs only interprete_test.erl

### Run Specific Test
```bash
./rebar3 eunit -m module_name:test_name_test
```
Example: `./rebar3 eunit -m interprete_test:local_fun_test` runs a single test

### Check Static Analysis (Dialyzer)
```bash
./rebar3 dialyzer
```
Currently has version compatibility issues with OTP 28+; mostly informational.

### Generate Documentation
```bash
./rebar3 edoc
```
Generates HTML documentation in the `edoc/` directory.

### Clean Build Artifacts
```bash
./rebar3 clean
```
Removes compiled `.beam` files and build directories.

## Architecture

### Pipeline Flow
1. **Lexer** (`els_scan.erl`) - Converts Lisp source to tokens using a leex scanner
2. **Parser** (`els_parser.erl`) - Builds S-expression AST from tokens using yecc parser
3. **Transpiler** (`els_transpile.erl`) - Converts Lisp forms to Erlang AST
4. **Compiler** (`els_compile.erl`) - Compiles Erlang AST to BEAM bytecode
5. **REPL** (`els_repl.erl`) - Interactive evaluation environment

### Core Components

| Module | Purpose |
|--------|---------|
| `els_scan.xrl` | Leex lexer definition (generates `els_scan.erl`) |
| `els_parser.erl` | Yecc parser; builds s-expressions |
| `els_transpile.erl` | Lisp→Erlang AST transformation (largest module, 52KB) |
| `els_compile.erl` | File compilation and module loading |
| `els_repl.erl` | Interactive evaluation with environment binding |
| `els_localfun.erl` | Local function definition and registration |
| `els_macro.erl` | Macro expansion support |
| `els_typespec.erl` | Type specification parsing and handling |
| `els_util.erl` | Utility functions (AST manipulation, etc.) |
| `els_item.erl` | Item representation (location, type, value) |
| `els_erlformat.erl` | Erlang syntax formatting (large module, 37KB) |

### Data Structures (from `els.hrl`)
```erlang
-record(item, {value :: atom(), loc :: location(), type :: atype()}).
-record(compile_info, {loc :: location(), msg :: string, detail :: any()}).
-record(compile_result, {ok :: list(), warning :: list(), error :: list()}).
```

### Key Entry Points
- **`els.erl`** - Escript CLI interface; handles compilation and REPL commands
- **`els_app.erl`** - OTP application callback
- **`els_sup.erl`** - Supervisor (minimal functionality)

## Key Conventions

### Test Organization
- **Location**: `test/` directory
- **Naming**: `*_test.erl` (e.g., `interprete_test.erl`, `scan_test.erl`)
- **Framework**: EUnit (`-include_lib("eunit/include/eunit.hrl")`)
- **Test Data**: `.elisp` source files in `test/testdata/`
- **Syntax**: Function names ending in `_test` are auto-discovered as test cases

### Lisp Dialect Specifics
- **S-expressions**: Parsed as nested Erlang lists `[Head | Tail]`
- **Special Forms**: `defun`, `defmacro`, `if`, `let`, `cond`
- **Pattern Matching**: Supported via `match` forms
- **Macros**: Expanded during transpilation via `els_macro.erl`
- **Local Functions**: Registered and evaluated via `els_localfun.erl`

### Erlang/Lisp Type Mapping
Lisp forms transpile to Erlang AST (`erl_syntax` module):
- Lisp atoms → Erlang atoms
- Lisp lists → Erlang function calls or data lists
- Lisp numbers → Erlang integers/floats
- Lisp strings → Erlang strings/binaries

### Environment & Binding
- **Environment** (type `env()`): List of variable bindings and macro definitions
- **Options** (type `options()`): Configuration passed through transpilation pipeline
- Binding merged incrementally during nested form evaluation

### File Extensions
- `.elisp` - Lisp source files (user-facing)
- `.erl` - Erlang source files (generated or hand-written)
- `.xrl` - Leex lexer definitions (generates `.erl`)
- `.yrl` - Yecc parser definitions (generates `.erl`)
- `.beam` - Compiled BEAM bytecode

### Backups & Temporaries
- `~` suffix files are backup/temporary (e.g., `foo.erl~`, `foo.elisp~`) - safe to ignore
- Generated files: `_build/` directory (build artifacts)

## Dependency Notes

- **No External Dependencies**: `rebar.lock` is empty (`[]`)
- **Built-in Libraries Used**:
  - `syntax_tools` - AST manipulation (`erl_syntax`, `merl`)
  - `eunit` - Testing framework
  - `kernel` - Core Erlang (logger, file I/O)
- **OTP Requirement**: OTP 26+ (requires `TOKEN_LOC` meta variable in leex)

## Common Workflows

### Debugging a Failing Test
```bash
./rebar3 eunit -m test_module:test_name_test
# Add io:format/2 calls in test code for inspection
```

### Adding a New Test
1. Create `test/new_feature_test.erl` with EUnit headers
2. Define test functions ending in `_test`
3. Run `./rebar3 eunit -m new_feature_test`

### Modifying the Lexer/Parser
- **Lexer changes**: Edit `src/els_scan.xrl`, run `./rebar3 compile` (auto-generates `els_scan.erl`)
- **Parser changes**: Edit `src/els_parser.erl` or `src/els_erlformat.erl`
- Verify with: `./rebar3 eunit -m scan_test` or `./rebar3 eunit -m feature_test`

### Transpilation Debugging
- Check `els_transpile.erl` for Lisp→Erlang conversion logic
- Inspect `els_erlformat.erl` for AST formatting to Erlang code
- Use `els_pp.erl` for pretty-printing (has test utilities)

### Adding a New Lisp Feature
1. Update lexer if new tokens needed (`els_scan.xrl`)
2. Update parser if new syntax (`els_parser.erl`)
3. Update transpiler to handle new forms (`els_transpile.erl`)
4. Add tests in `test/*_test.erl`
5. Test end-to-end with sample `.elisp` file
