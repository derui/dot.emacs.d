# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a personal Emacs configuration repository using a literate programming approach with org-mode. The configuration is written in `init.org` and `early-init.org` files and tangled to produce the actual Emacs Lisp files.

## Development Commands

The repository uses [just](https://just.systems/) as the task runner. Key commands:

- `just tangle` - Tangle org files to generate `init.el` and `early-init.el` from their `.org` sources
- `just emacs-test` - Clean install and test the configuration 
- `just emacs-clean` - Remove the Emacs config directory entirely
- `just help` - Show available commands

## Architecture

### Literate Configuration
- **`init.org`** - Main configuration file written in org-mode with Emacs Lisp code blocks
- **`early-init.org`** - Early initialization configuration 
- **`init.el`** and **`early-init.el`** - Generated from org files via tangling (do not edit directly)

### Package Management
- Uses **elpaca** as the package manager for maximum reproducibility
- Custom package loading utilities defined in init.org:
  - `my:with-elpaca-package` macro for package installation and configuration
  - Custom startup queue system with high/low priority delayed loading

### Performance Optimization
- Startup time measurement built-in (target ~150ms)
- Custom startup queue system for deferred package loading
- Early initialization optimizations in `early-init.org`

### Key Components
- **multistate** - Custom modal editing system (similar to vim modes)
- **Japanese input** - SKK configuration in `init-ddskk.el` 
- **Cross-platform** - Supports both Linux and macOS with platform-specific configurations

### File Structure
- `skk/` - Japanese input method dictionary files
- `templates` - Template files
- `elpaca.lock` - Package lock file for reproducible builds
- `bookmarks` - Emacs bookmarks file

## Important Notes

- Configuration targets early Emacs versions (e.g., 31.0.50)
- The configuration should be cloned to `~/.config/emacs`
- Uses `$XDG_CONFIG_DIR/emacs-local` for user-specific state
- Do not compile `init.el` as it's not recommended and only affects launch time
- Always run `just tangle` after modifying `.org` files to regenerate `.el` files