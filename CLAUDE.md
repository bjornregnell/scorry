# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

`scorry` is a Scala library for correlation, mean, and other statistics. T
## Expected Structure

When implementing, follow standard Scala/SBT conventions:
- `build.sbt` — project definition and dependencies
- `src/main/scala/` — library source
- `src/test/scala/` — tests (likely ScalaTest or MUnit)

## Dev workflow (two terminals)

```
# Terminal 1 — recompile on save
sbt ~fastOptJS

# Terminal 2 — Vite dev server with hot reload
npm install
npm run dev
```

Vite watches `target/scala-3.8.3/scorry-fastopt.js` and triggers a full page reload whenever SBT recompiles.

## Other SBT commands

```
sbt fastOptJS      # single compile (dev)
sbt fullOptJS      # optimised build (prod) → scorry-opt.js
```
