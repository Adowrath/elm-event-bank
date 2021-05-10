# Changelog for event-bank

## Wishful things

- Hot Module Replacement for the Haskell side

## Unreleased changes

## v0.2.0.0

- Added Reverse Proxy behaviour for the frontend,
  routed through the backend
- Removed CORS Header
- Added relude's default HLint

## v0.1.0.0

- Initial project setup
- Start backend and frontend simultaneously
- End both components at the same time
- React properly to SIGTERM and SIGINT signals
- Commandline arguments for ports and frontend folder
- Create Elm bindings for Haskell types using `elm-street`
- Frontend using Hot Reload to adjust to newly generated bindings
