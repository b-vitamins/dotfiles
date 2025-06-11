# Agent Development Guide

This repository does not currently define agent instructions. This file documents
standards for commit messages, pull requests, and repository maintenance.

## Commit Message Standards
- Use the **Conventional Commits** style: `type(scope): summary`.
- Common types are `feat`, `fix`, `docs`, `chore` and `refactor`.
- Scope should be a directory or component name when appropriate.
- The body should explain what and why, wrapped at 72 characters.

## Commit Sequencing
- Keep commits atomic; unrelated changes must be in separate commits.
- Order commits logically: documentation updates before code changes when both
  occur in a series.

## Pull Request Standards
- Title: short summary using imperative mood.
- Description should include a summary, testing steps, and references to any
  relevant issues.

## Housekeeping
- Regularly update package dependencies.
- Remove unused files and code as discovered.
- Record notable changes in `CHANGELOG.md` under the **Unreleased** section.

## Versioning
- Follow [Semantic Versioning](https://semver.org) using `MAJOR.MINOR.PATCH`.
- Tag releases with `v<version>` once CHANGELOG entries are complete.

## Documentation
- Update `README.md` when user-facing behavior changes.
- Document new scripts or important configuration in the repository.

