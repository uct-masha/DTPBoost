# Contributing to DTP Boost

Thank you for your interest in contributing to DTP Boost! This document outlines our development workflow and contribution guidelines.

## Git Flow Branching Strategy

We use a Git Flow branching model to manage development and releases:

### Branch Structure

- **`main`**: Production-ready code. This branch is protected and should never be pushed to directly.
- **`dev`**: Main development branch. All feature branches are created from and merged back into `dev`.
- **Feature branches**: Created from `dev` for individual features or bug fixes.

### Workflow

1. **Start a new feature**:
   ```bash
   git checkout dev
   git pull origin dev
   git checkout -b feature/your-feature-name
   ```

2. **Work on your feature**:
   - Make your changes
   - Commit regularly with clear, descriptive messages
   - Follow the commit message conventions below

3. **Submit your changes**:
   ```bash
   git push origin feature/your-feature-name
   ```
   - Create a Pull Request from your feature branch to `dev`
   - Use the PR template and provide a clear description

4. **Code review and merge**:
   - Wait for code review and approval
   - Address any feedback
   - Once approved, the feature branch will be merged into `dev`

5. **Release process**:
   - When ready for release, create a PR from `dev` to `main`
   - After thorough testing, the changes are merged into `main`

### Branch Naming Conventions

- Feature branches: `feature/descriptive-name`
- Bug fixes: `bugfix/issue-description`
- Hotfixes: `hotfix/critical-issue`
- Documentation: `docs/what-you-are-documenting`

### Commit Message Guidelines

Use clear, descriptive commit messages:

```
type(scope): brief description

Longer description if needed explaining what and why.

Fixes #123
```

**Types**:
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `style`: Code style changes (formatting, etc.)
- `refactor`: Code refactoring
- `test`: Adding or updating tests
- `chore`: Build process or auxiliary tool changes

## Development Setup

### Prerequisites

- **R** (version 4.0 or higher)
- **RStudio** (recommended for development)

### Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/uct-masha/DTPBoost.git
   cd DTPBoost
   ```

2. Install required R packages (see README.md for details)

3. Open `DTPBoost.Rproj` in RStudio

### Testing

- Run test scripts in the `testScripts/` directory
- Ensure the Shiny app runs without errors locally

## Pull Request Process

1. Ensure your branch is up to date with `dev`
2. Fill out the PR template completely
3. Link any related issues
4. Ensure all tests pass
5. Request review from maintainers

## Code Style

- Follow existing R coding conventions in the project
- Use meaningful variable and function names
- Comment complex logic
- Keep functions focused and concise

## Reporting Issues

- Use the provided issue templates
- Provide clear reproduction steps for bugs
- Include relevant system information

## Questions?

If you have questions about contributing, please open an issue or contact the maintainers.

---

Thank you for contributing to DTP Boost!