# Git Flow Workflow Example

This document provides a practical example of the Git Flow workflow for DTP Boost development.

## Scenario: Adding a New Feature

Let's walk through adding a new feature called "vaccination cost calculator".

### Step 1: Set up your local environment

```bash
# Clone the repository (if you haven't already)
git clone https://github.com/uct-masha/DTPBoost.git
cd DTPBoost

# Make sure you're on the dev branch
git checkout dev
git pull origin dev
```

### Step 2: Create a feature branch

```bash
# Create and switch to your feature branch
git checkout -b feature/vaccination-cost-calculator

# Verify you're on the right branch
git branch
```

### Step 3: Develop your feature

```r
# Make your changes to R files
# For example, add new functions to calculate vaccination costs
# Edit app.R, add new modules, update documentation, etc.
```

### Step 4: Commit your changes

```bash
# Stage your changes
git add .

# Commit with a descriptive message
git commit -m "feat(costs): add vaccination cost calculator module

- Add new cost calculation functions
- Create UI components for cost inputs
- Add validation for cost parameters
- Update documentation

Fixes #45"
```

### Step 5: Push and create pull request

```bash
# Push your feature branch
git push origin feature/vaccination-cost-calculator
```

Then on GitHub:
1. Navigate to the repository
2. Click "Compare & pull request"
3. **Ensure the base branch is `dev`** (not `main`)
4. Fill out the PR template
5. Link related issues
6. Request reviewers

### Step 6: Code review process

1. Reviewers examine your code
2. Address any feedback by making additional commits
3. Push updates to the same branch
4. Once approved, maintainers merge to `dev`

### Step 7: Clean up

```bash
# Switch back to dev and pull the latest changes
git checkout dev
git pull origin dev

# Delete your local feature branch (optional)
git branch -d feature/vaccination-cost-calculator
```

## Release Process (Maintainers Only)

When `dev` has accumulated enough features for a release:

### Step 1: Prepare release

```bash
# Make sure dev is up to date
git checkout dev
git pull origin dev

# Run tests and quality checks
# Update version numbers, CHANGELOG, etc.
```

### Step 2: Create release PR

```bash
# Create PR from dev to main
# On GitHub: Create pull request from dev → main
```

### Step 3: Final review and merge

1. Thorough review of all changes since last release
2. Run comprehensive tests
3. Get required approvals (typically 2+ reviewers)
4. Merge to `main`

### Step 4: Tag the release

```bash
git checkout main
git pull origin main
git tag -a v1.2.0 -m "Release version 1.2.0"
git push origin v1.2.0
```

## Common Commands Cheat Sheet

```bash
# Start new feature
git checkout dev && git pull origin dev
git checkout -b feature/my-feature

# Update feature branch with latest dev
git checkout dev && git pull origin dev
git checkout feature/my-feature
git merge dev

# Push feature branch
git push origin feature/my-feature

# Clean up after merge
git checkout dev && git pull origin dev
git branch -d feature/my-feature
git remote prune origin
```

## Tips for Success

1. **Keep feature branches small and focused**
2. **Commit often with clear messages**
3. **Sync with dev regularly** to avoid conflicts
4. **Test your changes locally** before pushing
5. **Fill out PR templates completely**
6. **Respond promptly to review feedback**

## Getting Help

- Check [CONTRIBUTING.md](../CONTRIBUTING.md) for detailed guidelines
- Ask questions in issues or discussions
- Contact maintainers if you're stuck