# Branch Protection Setup Guide

This document provides instructions for repository administrators to set up branch protection rules that enforce the Git Flow workflow.

## Overview

The DTP Boost repository uses Git Flow with the following branch structure:
- `main`: Production-ready code (protected)
- `dev`: Main development branch (protected with different rules)
- Feature branches: Created from `dev` for individual features

## Required Setup Steps

### 1. Create the `dev` Branch

First, create the `dev` branch from the current `main` branch:

```bash
git checkout main
git pull origin main
git checkout -b dev
git push origin dev
```

### 2. Set Default Branch

In GitHub repository settings:
1. Go to Settings → Branches
2. Change the default branch from `main` to `dev`
3. This ensures new clones start with `dev` and PRs default to `dev`

### 3. Configure Branch Protection Rules

#### For `main` branch:

Go to Settings → Branches → Add rule:

- **Branch name pattern**: `main`
- **Protect matching branches**:
  - ✅ Require a pull request before merging
    - ✅ Require approvals: 2
    - ✅ Dismiss stale PR approvals when new commits are pushed
    - ✅ Require review from code owners (if CODEOWNERS file exists)
  - ✅ Require status checks to pass before merging
    - ✅ Require branches to be up to date before merging
  - ✅ Require conversation resolution before merging
  - ✅ Restrict pushes that create files with these patterns (add: `*` to block all direct pushes)
  - ✅ Include administrators

#### For `dev` branch:

Go to Settings → Branches → Add rule:

- **Branch name pattern**: `dev`
- **Protect matching branches**:
  - ✅ Require a pull request before merging
    - ✅ Require approvals: 1
    - ✅ Dismiss stale PR approvals when new commits are pushed
  - ✅ Require status checks to pass before merging
    - ✅ Require branches to be up to date before merging
  - ✅ Require conversation resolution before merging
  - ❌ Include administrators (allows admins to push directly if needed for hotfixes)

### 4. Optional: Add Status Checks

If you set up CI/CD workflows, add them as required status checks:
- R CMD check
- Tests
- Linting
- Security scans

### 5. Create CODEOWNERS File (Optional)

Create `.github/CODEOWNERS` to require specific people to review certain changes:

```
# Global owners
* @maintainer1 @maintainer2

# R scripts
*.R @r-expert
*.Rmd @r-expert

# Configuration files
*.yml @devops-lead
*.yaml @devops-lead
```

## Migration Strategy

### For Existing Development

If there are existing feature branches or open PRs:

1. **Update existing PRs**: Change target branch from `main` to `dev`
2. **Notify contributors**: Inform them about the new workflow
3. **Update local development environments**:
   ```bash
   git checkout main
   git pull origin main
   git checkout -b dev origin/dev
   git branch --set-upstream-to=origin/dev dev
   ```

### Communication

Send a notification to all contributors about:
- The new Git Flow workflow
- Updated contributing guidelines
- Changed default branch
- Need to update local repositories

## Testing the Setup

1. Try to push directly to `main` (should be blocked)
2. Create a test feature branch from `dev`
3. Create a PR from feature branch to `dev` (should work)
4. Create a PR from `dev` to `main` (should work with proper approvals)

## Troubleshooting

### Common Issues

1. **Contributors pushing to main**: Remind them about the new workflow
2. **PRs targeting wrong branch**: GitHub should default to `dev` after setup
3. **Local branches out of sync**: Provide commands for updating local repos

### Emergency Procedures

For critical hotfixes that need to bypass normal workflow:
1. Admins can create hotfix branches from `main`
2. Apply minimal fix
3. Create PR to `main` with expedited review
4. After merge, ensure changes are also applied to `dev`

---

For questions about this setup, please contact the repository maintainers.