# GitHub Configuration Files

This directory contains configuration files and documentation for managing the DTP Boost repository using Git Flow workflow.

## Files Overview

### Templates
- **`pull_request_template.md`** - Standard template for all pull requests
- **`ISSUE_TEMPLATE/`** - Issue templates for bugs, features, and configuration

### Documentation
- **`BRANCH_PROTECTION_SETUP.md`** - Admin guide for configuring branch protection rules
- **`WORKFLOW_EXAMPLE.md`** - Practical examples of the Git Flow workflow

## Next Steps for Repository Administrators

To complete the Git Flow implementation, repository administrators need to:

1. **Create the `dev` branch** from `main`:
   ```bash
   git checkout main
   git checkout -b dev
   git push origin dev
   ```

2. **Set `dev` as the default branch** in GitHub Settings → Branches

3. **Configure branch protection rules** following the guide in `BRANCH_PROTECTION_SETUP.md`

4. **Notify all contributors** about the new workflow and point them to `CONTRIBUTING.md`

5. **Update any existing open PRs** to target `dev` instead of `main`

## Benefits of This Setup

- **Organized development** with clear separation between stable (`main`) and development (`dev`) code
- **Consistent contribution process** with standardized PR and issue templates
- **Reduced risk** of breaking production code through branch protection
- **Better collaboration** with clear guidelines and examples

For questions about these configurations, please refer to the documentation files or contact the repository maintainers.