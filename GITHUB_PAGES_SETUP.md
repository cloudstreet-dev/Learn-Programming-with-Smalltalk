# GitHub Pages Setup Guide

This document explains how GitHub Pages is configured for **Learn Programming with Smalltalk**.

## What is GitHub Pages?

GitHub Pages allows you to host a static website directly from your GitHub repository. We're using it to create a beautiful, navigable online version of the book.

## Files Added

### 1. `_config.yml`
The Jekyll configuration file that controls:
- Site metadata (title, description, author)
- Theme selection (using Cayman theme)
- Navigation structure
- Plugin configuration
- Build settings

### 2. `index.html`
The homepage/landing page featuring:
- Book introduction
- Quick start links
- Visual feature highlights
- Table of contents link
- Call-to-action buttons

### 3. `assets/css/style.scss`
Custom CSS styling for:
- Better code block formatting for Smalltalk
- Chapter navigation styling
- Improved typography and readability
- Responsive design (mobile-friendly)
- Print styles
- Syntax highlighting

### 4. `.github/workflows/pages.yml`
GitHub Actions workflow that:
- Automatically builds the Jekyll site on every push to main
- Deploys to GitHub Pages
- Handles Ruby and Jekyll dependencies

### 5. `Gemfile`
Ruby gem dependencies:
- github-pages gem
- Jekyll plugins (SEO, sitemap, etc.)
- Platform-specific dependencies

## How to Enable GitHub Pages

Follow these steps to activate GitHub Pages for this repository:

### Step 1: Push Configuration Files

All necessary files have been created. Just push to main:

```bash
git add .
git commit -m "Add GitHub Pages configuration"
git push origin main
```

### Step 2: Enable GitHub Pages in Repository Settings

1. Go to your repository on GitHub: https://github.com/cloudstreet-dev/Learn-Programming-with-Smalltalk
2. Click on **Settings** (top right)
3. Scroll down to **Pages** section (left sidebar)
4. Under **Source**, select:
   - **Source**: GitHub Actions
5. Save changes

### Step 3: Wait for Deployment

- GitHub Actions will automatically build and deploy the site
- Check the **Actions** tab to see the deployment progress
- First deployment typically takes 2-5 minutes
- Subsequent deployments are faster (1-2 minutes)

### Step 4: Access Your Book

Once deployed, your book will be available at:

**https://cloudstreet-dev.github.io/Learn-Programming-with-Smalltalk/**

## How It Works

1. **You push changes** to the main branch
2. **GitHub Actions triggers** (see `.github/workflows/pages.yml`)
3. **Jekyll builds** the site using `_config.yml` and custom styling
4. **Site is deployed** to GitHub Pages
5. **Book is live** at the GitHub Pages URL

## File Structure

```
Learn-Programming-with-Smalltalk/
├── _config.yml              # Jekyll configuration
├── index.html               # Landing page
├── README.md                # GitHub repository readme
├── TABLE_OF_CONTENTS.md     # Full table of contents
├── Gemfile                  # Ruby dependencies
├── .github/
│   └── workflows/
│       └── pages.yml        # Deployment automation
├── assets/
│   └── css/
│       └── style.scss       # Custom styling
├── chapters/
│   ├── chapter-01-*.md     # All 40 chapters
│   └── ...
└── appendices/
    ├── appendix-a-*.md     # All 5 appendices
    └── ...
```

## Customization

### Changing the Theme

Edit `_config.yml` and change:

```yaml
theme: jekyll-theme-cayman
```

To one of:
- `jekyll-theme-minimal`
- `jekyll-theme-architect`
- `jekyll-theme-slate`
- `jekyll-theme-tactile`
- `jekyll-theme-time-machine`

### Adding Custom Styling

Edit `assets/css/style.scss` to modify:
- Colors
- Typography
- Layout
- Responsive breakpoints
- Code block styling

### Modifying the Landing Page

Edit `index.html` to change:
- Welcome message
- Feature highlights
- Call-to-action buttons
- Quick links

### Adding Analytics

To add Google Analytics, edit `_config.yml`:

```yaml
google_analytics: UA-XXXXXXXXX-X
```

## Navigation

Each chapter markdown file should have navigation links at the bottom:

```markdown
[Previous: Chapter X](../chapter-X.md) | [Next: Chapter Y](../chapter-Y.md)
```

These are automatically converted to working links by Jekyll.

## Testing Locally

To test the site on your local machine before pushing:

### Prerequisites

- Ruby 2.5.0 or higher
- Bundler

### Steps

```bash
# Install dependencies
bundle install

# Serve the site locally
bundle exec jekyll serve

# Open browser to:
# http://localhost:4000/Learn-Programming-with-Smalltalk/
```

The site will auto-reload when you make changes.

## Troubleshooting

### Build Fails

1. Check the **Actions** tab on GitHub for error messages
2. Verify `_config.yml` syntax (YAML is whitespace-sensitive)
3. Ensure all markdown files are valid
4. Check that all links point to existing files

### Site Not Updating

1. Clear your browser cache
2. Wait a few minutes (CDN propagation)
3. Check the deployment status in Actions tab
4. Verify the workflow completed successfully

### 404 Errors

1. Verify file paths in `_config.yml` baseurl
2. Check that all linked files exist
3. Ensure relative paths are correct
4. Remember: Jekyll converts `.md` to `.html`

## Features

### Automatic Features (via plugins)

- **SEO tags**: Automatic meta tags for search engines
- **Sitemap**: Auto-generated sitemap.xml
- **Relative links**: Markdown links work correctly
- **Titles from headings**: Page titles from first heading
- **Syntax highlighting**: Code blocks with proper colors

### Custom Features

- **Responsive design**: Mobile-friendly layout
- **Print styles**: Print-optimized CSS
- **Code formatting**: Smalltalk-specific highlighting
- **Navigation**: Previous/Next chapter links
- **Search**: GitHub's built-in site search

## Maintenance

### Updating Content

Just edit the markdown files and push:

```bash
# Make changes to chapter files
git add chapters/chapter-XX-*.md
git commit -m "Update chapter XX"
git push origin main

# Site rebuilds automatically
```

### Adding New Chapters

1. Create new `.md` file in `chapters/`
2. Update `TABLE_OF_CONTENTS.md`
3. Update navigation links in adjacent chapters
4. Push changes

### Monitoring

- **Actions tab**: See deployment status
- **Pages settings**: View published URL and deployment history
- **Insights > Traffic**: See visitor statistics

## Advanced Configuration

### Custom Domain

To use a custom domain (e.g., learnsmalltalk.com):

1. Add a `CNAME` file with your domain
2. Configure DNS with your domain provider
3. Update `url` in `_config.yml`

### Custom Layouts

Create custom layouts in `_layouts/` directory:

```
_layouts/
├── default.html
├── chapter.html
└── appendix.html
```

Reference in markdown frontmatter:

```yaml
---
layout: chapter
title: Chapter 1
---
```

## Resources

- [GitHub Pages Documentation](https://docs.github.com/en/pages)
- [Jekyll Documentation](https://jekyllrb.com/docs/)
- [Jekyll Themes](https://pages.github.com/themes/)
- [Markdown Guide](https://www.markdownguide.org/)

## Support

For issues with GitHub Pages setup:
- [GitHub Pages Issues](https://github.com/cloudstreet-dev/Learn-Programming-with-Smalltalk/issues)
- [GitHub Community](https://github.community/)

---

## Quick Reference

### URLs

- **Live Site**: https://cloudstreet-dev.github.io/Learn-Programming-with-Smalltalk/
- **Repository**: https://github.com/cloudstreet-dev/Learn-Programming-with-Smalltalk
- **Actions**: https://github.com/cloudstreet-dev/Learn-Programming-with-Smalltalk/actions

### Commands

```bash
# Local development
bundle install
bundle exec jekyll serve

# Deploy
git push origin main
```

### Key Files

- `_config.yml` - Main configuration
- `index.html` - Homepage
- `assets/css/style.scss` - Custom CSS
- `.github/workflows/pages.yml` - Auto-deployment

---

**Your book is now ready for the world!** 🎉📚
