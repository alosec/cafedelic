# Resolve/Close GitHub Issue

Resolve or close GitHub issue after implementation: $ARGUMENTS

Parse the input to extract:
- Issue number (required)
- Resolution comment (optional)

Steps:
1. If a resolution comment is provided, add it first using the safe method:
```bash
cat > /tmp/gh-comment-body.md <<'EOF'
RESOLUTION_COMMENT_HERE
EOF

gh issue comment NUMBER --body-file /tmp/gh-comment-body.md
rm /tmp/gh-comment-body.md
```

2. Close the issue:
```bash
gh issue close NUMBER
```

Confirm the issue has been closed.