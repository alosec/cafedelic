# Update GitHub Issue with Comment

Add a comment to GitHub issue: $ARGUMENTS

Parse the input to extract:
- Issue number (required)
- Comment text (required)

IMPORTANT: For safety, write the comment to a temporary file first:
```bash
cat > /tmp/gh-comment-body.md <<'EOF'
COMMENT_TEXT_HERE
EOF

gh issue comment NUMBER --body-file /tmp/gh-comment-body.md
rm /tmp/gh-comment-body.md
```

Never use --body parameter directly as it can cause command injection with technical content.