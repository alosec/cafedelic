# GitHub Issue Comment Command

Add a comment to GitHub issue #$ARGUMENTS using the safe temporary file approach.

## Instructions:

1. First, create the comment content in `/tmp/gh-comment-body.md`
2. Use the temporary file with `gh issue comment`
3. Clean up the temporary file

## Process:

```bash
# Step 1: Write your comment content
cat > /tmp/gh-comment-body.md <<'EOF'
[Write your comment content here - backticks, variables, and special characters are safe]
EOF

# Step 2: Post the comment
gh issue comment $ARGUMENTS --body-file /tmp/gh-comment-body.md

# Step 3: Clean up
rm /tmp/gh-comment-body.md
```

## Why this approach?

- The `--body-file` parameter prevents shell interpretation of special characters
- Quoted heredoc (`<<'EOF'`) ensures content is written literally
- Temporary file approach is the safest for complex technical content
- No risk of accidental command execution from backticks or variables

## Example usage:

For issue #12, you would:
1. Run `/gh-comment 12`
2. Write your comment with any special characters
3. The comment posts safely without shell interpretation