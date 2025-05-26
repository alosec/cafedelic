# Git Clear State Command

Review the current git status and prepare to clear the working state with a comprehensive commit that documents architectural decisions.

## Steps to execute:

1. **Review current changes**:
   - Run `git status` to see all modified and untracked files
   - Run `git diff` to review all unstaged changes
   - Run `git diff --staged` to review any staged changes

2. **Analyze recent work**:
   - Run `git log --oneline -10` to see recent commit history
   - Identify the architectural decisions and design patterns implemented
   - Note any significant technical choices made

3. **Chart project trajectory**:
   - Summarize what was accomplished in this work session
   - Document why certain architectural decisions were made
   - Explain how these changes fit into the overall project goals
   - Note any trade-offs or technical debt introduced

4. **Prepare comprehensive commit**:
   - Stage all relevant changes with `git add`
   - Craft a detailed commit message that includes:
     - Summary line describing the overall change
     - Detailed explanation of architectural decisions
     - Rationale for design choices
     - Impact on system architecture
     - Any notable patterns or conventions established
     - Future considerations or next steps

5. **Clear working state**:
   - After committing, verify clean state with `git status`
   - Document any intentionally uncommitted files

## Example commit message format:
```
feat: implement [feature/system] with [architectural pattern]

Architectural Decisions:
- Chose [pattern/approach] because [reasoning]
- Implemented [component] using [technology/pattern] to achieve [goal]
- Structured [module] to support [future capability]

Technical Rationale:
- [Decision 1]: [Why it matters for the system]
- [Decision 2]: [Trade-offs considered]

This establishes [pattern/convention] for future [components/features].
Next steps include [what comes next].
```

Arguments: $ARGUMENTS