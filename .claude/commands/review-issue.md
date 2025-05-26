# Review GitHub Issue

Review GitHub issue #$ARGUMENTS and prepare a task plan.

## Steps:
1. Use `gh issue view $ARGUMENTS` to read the issue and all comments
2. Check for corresponding documentation in `docs/issues/` directory
3. Analyze the issue requirements and current project state
4. Present a concise task plan based on the issue's specificity:
   - For specific tasks: Break down into actionable steps
   - For general issues: Identify key areas and propose approach
   - For stubs: Suggest clarifying questions or initial exploration

## Output Format:
- Issue Summary: Brief overview of the issue
- Task Plan: Numbered list of specific actions
- Considerations: Any blockers, dependencies, or clarifications needed

Keep the plan focused and actionable, adapting to the issue's level of detail.