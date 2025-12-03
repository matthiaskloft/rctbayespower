#!/usr/bin/env python3
"""PreToolUse hook to block access to sensitive files.

Required because Claude Code's deny rules for file operations don't work.
See: https://github.com/anthropics/claude-code/issues/6699

This hook intercepts Read, Edit, and Write tool calls and blocks access
to files matching sensitive patterns (credentials, secrets, API keys, etc.).
"""
import json
import sys
import re


# Patterns for sensitive files (case-insensitive regex)
BLOCKED_PATTERNS = [
    r"\.env$",           # .env file
    r"\.env\.",          # .env.local, .env.production, etc.
    r"\.Renviron$",      # R environment file
    r"credentials",      # Any file with "credentials" in name
    r"\.pem$",           # SSL certificates
    r"\.key$",           # Private keys
    r"secret",           # Any file with "secret" in path
    r"password",         # Any file with "password" in path
    r"api[_-]?key",      # API key files
    r"\.htpasswd$",      # Apache password files
    r"id_rsa",           # SSH private keys
    r"id_ed25519",       # SSH private keys
]


def is_sensitive(filepath: str) -> bool:
    """Check if filepath matches any blocked pattern."""
    if not filepath:
        return False
    path_lower = filepath.lower()
    return any(re.search(pattern, path_lower) for pattern in BLOCKED_PATTERNS)


def main():
    """Main hook entry point."""
    try:
        # Read tool input from stdin (JSON format)
        data = json.load(sys.stdin)
        tool_input = data.get("tool_input", {})

        # Check file_path parameter (used by Read, Edit, Write)
        file_path = tool_input.get("file_path", "")

        if file_path and is_sensitive(file_path):
            # Output JSON to deny the operation
            result = {
                "hookSpecificOutput": {
                    "hookEventName": "PreToolUse",
                    "permissionDecision": "deny",
                    "permissionDecisionReason": (
                        f"Blocked: '{file_path}' matches sensitive file pattern. "
                        "This file may contain secrets or credentials."
                    )
                }
            }
            print(json.dumps(result))
            sys.exit(0)

        # Allow the operation (exit 0 with no output = allow)
        sys.exit(0)

    except json.JSONDecodeError:
        # If we can't parse input, allow the operation
        # (fail open to avoid breaking legitimate operations)
        sys.exit(0)
    except Exception as e:
        # Log error to stderr but don't block
        print(f"file_guard.py error: {e}", file=sys.stderr)
        sys.exit(0)


if __name__ == "__main__":
    main()
