#!/usr/bin/env python3
"""
Fix finalize_with_surrogate function to use sims_final_run
"""

import re
import os

def fix_finalize_function():
    filepath = "R/optimization_internal.R"

    # Read the file
    with open(filepath, 'r') as f:
        content = f.read()

    # Fix the n_sims assignment
    content = re.sub(
        r'n_sims <- max\(fidelity_schedule\$n_sims\)',
        'n_sims <- sims_final_run',
        content
    )

    # Write back
    with open(filepath, 'w') as f:
        f.write(content)

    print("✓ Updated n_sims assignment to use sims_final_run")

if __name__ == "__main__":
    # Change to project root
    os.chdir("C:/Users/Matze/Documents/GitHub/rctbayespower")
    fix_finalize_function()