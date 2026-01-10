# Claude Code Permissions Template

## Required Commands for Efficient Code Development

### Text Processing Tools
```bash
# sed - Stream editor for filtering and transforming text
sed -i 's/old_pattern/new_pattern/g' file.txt
sed -i '10s/old/new/' file.txt  # Replace on specific line
# Why needed: Precise single-line replacements in large files without loading entire file

# awk - Pattern scanning and processing language
awk '/pattern/ {gsub(/old/, "new")} 1' file.txt > temp && mv temp file.txt
# Why needed: More robust pattern matching and conditional replacements

# perl - Perl one-liners for regex replacements
perl -pi -e 's/old_pattern/new_pattern/g' file.txt
perl -pi -e 's/pattern/replacement/ if /condition/' file.txt
# Why needed: Advanced regex capabilities, handles complex escaping better than sed

# grep with replacement (GNU grep)
grep -l "pattern" *.R | xargs perl -pi -e 's/old/new/g'
# Why needed: Bulk operations across multiple files
```

### File Manipulation
```bash
# In-place file editing without intermediate files
python -c "
import re
with open('file.txt', 'r') as f: content = f.read()
content = re.sub(r'pattern', 'replacement', content)
with open('file.txt', 'w') as f: f.write(content)
"
# Why needed: Handles complex multi-line patterns and escaping issues

# Backup and restore capabilities
cp file.txt file.txt.bak  # Create backup before risky edits
# Why needed: Safety when making complex changes
```

### Development-Specific Tools
```bash
# R script execution with error handling
Rscript -e "tryCatch(devtools::document(), error=function(e) cat('Error:', e\$message))"
# Why needed: Better error reporting during development

# Git operations for tracking changes
git add -A && git commit -m "Automated changes: description"
# Why needed: Track incremental changes during large refactors
```

## Use Cases Where These Commands Are Essential

### 1. Large File Modifications
- **Problem**: `optimization_internal.R` has 2900+ lines, Edit tool sometimes has concurrency issues
- **Solution**: `sed`/`perl` for targeted line replacements without full file rewrite
- **Example**: Adding parameters to function signatures across multiple locations

### 2. Function Signature Updates
- **Problem**: Need to update function signatures and their calls consistently
- **Solution**: Pattern-based replacements ensuring all locations are updated
- **Example**: Adding `optimum, sims_final_run` parameters to call chain

### 3. Bulk Renaming/Refactoring
- **Problem**: Changing variable names across multiple files
- **Solution**: `grep -l pattern files | xargs sed -i 's/old/new/g'`
- **Example**: Renaming properties from `surrogate_optimum` to `optimum_surrogate`

### 4. Multi-line Pattern Replacements
- **Problem**: Function bodies, roxygen blocks, or code blocks spanning multiple lines
- **Solution**: Python/Perl scripts with multi-line regex
- **Example**: Updating roxygen documentation blocks

## Alternative: Enhanced Edit Tool Capabilities

If command-line tools aren't available, enhanced Edit tool features would help:
1. **Pattern-based replacement**: `Edit` with regex support
2. **Line-range editing**: Specify line ranges for modifications
3. **Concurrent access handling**: Better conflict resolution
4. **Backup/restore**: Automatic versioning for complex changes

## Security Considerations

All requested commands are:
- ✅ **Read/Write only**: No network access, no system modification beyond project files
- ✅ **Project-scoped**: Only operate on files within project directory
- ✅ **Reversible**: Can be undone with git or backup files
- ✅ **Transparent**: All changes visible in version control

The tools are standard development utilities available in most programming environments and pose no security risk beyond normal file editing capabilities.