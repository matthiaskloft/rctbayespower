@echo off
echo Building rctbayespower documentation...
echo.

REM Set R path directly
set R_PATH="C:\R\R-4.4.2\bin\R.exe"

if not exist %R_PATH% (
    echo R not found at: %R_PATH%
    echo Please check the R installation path.
    pause
    exit /b 1
)

echo Found R at: %R_PATH%
echo Running documentation build script...
echo.

%R_PATH% --vanilla -f build_docs_manual.R

echo.
echo Documentation build completed!
echo Check the output above for any errors or warnings.
pause
