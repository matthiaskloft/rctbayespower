# PowerShell script to build rctbayespower documentation
Write-Host "Building rctbayespower documentation..." -ForegroundColor Green
Write-Host ""

# Change to package directory
Set-Location "c:\Users\Matze\Documents\GitHub\rctbayespower"

# Set R executable path
$rExe = "C:\R\R-4.4.2\bin\R.exe"

if (Test-Path $rExe) {
    Write-Host "Found R at: $rExe" -ForegroundColor Yellow
    Write-Host "Running documentation build script..." -ForegroundColor Yellow
    Write-Host ""
    
    & $rExe --vanilla -f "build_docs_manual.R"
      Write-Host ""
    Write-Host "Documentation build completed!" -ForegroundColor Green
} else {
    Write-Host "R not found at: $rExe" -ForegroundColor Red
    Write-Host ""
    Write-Host "Please check the R installation path." -ForegroundColor Yellow
}

Write-Host ""
Write-Host "Check these directories for results:" -ForegroundColor Blue
Write-Host "  - man/ - Generated documentation files" -ForegroundColor White
Write-Host "  - doc/ - Built vignette HTML files" -ForegroundColor White
