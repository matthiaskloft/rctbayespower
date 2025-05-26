# PowerShell script to build vignettes using R
Write-Host "📖 Building rctbayespower Vignettes" -ForegroundColor Green
Write-Host "==================================`n"

# Change to package directory
Set-Location "c:\Users\Matze\Documents\GitHub\rctbayespower"

# Set R executable path
$rExe = "C:\R\R-4.4.2\bin\R.exe"

if (Test-Path $rExe) {
    Write-Host "Running vignette build script...`n" -ForegroundColor Yellow
    
    # Run R script
    try {
        & $rExe --vanilla --slave -f "build_vignettes_only.R"
        Write-Host "`n✅ Vignette build process completed!" -ForegroundColor Green
    }
    catch {
        Write-Host "❌ Error running R script: $_" -ForegroundColor Red
    }
} else {
    Write-Host "❌ R not found at: $rExe" -ForegroundColor Red
    Write-Host "`nPlease check the R installation path." -ForegroundColor Yellow
}

Write-Host "`n📁 Results should be in the doc/ directory" -ForegroundColor Blue
