# FULL prototype run (2,500 seasons + pool) of one hours-function candidate, spread days from its fitted curve, then the
# standard checks: 203 resample + BP, 176 sizes vs FPA, 162 AUC vs WRC, 169 selection, and the VIIRS daily-run check.
# Usage: pwsh run_full.ps1 <tag> <A> <B> <wind> <curve csv> [workers]
param([string]$Tag, [double]$A, [double]$B, [string]$Wind, [string]$Curve, [int]$Workers = 100)
$ErrorActionPreference = "Stop"; $R = "C:\Program Files\R\R-4.6.1\bin\Rscript.exe"; $M = "F:\lucas-master\Models\California 30m BurnP3"; Set-Location $M
$log = "F:\lucasFireC2F\dev\hours_days\logs\$Tag.log"; New-Item -ItemType Directory -Force (Split-Path $log) | Out-Null
function Step($n, [scriptblock]$b) { "`n==== $n  $(Get-Date -Format 'HH:mm:ss')" | Tee-Object -FilePath $log -Append; & $b *>&1 | Out-String -Width 260 | Tee-Object -FilePath $log -Append; if ($LASTEXITCODE) { throw "$n failed - see $log" } }
$env:ZONE_SYSTEM = "pyrome"; $env:ZONE_VINTAGE = "2020"; $env:C2FW_FMC = "85"
Step "fire list" { $env:PROTO_CURVE = $Curve; & $R F:\lucasFireC2F\dev\hours_days\proto_fires.R $Tag $A $B $Wind; $env:PROTO_CURVE = "" }
Step "grow (spread network)" { $env:C2FW_MESSAGES = "1"; $env:C2FW_CHUNK = "20"; & $R F:\lucasFireC2F\dev\hours_days\202_run_proto.R $Tag 4 $Workers; $env:C2FW_MESSAGES = "" }
Step "203 collect" { & $R src/203_c2fw_collect.R $Tag 4 36.8 }
$Bd = ("$M\data\c2fw\$Tag") -replace '\\', '/'
& $R -e "k<-read.csv('$Bd/kept_r4.csv'); write.csv(data.frame(Iteration=k`$Iteration, Area=k`$c2fw_ha), '$Bd/sim_sizes_r4.csv', row.names=FALSE)"
$env:CHECK_SOURCE = "fpa"; $env:CHECK_MIN = "36.8"
Step "176 sizes" { $env:SIM_CSV = "$M\data\c2fw\$Tag\sim_sizes_r4.csv"; & $R src/176_size_vs_mtbs.R 1026 "P26 Northern Sierra Nevada" 4 2020 2500; $env:SIM_CSV = "" }
Step "162 AUC" { $env:BP_TIF = "$M\data\c2fw\$Tag\bp_r4.tif"; & $R src/162_score_vs_wrc.R 1026 4 1 }
Step "169 selection" { & $R src/169_selection_vs_wrc.R 1026 4 1 2020; $env:BP_TIF = "" }
Step "VIIRS daily runs" { & $R F:\lucasFireC2F\dev\hours_days\full_check.R $Tag }
"`n==== done $(Get-Date)" | Tee-Object -FilePath $log -Append
