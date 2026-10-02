# One prototype candidate end to end: seed fire list (proto_fires.R) -> C2F-W growth with the spread network (202) -> score.
# Usage: pwsh run_candidate.ps1 <tag> <A> <B> <wind daily|raws> [seasons] [dmax] [workers]
param([string]$Tag, [double]$A, [double]$B, [string]$Wind, [int]$Seasons = 300, [int]$Dmax = 20, [int]$Workers = 100)
$ErrorActionPreference = "Stop"; $R = "C:\Program Files\R\R-4.6.1\bin\Rscript.exe"; $M = "F:\lucas-master\Models\California 30m BurnP3"; Set-Location $M
$log = "F:\lucasFireC2F\dev\hours_days\logs\$Tag.log"; New-Item -ItemType Directory -Force (Split-Path $log) | Out-Null
$t = [Diagnostics.Stopwatch]::StartNew()
& $R F:\lucasFireC2F\dev\hours_days\proto_fires.R $Tag $A $B $Wind $Seasons $Dmax *>> $log; if ($LASTEXITCODE) { throw "proto_fires failed" }
$env:ZONE_SYSTEM = "pyrome"; $env:C2FW_FMC = "85"; $env:C2FW_MESSAGES = "1"
$env:C2FW_CHUNK = "5"; & $R F:\lucasFireC2F\dev\hours_days\202_run_proto.R $Tag 4 $Workers *>> $log; if ($LASTEXITCODE) { throw "202 failed" }
$env:C2FW_MESSAGES = ""
& $R F:\lucasFireC2F\dev\hours_days\proto_score.R $Tag *>> $log; if ($LASTEXITCODE) { throw "score failed" }
"{0} done in {1:N1} min" -f $Tag, $t.Elapsed.TotalMinutes | Tee-Object -FilePath $log -Append
