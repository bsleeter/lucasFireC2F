# Builds lucasFireC2F-<version>.ssimpkg, the single-file package attached to a GitHub release, from the committed state of the
# repository (git archive HEAD, so uncommitted edits and untracked dev outputs never ship). dev/, docs/ and tests/ are left out:
# the stages do not read them, and they are 90% of the repository's size.
# Usage, from the repository root:  powershell -File tools\build_ssimpkg.ps1 [-OutDir <folder>]
param([string]$OutDir = ".")
$ErrorActionPreference = "Stop"
$PM = "C:\Program Files\SyncroSim\SyncroSim.PackageManager.exe"
$ver = ([xml](git show HEAD:package.xml)).package.version
$stage = Join-Path ([IO.Path]::GetTempPath()) "lucasFireC2F-$ver-build"
if (Test-Path $stage) { Remove-Item -Recurse -Force $stage }
New-Item -ItemType Directory $stage | Out-Null
# the archive goes through a file: a PowerShell pipe would re-encode the binary tar stream
$tarf = "$stage.tar"
git archive --format=tar --output="$tarf" HEAD; if ($LASTEXITCODE) { throw "git archive failed" }
tar -x -f $tarf -C $stage; if ($LASTEXITCODE) { throw "tar failed" }
Remove-Item $tarf
foreach ($d in "dev", "docs", "tests", ".gitignore") { $p = Join-Path $stage $d; if (Test-Path $p) { Remove-Item -Recurse -Force $p } }
$out = Join-Path (Resolve-Path $OutDir) "lucasFireC2F-$ver.ssimpkg"
if (Test-Path $out) { Remove-Item $out }
& $PM --create="$stage" --file="$out" --force
if (-not (Test-Path $out)) { throw "the package manager did not write $out" }
& $PM --metadata="$out"
Remove-Item -Recurse -Force $stage
Write-Host "wrote $out"
