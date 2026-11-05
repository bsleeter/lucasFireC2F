# =============================================================================
# build_ui.ps1 - compile the custom SyncroSim pages (ui/*.cs) into lucasFireUI.dll at the package root
# =============================================================================
# package.xml names the views as viewClassName="lucasFireUI.<Class>" viewClassAssembly="lucasFireUI". Uses the C# compiler that ships
# with .NET Framework 4.8 (C# 5: no string interpolation or ?.), against the SyncroSim assemblies of the installed SyncroSim.
# SyncroSim Studio locks the DLL while it is open: close Studio before rebuilding, and restart it to load a new build.
# Usage: powershell -ExecutionPolicy Bypass -File tools/build_ui.ps1 [-SyncroSim "C:\Program Files\SyncroSim"] [-Out <dll>]
#   -Out  write elsewhere, to check that a change compiles while Studio has the package DLL open
# =============================================================================
param([string]$SyncroSim = "C:\Program Files\SyncroSim", [string]$Out = "")
$ErrorActionPreference = "Stop"
$root = Split-Path -Parent $PSScriptRoot
$csc = Join-Path $env:WINDIR "Microsoft.NET\Framework64\v4.0.30319\csc.exe"
if (-not (Test-Path $csc)) { throw "C# compiler not found: $csc" }
$out = if ($Out) { $Out } else { Join-Path $root "lucasFireUI.dll" }
$src = Get-ChildItem (Join-Path $root "ui") -Filter *.cs | ForEach-Object FullName
$refs = @("System.dll", "System.Core.dll", "System.Data.dll", "System.Drawing.dll", "System.Windows.Forms.dll",
          (Join-Path $SyncroSim "SyncroSim.Core.dll"), (Join-Path $SyncroSim "SyncroSim.Core.Forms.dll")) | ForEach-Object { "/reference:$_" }
& $csc /nologo /target:library /optimize+ "/out:$out" $refs $src
if ($LASTEXITCODE -ne 0) { throw "compile failed" }
Write-Output "built $out"
