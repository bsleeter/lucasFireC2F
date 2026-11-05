# =============================================================================
# unit-home-check.ps1 - the Data Sources page's rules (ui/HomeCheck.cs), compiled alone, without SyncroSim or Studio
# =============================================================================
# powershell -ExecutionPolicy Bypass -File tests/unit-home-check.ps1
$ErrorActionPreference = "Stop"
$root = Split-Path -Parent $PSScriptRoot
$dll = Join-Path ([IO.Path]::GetTempPath()) ("homecheck-" + [guid]::NewGuid().ToString("N") + ".dll")
$csc = Join-Path $env:WINDIR "Microsoft.NET\Framework64\v4.0.30319\csc.exe"
& $csc /nologo /target:library "/out:$dll" /reference:System.Core.dll (Join-Path $root "ui\HomeCheck.cs")
if ($LASTEXITCODE -ne 0) { throw "compile failed" }
[void][Reflection.Assembly]::LoadFrom($dll)
$n = 0; function Expect($what, $x) { if ($x -ne $true) { throw "FAILED: $what" }; $script:n++; Write-Output "ok  $what" }

$boxes = [lucasFireUI.HomeCheck]::ZoneBoxes((Join-Path $root "defaults\download\fire-zone-boxes.csv"))
Expect "all 128 pyromes have a box" ($boxes.Count -eq 128)
$p26 = New-Object lucasFireUI.HomeCheck+Zone; $p26.Name = "P26"; $p26.Box = $boxes[1026]
Expect "a zone needs its 25 km grid + 32.4 km" ($p26.NeedKm -eq 57.4)
function Row($km, $status = "done") { $b = $boxes[1026]; $m = $km * 1000
  $d = New-Object 'System.Collections.Generic.Dictionary[string,string]'; $d["Status"] = $status; $d["Units"] = "P26"; $d["BufferKm"] = "$km"
  $d["Xmin"] = [string]($b[0] - $m); $d["Ymin"] = [string]($b[1] - $m); $d["Xmax"] = [string]($b[2] + $m); $d["Ymax"] = [string]($b[3] + $m); $d }
Expect "a 60 km region covers the zone" ([lucasFireUI.HomeCheck]::Covers((Row 60), $p26))
Expect "a 10 km lucasCore region does not" (-not [lucasFireUI.HomeCheck]::Covers((Row 10), $p26))
Expect "a failed download does not, whatever its box" (-not [lucasFireUI.HomeCheck]::Covers((Row 60 "failed"), $p26))
Expect "no manifest row (by hand) is not checked" ([lucasFireUI.HomeCheck]::Covers($null, $p26))
$list = { param($r) $l = New-Object 'System.Collections.Generic.List[System.Collections.Generic.Dictionary[string,string]]'; foreach ($x in $r) { $l.Add($x) }; ,$l }
$zs = [lucasFireUI.HomeCheck+Zone[]]@($p26)
Expect "one covering folder among several is enough" ($null -eq [lucasFireUI.HomeCheck]::Uncovered($zs, (& $list @((Row 10), (Row 60)))))
$u = [lucasFireUI.HomeCheck]::Uncovered($zs, (& $list @(,(Row 10))))
Expect "only a small one: the zone is named" ($u.Name -eq "P26")
$w = [lucasFireUI.HomeCheck]::Why($p26, (& $list @(,(Row 10))))
Expect "the reason says what it holds and what is needed: $w" ($w -match "\+ 10 km" -and $w -match "P26 needs \+ 58 km")
$nobox = New-Object lucasFireUI.HomeCheck+Zone; $nobox.Name = "no box"
Expect "a zone with no box is not checked" ($null -eq [lucasFireUI.HomeCheck]::Uncovered([lucasFireUI.HomeCheck+Zone[]]@($nobox), (& $list @(,(Row 10)))))

$h = Join-Path ([IO.Path]::GetTempPath()) ("homecheck-" + [guid]::NewGuid().ToString("N")); $lf = Join-Path $h "fuels\landfire"
function Tif($v, $p, $name) { $d = Join-Path $lf "LF${v}_${p}_CONUS\LF${v}_${p}_CONUS\Tif"; [void](New-Item -ItemType Directory -Force $d); [void](New-Item -ItemType File -Force (Join-Path $d $name)) }
Tif "2020" "FBFM40" "LF2020_FBFM40_CONUS.tif"; Tif "2020" "CH" "LC22_CH_220.tif"
Expect "a layer at its standard name" ([lucasFireUI.HomeCheck]::LayerFile($lf, "2020", "FBFM40") -like "*LF2020_FBFM40_CONUS.tif")
Expect "LF2020 CH under its old file name" ([lucasFireUI.HomeCheck]::LayerFile($lf, "2020", "CH") -like "*LC22_CH_220.tif")
Expect "a layer not there" ($null -eq [lucasFireUI.HomeCheck]::LayerFile($lf, "2020", "CBD"))
Expect "a vintage not there" ($null -eq [lucasFireUI.HomeCheck]::LayerFile($lf, "2022", "FBFM40"))
Expect "a folder not there" ($null -eq [lucasFireUI.HomeCheck]::LayerFile((Join-Path $h "nope"), "2020", "FBFM40"))
Expect "LF2020, no zip: where to put it" ([lucasFireUI.HomeCheck]::LayerWhy($h, "2020", "CBD") -match "by hand into .*supplied")
[void](New-Item -ItemType Directory -Force (Join-Path $h "supplied")); [void](New-Item -ItemType File (Join-Path $h "supplied\LF2020_CBD_CONUS_20260522.zip"))
Expect "LF2020, zip supplied: a download clips it" ([lucasFireUI.HomeCheck]::LayerWhy($h, "2020", "CBD") -match "zip is in supplied")
Expect "another vintage comes from LANDFIRE" ([lucasFireUI.HomeCheck]::LayerWhy($h, "2022", "CBD") -match "from LANDFIRE")
Remove-Item -LiteralPath $h -Recurse -Force
Write-Output "$n checks passed"
