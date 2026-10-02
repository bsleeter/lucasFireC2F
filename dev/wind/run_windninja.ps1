# Runs WindNinja (mass solver, domain-average initialization) over a zone's elevation grid for a set of domain wind
# directions at one reference speed. The solution is linear in speed (checked: 40 vs 20 km/h gives 2.00x, directions within
# the 1-degree output rounding), so one run per direction gives the terrain speed ratio and direction deviation for any speed.
# Each direction writes to its own folder dir_<ddd.d> (WindNinja rounds the direction in its file names).
# Usage: pwsh run_windninja.ps1 <dem.tif> <out dir> [n directions = 16] [parallel runs = 4] [threads per run = 26]
param([string]$Dem, [string]$Out, [int]$NDir = 16, [int]$Par = 4, [int]$Threads = 26)
$exe = "F:\lucas-master\Tools\WindNinja\3.12.2\app\bin\WindNinja_cli.exe"
$env:WINDNINJA_DATA = "F:\lucas-master\Tools\WindNinja\3.12.2\app\share\windninja"
$dirs = 0..($NDir - 1) | ForEach-Object { [math]::Round($_ * 360 / $NDir, 1) }
$dirs | ForEach-Object -ThrottleLimit $Par -Parallel {
  $d = $_; $o = Join-Path $using:Out ("dir_{0:000.0}" -f $d); New-Item -ItemType Directory -Force $o | Out-Null
  if (Get-ChildItem $o -Filter "*_vel.asc" -ErrorAction SilentlyContinue) { "dir $d exists - skipped"; return }
  $env:WINDNINJA_DATA = $using:env:WINDNINJA_DATA
  $t = [Diagnostics.Stopwatch]::StartNew()
  & $using:exe --num_threads $using:Threads --elevation_file $using:Dem --initialization_method domainAverageInitialization `
    --input_speed 20 --input_speed_units kph --output_speed_units kph --input_direction $d `
    --input_wind_height 10 --units_input_wind_height m --output_wind_height 20 --units_output_wind_height ft `
    --vegetation trees --mesh_resolution 150 --units_mesh_resolution m --write_ascii_output true --ascii_out_utm false `
    --output_path $o *> (Join-Path $o "windninja.log")
  "dir {0,5}: {1:N0} s, exit {2}" -f $d, $t.Elapsed.TotalSeconds, $LASTEXITCODE
}
