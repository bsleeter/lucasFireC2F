# Prototype grid: hours-per-day function x growth wind, each a 300-season seed run scored by proto_score.R (scores.csv).
$C = @(
  @{Tag = "p026_hd_ctrl";      A = 2; B = 0; Wind = "daily"; Dmax = 25 },   # control: the current model (2 h/day, daily wind)
  @{Tag = "p026_hd_raws";      A = 2; B = 0; Wind = "raws";  Dmax = 25 },   # wind alone
  @{Tag = "p026_hd_a1b3";      A = 1; B = 3; Wind = "raws";  Dmax = 30 },
  @{Tag = "p026_hd_a1b5";      A = 1; B = 5; Wind = "raws";  Dmax = 30 },
  @{Tag = "p026_hd_a2b3";      A = 2; B = 3; Wind = "raws";  Dmax = 25 },
  @{Tag = "p026_hd_a2b5";      A = 2; B = 5; Wind = "raws";  Dmax = 25 },
  @{Tag = "p026_hd_a1b4d";     A = 1; B = 4; Wind = "daily"; Dmax = 30 },   # hours function without the wind change
  @{Tag = "p026_hd_a3b4";      A = 3; B = 4; Wind = "raws";  Dmax = 20 }
)
foreach ($c in $C) {
  if (Test-Path "F:\lucas-master\Models\California 30m BurnP3\data\c2fw\$($c.Tag)\proto_score.rds") { "$($c.Tag) scored - skipped"; continue }
  try { pwsh -NoProfile -File F:\lucasFireC2F\dev\hours_days\run_candidate.ps1 -Tag $c.Tag -A $c.A -B $c.B -Wind $c.Wind -Seasons 300 -Dmax $c.Dmax -Workers 100 }
  catch { "$($c.Tag) FAILED: $_" }
  Get-Content "F:\lucasFireC2F\dev\hours_days\logs\$($c.Tag).log" -Tail 1
}
"grid finished $(Get-Date)"
