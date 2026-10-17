# lucasFireC2F

A [SyncroSim](https://syncrosim.com) 3 package for wildfire burn probability, fire effects and risk, part of the LUCAS model
family. Fires are grown with [Cell2Fire-World](https://github.com/fire2a/C2F-W) (C2F-W) on Scott and Burgan fuels from
LANDFIRE; fire effects (flame length, crown fire, severity) come from Rothermel surface fire with Van Wagner and Cruz crown
fire; hazard and risk follow Wildfire Risk to Communities (WRC). Burn probability is built FSim-style: thousands of simulated
fire seasons per fire zone, calibrated to the recorded large-fire population.

The unit of work is a **fire zone**: one of the 128 pyromes of the conterminous US, the units FSim and WRC simulate on.
A zone is prepared from national data, calibrated to its own fire record, run, and optionally merged with neighbouring
zones so that fire crossing their shared boundaries is counted.

Package version **2.4.0**. Runs anywhere in CONUS; developed and tested on California pyromes.

## User guide

[docs/user-guide/lucasFireC2F-User-Guide.md](docs/user-guide/lucasFireC2F-User-Guide.md) builds, calibrates, runs and
merges a zone entirely in SyncroSim Studio, with screen captures at each step. Its worked example is P18 Southern
Cascades. Section 2.1 lists every input dataset and where to download it.

## Installing

Requirements: Windows; SyncroSim Studio 3.1; R 4.4 or later with `rsyncrosim`, `data.table`, `terra`, `sf`, `arrow`, `DBI`,
`RSQLite`, `cffdrs`, `jsonlite` and `mgcv` (also listed in `lucasFireC2F.yml`). The C2F-W executable and its DLLs ship in
`bin/windows`.

There are two ways to install. Close SyncroSim Studio first in either case; the package manager is
`C:\Program Files\SyncroSim\SyncroSim.PackageManager.exe`.

**A. From a release file (no git needed).** Download `lucasFireC2F-<version>.ssimpkg` from the repository's
[Releases](https://github.com/bsleeter/lucasFireC2F/releases) page and install it:

```
"C:\Program Files\SyncroSim\SyncroSim.PackageManager.exe" --finstall="lucasFireC2F-2.4.0.ssimpkg"
```

The package is copied into SyncroSim's own package folder, so the downloaded file can be deleted afterwards.

**B. From a clone of this repository.** This route suits anyone who wants to follow development or change the scripts:

```
git clone https://github.com/bsleeter/lucasFireC2F.git
"C:\Program Files\SyncroSim\SyncroSim.PackageManager.exe" --xinstall="C:\path\to\lucasFireC2F"
```

`--xinstall` registers the folder instead of copying it: SyncroSim runs the scripts where they are, so the folder must stay
in place, and a `git pull` that keeps the same version number takes effect the next time a scenario runs. (Without git,
GitHub's *Code > Download ZIP*, unzipped to a permanent folder, works the same way.)

Both routes are also buttons in Studio: *File > Local Packages* has **Install from File...** (A) and **Install from
Folder...** (B), shown in section 1.1 of the user guide.

Either way, check the install with `SyncroSim.PackageManager.exe --installed`, which should list lucasFireC2F with status
OK. Then, in Studio, create a library, add the lucasFireC2F package, turn SyncroSim multiprocessing off (the package
parallelises fire growth itself) and point the library at your R installation. Chapter 1 of the user guide shows each step.

**Upgrading.** For a new version number, back up your libraries, then remove the old version and install the new one:

```
"C:\Program Files\SyncroSim\SyncroSim.PackageManager.exe" --remove=lucasFireC2F --version=<old version>
```

followed by `--finstall` (A) or `git pull` and `--xinstall` (B). Then move each existing library to the new version:

```
"C:\Program Files\SyncroSim\SyncroSim.Console.exe" --add --package --pkg=lucasFireC2F --ver=<new version> --lib=<library>
"C:\Program Files\SyncroSim\SyncroSim.Console.exe" --update --lib=<library>
```

**Building a release file.** `tools/build_ssimpkg.ps1` builds `lucasFireC2F-<version>.ssimpkg` from the committed state
of the repository, leaving out `dev/`, `docs/` and `tests/` (about 1 MB instead of 40). Attach it to a GitHub release
named for the version in `package.xml`.

## Workflow and stages

| Scenario | Stages | Does |
|---|---|---|
| Prep | P1 Prepare Zone Landscape, P2 Fit Ignition Surfaces, P3 Extract Zone Weather, P4 MTBS Severity Target, P5 Burning-Period Wind (RAWS) | Builds the zone's 150 m landscape, ignition-probability surfaces, daily weather, severity target and wind table from national data |
| Calibrate | C1 Import Fire Record, C2 Build Weather Stream, C3 Fit Ignitions, stages 1-5 as a seed run, C5 Fit Spread Days, C6 Score Against Fire Records | Fits fires per season, their season and cause, and the spread-day distribution that reproduces the recorded fire sizes |
| Run | 1 Prepare Landscape, 2 Sample Ignitions, 3 Sample Burning Conditions, 4 Grow Fires (C2F-W), 5 Summarize Burn Probability, 6 Fire Effects, 7 Hazard and Risk, 8 Fire Detail, C6, C7 Validate Burn Probability, R Report Figures | Burn probability, effects and risk maps, per-fire detail, scores against the record and WRC, report figures |
| Merge | M Merge Zone Results, then 5-8, C7, R | Combines zone runs, from one library or several, into one landscape without growing any fire again |

Calibrate depends on the Prep result; Run depends on the Calibrate and Prep results.

## Data

All inputs are public national datasets: LANDFIRE (fuels, canopy, elevation, biophysical settings, roads), the FPA-FOD fire
occurrence database, MTBS perimeters and severity mosaics, gridMET daily meteorology, FEMS RAWS hourly weather, NWS fire
weather zones, pyromes, PRISM normals, Annual NLCD, SILVIS housing density, NIFC interagency fire perimeters, and WRC burn
probability as a benchmark. Paths are set once per project in the *Data Sources* sheet; the files are read where they are and
never copied into the library. The data are not in this repository.

## Repository layout

| Path | Contents |
|---|---|
| `package.xml` | SyncroSim package definition: datasheets, stages, layouts |
| `src/` | R scripts, one per stage, and shared code (`shared.R`, `effects.R`, `rothermel.R`, `covariates.R`, `report.R`, ...) |
| `defaults/` | Default project data: fuel models and types, fire and weather zones, moisture scenarios, seasons |
| `updates/` | Schema updates applied when a library is upgraded |
| `bin/windows/` | Cell2Fire-World v1.0.1 executable and DLLs (GPL-3.0) |
| `docs/user-guide/` | The user guide and its figures |
| `tools/` | Scripts that regenerate the defaults and build the release package file |
| `prep/`, `tests/` | Zone preparation helper and tests |
| `dev/` | Development and analysis scripts (validation, calibration experiments, paper figures, comparisons); their outputs are not tracked |

## Licence

GPL-3.0 (see `LICENSE`). The package distributes Cell2Fire-World, which is GPL-3.0.
