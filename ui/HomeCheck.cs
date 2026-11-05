// =============================================================================
// HomeCheck.cs - what the Data Sources page asks of a home folder: does it cover a zone, and is a LANDFIRE layer there
// =============================================================================
// No SyncroSim types, so tests/unit-home-check.ps1 can run it. The rule is the stages' own (src/shared.R): a zone needs its bounding box plus its grid
// buffer plus the 32.1 km reach of the ignition covariates plus 0.3 km (study_box); a home dataset with no manifest row was put there by hand and is
// not checked; a manifest box must hold the need to within a metre (manifest_covers).
// =============================================================================
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;

namespace lucasFireUI
{
    public static class HomeCheck
    {
        public const double ReachKm = 32.4, DefaultGridKm = 25;

        public class Zone { public string Name; public double[] Box; public double GridKm = DefaultGridKm; public double NeedKm { get { return GridKm + ReachKm; } } }

        // defaults/download/fire-zone-boxes.csv: Map Code -> box in EPSG:5070 (a zone that is not listed has no box, and is not checked)
        public static Dictionary<int, double[]> ZoneBoxes(string csv)
        {
            var d = new Dictionary<int, double[]>();
            foreach (string line in File.ReadAllLines(csv).Skip(1))
            {
                string[] p = line.Split(','); if (p.Length < 5) continue;
                d[int.Parse(p[0], CultureInfo.InvariantCulture)] = p.Skip(1).Take(4).Select(x => double.Parse(x, CultureInfo.InvariantCulture)).ToArray();
            }
            return d;
        }

        private static string Get(Dictionary<string, string> r, string k) { string v; return r != null && r.TryGetValue(k, out v) ? v : ""; }

        // one folder's manifest row against a zone: no row = by hand, not checked; a row without a box cannot be checked either
        public static bool Covers(Dictionary<string, string> row, Zone z)
        {
            if (row == null) return true;
            if (Get(row, "Status") != "done") return false;
            double x0, y0, x1, y1; const NumberStyles n = NumberStyles.Float; CultureInfo c = CultureInfo.InvariantCulture;
            if (!double.TryParse(Get(row, "Xmin"), n, c, out x0) || !double.TryParse(Get(row, "Ymin"), n, c, out y0) || !double.TryParse(Get(row, "Xmax"), n, c, out x1) || !double.TryParse(Get(row, "Ymax"), n, c, out y1)) return true;
            double b = z.NeedKm * 1000;
            return x0 <= z.Box[0] - b + 1 && y0 <= z.Box[1] - b + 1 && x1 >= z.Box[2] + b - 1 && y1 >= z.Box[3] + b - 1;
        }

        // the first zone no holder covers (null = every zone is covered). Holders: the manifest rows of the folders holding the dataset
        public static Zone Uncovered(IEnumerable<Zone> zones, List<Dictionary<string, string>> holders)
        {
            return zones.Where(z => z.Box != null).FirstOrDefault(z => !holders.Any(h => Covers(h, z)));
        }

        // why: a sentence for the page
        public static string Why(Zone z, List<Dictionary<string, string>> holders)
        {
            Dictionary<string, string> bad = holders.FirstOrDefault(h => h != null && Get(h, "Status") != "done");
            if (bad != null) return string.Format("Its last Data Download ended {0}. Run it again.", Get(bad, "Status"));
            Dictionary<string, string> h0 = holders.FirstOrDefault(h => h != null);
            string held = h0 == null ? "" : string.Format("Downloaded for {0}{1}; ", Get(h0, "Units"), Get(h0, "BufferKm").Length > 0 ? " + " + Get(h0, "BufferKm") + " km" : "");
            return string.Format(CultureInfo.InvariantCulture, "{0}{1} needs + {2:0} km. Run a Data Download for this zone.", held, z.Name, Math.Ceiling(z.NeedKm));
        }

        // a LANDFIRE layer under a folder, as landfire_file finds it: the standard name, else the product folder's one GeoTIFF (LF2020 CH is LC22_CH_220.tif)
        public static string LayerFile(string folder, string vintage, string product)
        {
            try
            {
                if (!Directory.Exists(folder)) return null;
                string stem = string.Format("LF{0}_{1}_CONUS", vintage, product);
                var tifs = new List<string>();
                foreach (string d in Directory.GetDirectories(folder, stem + "*"))
                {
                    string dir = Path.Combine(d, stem, "Tif"); if (!Directory.Exists(dir)) continue;
                    string std = Path.Combine(dir, stem + ".tif"); if (File.Exists(std)) return std;
                    tifs.AddRange(Directory.GetFiles(dir, "*.tif").Where(f => f.EndsWith(".tif", StringComparison.OrdinalIgnoreCase)));
                }
                return tifs.Count == 1 ? tifs[0] : null;
            }
            catch (Exception) { return null; }
        }

        // the sentence for a layer not downloaded yet: LF2020 is the five zips a person supplies, other vintages come from LANDFIRE
        public static string LayerWhy(string home, string vintage, string product)
        {
            if (vintage != "2020") return string.Format("LF{0} {1}: a Data Download gets this from LANDFIRE.", vintage, product);
            string zip = string.Format("LF2020_{0}_CONUS_20260522.zip", product);
            if (home.Length > 0 && File.Exists(Path.Combine(home, "supplied", zip))) return "Its zip is in supplied. A Data Download will clip it.";
            return string.Format("Download {0} by hand into {1}, then run a Data Download.", zip, home.Length > 0 ? Path.Combine(home, "supplied") : "the home's supplied folder");
        }
    }
}
