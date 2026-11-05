// =============================================================================
// DataDownloadView.cs - the Data Download page (stage D): which datasets to fetch into Data Sources Home, and how
// =============================================================================
// Single-row sheet lucasFireC2F_DataDownloadOptions (ported from lucasCore's page). The region is the scenario's Fire Zones to Download
// sheet plus the buffer here. Datasets: one tick per dataset, then how it comes and its size, every row alike, from
// defaults/download/datasets.csv and endpoints.csv: Comes (source = clipped by its source, remote = the region read from a remote archive,
// whole = whole file fetched then clipped, states = the state files it touches kept whole, shared = one national copy for every region)
// and the sizes the P26 download measured. SizeGb (the national files) is in the tooltip. All ticked writes a blank (every one); otherwise
// the ticked columns, comma separated. The home folder is shown from the project's Data Sources.
// Home layout (HomeLayout 1 National, 2 By region) and Region name (blank = from the fire zones): the region's clips go in
// <home>/regions/<name>/, and the page lists the regions the home already holds. Fetch new editions (FetchNewEditions): a dataset the
// home holds at an older version gets the new one in <folder>/editions/<tag>/ (src/download.R).
// =============================================================================
using System;
using System.Collections.Generic;
using System.Data;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Windows.Forms;
using SyncroSim.Core;

namespace lucasFireUI
{
    public class DataDownloadView : LucasView
    {
        protected override string SheetName { get { return "lucasFireC2F_DataDownloadOptions"; } }

        private class Ds { public string Column, Name, Category, Method, Login, Note, Comes; public double SizeGb, DlMb, DiskMb; public bool Fetched; public CheckBox Box; }
        private readonly List<Ds> m_ds = new List<Ds>();
        private readonly Choice m_layout = new Choice("HomeLayout", 2);
        private readonly TextBox m_region = new TextBox { Width = 200, Margin = new Padding(3, 3, 6, 3) };
        private readonly Label m_regions = new Label { AutoSize = true, MaximumSize = new Size(680, 0), ForeColor = SystemColors.GrayText, Margin = new Padding(3, 2, 3, 4) };
        private readonly Label m_zones = new Label { AutoSize = true, Margin = new Padding(3, 7, 3, 3) };
        private readonly TextBox m_buffer = Box(60), m_from = Box(60), m_to = Box(60), m_con = Box(40), m_chv = Box(60);
        private readonly TextBox m_vintages = new TextBox { Width = 120, Margin = new Padding(3, 3, 6, 3) };
        private readonly CheckBox m_keep = new CheckBox { Text = "Keep national copies (else each whole file is clipped to the region after download)", AutoSize = true, Margin = new Padding(3, 5, 3, 3) };
        private readonly CheckBox m_editions = new CheckBox { Text = "Fetch new editions: a dataset the home holds at an older version gets the new one beside it (editions/); libraries keep reading the old one until changed on their Data Sources page", AutoSize = true, MaximumSize = new Size(680, 0), Margin = new Padding(3, 5, 3, 3) };
        private readonly CheckBox m_check = new CheckBox { Text = "Check only: list what would download, and how big, without fetching anything", AutoSize = true, Margin = new Padding(3, 5, 3, 3) };
        private readonly Label m_home = new Label { AutoSize = true, Margin = new Padding(3, 7, 3, 3), Font = new Font(SystemFonts.DefaultFont, FontStyle.Bold) };
        private readonly TextBox m_email = new TextBox { Width = 260, Margin = new Padding(3, 3, 6, 3) };
        private readonly Label m_emailNote = new Label { AutoSize = true, Margin = new Padding(3, 7, 3, 3) };
        private string m_homeDir = "";
        private readonly Label m_total = new Label { AutoSize = true, Margin = new Padding(3, 6, 3, 3), ForeColor = SystemColors.GrayText };
        private readonly ToolTip m_tip = new ToolTip { AutoPopDelay = 20000 };
        private bool m_built;

        public DataDownloadView() { }

        public override void LoadDataFeed(DataFeed dataFeed)
        {
            base.LoadDataFeed(dataFeed);
            if (!m_built) { BuildPage(); m_built = true; }
            RefreshControls();
        }

        private void BuildPage()
        {
            this.SuspendLayout();
            var where = Column();
            var hr = Row(); hr.Controls.Add(Caption("Into Data Sources Home:")); hr.Controls.Add(m_home); where.Controls.Add(hr);
            var zr = Row(); zr.Controls.Add(Caption("Fire zones:")); zr.Controls.Add(m_zones); where.Controls.Add(zr);
            var br = Row(); br.Controls.Add(Caption("Buffer around the fire zones (km):")); br.Controls.Add(m_buffer); br.Controls.Add(Note("blank = 60")); where.Controls.Add(br);
            where.Controls.Add(Wrapped("The region is this scenario's Fire Zones to Download sheet plus the buffer. 60 km holds the zone grid (25 km) and the reach of the ignition covariates (32 km past the grid); widen it if Zone Preparation's own buffer is wider. A rerun fetches only what the home folder's manifest does not yet show as done, and resumes partial files.", 680));
            var lr = Row(); lr.Controls.Add(Caption("Home layout:")); lr.Controls.Add(Option(m_layout, 2, "By region")); lr.Controls.Add(Option(m_layout, 1, "National")); where.Controls.Add(lr);
            var rn = Row(); rn.Controls.Add(Caption("Region name:")); rn.Controls.Add(m_region); rn.Controls.Add(Note("blank = from the fire zones")); where.Controls.Add(rn);
            where.Controls.Add(m_regions);
            where.Controls.Add(Wrapped("By region: the region's clips go in the home folder's regions/<name> folder, and the datasets kept whole (gridMET, FPA-FOD, MTBS perimeters, PRISM, RAWS, the zone polygons) at its top, shared by every region. National: every dataset whole at the top, for a home that serves many zones. No file in the home is ever replaced: a region that differs from one already downloaded needs its own name. A stage reads each dataset from the smallest region holding its zone. The same home folder can serve lucasCore: what the two share is fetched once.", 680));
            m_region.Validated += (s, a) => { if (!Refreshing) { string v = m_region.Text.Trim(); Write("RegionName", v.Length > 0 ? (object)v : DBNull.Value); RefreshControls(); } };
            AddGroup(Group("Region", where));

            // the datasets, grouped by kind
            var meta = new Dictionary<string, string[]>(StringComparer.OrdinalIgnoreCase);
            var ep = new Dictionary<string, string[]>(StringComparer.OrdinalIgnoreCase);
            List<string> epHead = new List<string>();
            try
            {
                foreach (string[] r in Csv.Read(Path.Combine(LibrarySetup.PackageDirectory(), "defaults", "download", "datasets.csv")).Skip(1)) if (r.Length >= 5) meta[r[0]] = r;
                List<string[]> e = Csv.Read(Path.Combine(LibrarySetup.PackageDirectory(), "defaults", "download", "endpoints.csv"));
                epHead = e[0].ToList(); foreach (string[] r in e.Skip(1)) ep[r[0]] = r;
            }
            catch (Exception) { }
            Func<string[], string, string> col = (r, k) => { int i = epHead.IndexOf(k); return r != null && i >= 0 && i < r.Length ? r[i] : ""; };
            foreach (var kv in meta)
            {
                string[] e; ep.TryGetValue(kv.Key, out e);
                Func<string, double> num = k => { double v; return double.TryParse(col(e, k), System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture, out v) ? v : 0; };
                m_ds.Add(new Ds { Column = kv.Key, Name = kv.Value[4], Category = kv.Value[1], Method = col(e, "Method"), Login = col(e, "Login"), Note = col(e, "Note"), Comes = col(e, "Comes"),
                                  SizeGb = num("SizeGb"), DlMb = num("P26DownloadMb"), DiskMb = num("P26DiskMb"), Fetched = string.Equals(col(e, "Fetched"), "yes", StringComparison.OrdinalIgnoreCase) });
            }
            var list = Column();
            var quick = Row();
            var all = new Button { Text = "Tick all", AutoSize = true }; var none = new Button { Text = "Clear", AutoSize = true };
            quick.Controls.Add(all); quick.Controls.Add(none); quick.Controls.Add(m_total); list.Controls.Add(quick);
            list.Controls.Add(Wrapped("Sizes are those of the P26 download (Pyrome P26 + 60 km): a clipped dataset grows with the region's area. Shared national copies are fetched once per home folder, whatever the region. Hover over a dataset for its national size and notes.", 680));
            foreach (string cat in m_ds.Select(d => d.Category).Distinct().ToList())
            {
                list.Controls.Add(new Label { Text = cat, AutoSize = true, Font = new Font(this.Font, FontStyle.Bold), Margin = new Padding(3, 8, 3, 2) });
                foreach (Ds d in m_ds.Where(x => x.Category == cat))
                {
                    d.Box = new CheckBox { Text = d.Name + " - " + Comes(d), AutoSize = true, Enabled = d.Fetched, Margin = new Padding(18, 2, 3, 2) };
                    m_tip.SetToolTip(d.Box, d.Column + (d.Method.Length > 0 ? " (" + Method(d.Method) + ")" : "") + (d.SizeGb > 0 ? Environment.NewLine + "National files: " + SizeText(d.SizeGb * 1000) : "") + (d.Note.Length > 0 ? Environment.NewLine + d.Note : ""));
                    d.Box.CheckedChanged += (s, a) => { if (!Refreshing) WriteDatasets(); };
                    list.Controls.Add(d.Box);
                }
            }
            all.Click += (s, a) => { Refreshing = true; foreach (Ds d in m_ds.Where(x => x.Fetched)) d.Box.Checked = true; Refreshing = false; WriteDatasets(); };
            none.Click += (s, a) => { Refreshing = true; foreach (Ds d in m_ds) d.Box.Checked = false; Refreshing = false; WriteDatasets(); };
            AddGroup(Group("Datasets", list));

            var yr = Column();
            var lv = Row(); lv.Controls.Add(Caption("LANDFIRE vintages:")); lv.Controls.Add(m_vintages); lv.Controls.Add(Note("comma separated; blank = 2020")); yr.Controls.Add(lv);
            var cv = Row(); cv.Controls.Add(Caption("Canopy height vintage:")); cv.Controls.Add(m_chv); cv.Controls.Add(Note("blank = the vintages above; as Zone Preparation's Canopy Height Vintage")); yr.Controls.Add(cv);
            var y = Row(); y.Controls.Add(Caption("Years:")); y.Controls.Add(m_from); y.Controls.Add(Caption("to")); y.Controls.Add(m_to);
            y.Controls.Add(Note("the annual datasets (gridMET, MTBS severity); blank = 1979 to the latest each source has")); yr.Controls.Add(y);
            AddGroup(Group("Vintages and years", yr));

            var how = Column(); how.Controls.Add(m_check); how.Controls.Add(m_keep); how.Controls.Add(m_editions);
            var c = Row(); c.Controls.Add(Caption("Downloads at once:")); c.Controls.Add(m_con); c.Controls.Add(Note("blank = 4")); how.Controls.Add(c);
            var em = Row(); em.Controls.Add(Caption("LANDFIRE email:")); em.Controls.Add(m_email); em.Controls.Add(m_emailNote); how.Controls.Add(em);
            how.Controls.Add(Wrapped("The LANDFIRE Product Service clips the LANDFIRE layers to the region and needs an email address with each job. It is kept in the home folder's settings.csv, not in the scenario, so every download into that folder uses it.", 680));
            AddGroup(Group("How", how));
            EndGroups();
            m_email.Validated += (s, a) => { if (!Refreshing) SaveEmail(); };
            m_vintages.Validated += (s, a) => { if (!Refreshing) { string v = m_vintages.Text.Trim(); Write("LandfireVintages", v.Length > 0 ? (object)v : DBNull.Value); RefreshControls(); } };

            BindNumber(m_buffer, () => Sheet, "BufferKm", false); BindNumber(m_from, () => Sheet, "YearFrom", true); BindNumber(m_chv, () => Sheet, "CanopyHeightVintage", true);
            BindNumber(m_to, () => Sheet, "YearTo", true); BindNumber(m_con, () => Sheet, "Connections", true);
            m_keep.CheckedChanged += (s, a) => { if (!Refreshing) Write("KeepNational", m_keep.Checked); };
            m_check.CheckedChanged += (s, a) => { if (!Refreshing) Write("CheckOnly", m_check.Checked); };
            m_editions.CheckedChanged += (s, a) => { if (!Refreshing) Write("FetchNewEditions", m_editions.Checked); };
            this.ResumeLayout(false);
        }

        // the same two parts on every row: how the dataset comes, then its size (P26 measured)
        private static string SizeText(double mb) { return mb < 1 ? "under 1 MB" : mb < 1000 ? string.Format("{0:0} MB", mb) : string.Format("{0:0.#} GB", mb / 1000); }
        private static string Comes(Ds d)
        {
            switch (d.Comes)
            {
                case "source": return "clipped by its source; P26: " + SizeText(d.DiskMb);
                case "remote": return "the region read from a remote archive; P26: " + SizeText(d.DiskMb);
                case "whole": return string.Format("whole file fetched, then clipped; P26: {0} download, {1} kept", SizeText(d.DlMb), SizeText(d.DiskMb));
                case "archive": return "LF2020: five zips you download once (hover for how), then clipped; other vintages clipped by LANDFIRE";
                case "states": return string.Format("the states the region touches, fetched whole; P26: {0} download, {1} kept", SizeText(d.DlMb), SizeText(d.DiskMb));
                case "shared": return string.Format("one national copy shared by every region: {0} download{1}, once", SizeText(d.DlMb), d.DiskMb > 0 ? ", " + SizeText(d.DiskMb) + " on disk" : "");
                default: return d.Fetched ? "fetched" : "not fetched by this version";
            }
        }

        private static string Method(string m)
        {
            switch (m) { case "wcs": return "web coverage service"; case "lfps": return "LANDFIRE Product Service"; case "rest": return "web service"; default: return m; }
        }

        // every dataset ticked = blank (every one); else the ticked columns
        private void WriteDatasets()
        {
            List<Ds> f = m_ds.Where(d => d.Fetched).ToList();
            bool every = f.All(d => d.Box.Checked);
            Write("Datasets", every ? (object)DBNull.Value : string.Join(", ", m_ds.Where(d => d.Box.Checked).Select(d => d.Column)));
            RefreshControls();
        }

        // <home>/settings.csv: Key,Value rows (src/datahome.R home_setting); the email lives there, not in the scenario
        private string Setting(string key)
        {
            try { string f = Path.Combine(m_homeDir, "settings.csv"); if (m_homeDir.Length == 0 || !File.Exists(f)) return "";
                  foreach (string[] r in Csv.Read(f).Skip(1)) if (r.Length >= 2 && r[0] == key) return r[1]; }
            catch (Exception) { }
            return "";
        }

        private void SaveEmail()
        {
            string v = m_email.Text.Trim();
            if (v.Length > 0 && !v.Contains("@")) { MessageBox.Show("That does not look like an email address.", "LANDFIRE email", MessageBoxButtons.OK, MessageBoxIcon.Warning); return; }
            SaveSetting("LfpsEmail", v);
        }

        // a blank value removes the key; other keys are kept
        private void SaveSetting(string key, string value)
        {
            if (m_homeDir.Length == 0) return;
            try
            {
                string f = Path.Combine(m_homeDir, "settings.csv");
                var rows = File.Exists(f) ? Csv.Read(f).Skip(1).Where(r => r.Length >= 2 && r[0] != key).Select(r => r[0] + "," + r[1]).ToList() : new List<string>();
                if (value.Length > 0) rows.Add(key + "," + value);
                Directory.CreateDirectory(m_homeDir); File.WriteAllLines(f, new[] { "Key,Value" }.Concat(rows));
            }
            catch (Exception ex) { MessageBox.Show("Could not save " + key + " in the home folder: " + ex.Message, "Data Download", MessageBoxButtons.OK, MessageBoxIcon.Warning); }
            RefreshControls();
        }

        protected override void ShowData()
        {
            string home = "";
            try { DataSheet src = this.DataFeed.Project.GetDataSheet("lucasFireC2F_DataSource"); if (src.HasData() && src.GetData().Columns.Contains("DataSourcesHome")) home = Show(src.GetDataRow()["DataSourcesHome"]).Trim(); }
            catch (Exception) { }
            m_home.Text = home.Length > 0 ? home : "not set: set it on the Data Sources page (project)";
            m_home.ForeColor = home.Length > 0 ? SystemColors.ControlText : Color.Firebrick;
            m_homeDir = home; m_email.Enabled = home.Length > 0; m_email.Text = Setting("LfpsEmail");
            int zones = 0;
            try { DataTable zt = this.DataFeed.Scenario.GetDataSheet("lucasFireC2F_DataDownloadZone").GetData(); zones = zt.Rows.Cast<DataRow>().Count(r => r.RowState != DataRowState.Deleted); }
            catch (Exception) { }
            bool byRegion = IntValue("HomeLayout", 2) == 2;
            m_zones.Text = !byRegion ? "National: every zone" : zones == 0 ? "none yet: list them on the Fire Zones to Download sheet" : string.Format("{0} on the Fire Zones to Download sheet", zones);
            m_zones.ForeColor = byRegion && zones == 0 ? Color.Firebrick : SystemColors.ControlText;
            ShowChoice(m_layout); m_region.Text = Show(Value("RegionName")); m_region.Enabled = byRegion;
            string[] regs = new string[0];
            try { string rd = Path.Combine(home, "regions"); if (home.Length > 0 && Directory.Exists(rd)) regs = Directory.GetDirectories(rd).Select(Path.GetFileName).OrderBy(x => x).ToArray(); }
            catch (Exception) { }
            m_regions.Text = home.Length == 0 ? "" : regs.Length == 0 ? "This home holds no regions yet." : "Regions in this home: " + string.Join(", ", regs) + " (the same name adds to that region only if its zones match)";
            m_buffer.Text = Show(Value("BufferKm")); m_from.Text = Show(Value("YearFrom")); m_to.Text = Show(Value("YearTo")); m_con.Text = Show(Value("Connections"));
            m_vintages.Text = Show(Value("LandfireVintages")); m_chv.Text = Show(Value("CanopyHeightVintage"));
            object k = Value("KeepNational"); m_keep.Checked = k != DBNull.Value && Convert.ToBoolean(k);
            object c = Value("CheckOnly"); m_check.Checked = c != DBNull.Value && Convert.ToBoolean(c);
            object ne = Value("FetchNewEditions"); m_editions.Checked = ne != DBNull.Value && Convert.ToBoolean(ne);
            string ds = Show(Value("Datasets")).Trim();
            var picked = new HashSet<string>(ds.Split(',').Select(x => x.Trim()).Where(x => x.Length > 0), StringComparer.OrdinalIgnoreCase);
            foreach (Ds d in m_ds) d.Box.Checked = d.Fetched && (ds.Length == 0 || picked.Contains(d.Column));
            var on = m_ds.Where(d => d.Box.Checked).ToList(); var reg = on.Where(d => d.Comes != "shared").ToList(); var sh = on.Where(d => d.Comes == "shared").ToList();
            m_total.Text = string.Format("{0} ticked. A P26-sized region: about {1} to download, {2} kept{3}", on.Count, SizeText(reg.Sum(d => d.DlMb)), SizeText(reg.Sum(d => d.DiskMb)),
                sh.Count == 0 ? "" : string.Format("; shared copies {0} to download ({1} on disk), the first time only", SizeText(sh.Sum(d => d.DlMb)), SizeText(sh.Sum(d => d.DiskMb))));
            bool lf = m_ds.Any(d => d.Box.Checked && d.Method == "lfps"), hasEmail = m_email.Text.Contains("@");
            m_emailNote.Text = home.Length == 0 ? "set Data Sources Home first" : hasEmail ? "" : lf ? "needed: LANDFIRE layers are ticked" : "needed only for the LANDFIRE layers";
            m_emailNote.ForeColor = lf && !hasEmail ? Color.Firebrick : SystemColors.GrayText;
        }
    }
}
