// =============================================================================
// DataSourcesView.cs - the Data Sources page of the project: the national data paths, grouped by kind, with where to get each
// =============================================================================
// Ported from lucasCore's page, with each column's home Dataset (the file's Dataset column) for the manifest and editions. Single-row project sheet lucasFireC2F_DataSource. defaults/library/data-sources.csv gives each column its category, the dataset it
// holds, a download page, the name the page shows and an optional note; a column the file does not list goes under "Other". One row per
// dataset (user, 2026-10-28): its name (else the column's display name without bracketed detail) with the note in grey under it, and
// the column's full display name, source and link in a tooltip; the path; a Browse button (a folder dialog for *Folder columns, a file
// dialog otherwise); a Source page button (the download page); and a mark: green when the data is usable, red with the reason on the
// line below when it is not (HomeCheck.cs). A blank path reads the home. The file's Access column marks "built" the optional grids the
// stages make themselves: they are not source data, so they sit in a collapsed Advanced group, and a blank one is neither good nor bad.
// Usable means here, for the fire zones this project's scenarios name, by the stages' own coverage rule; the LANDFIRE row lists its layers.
// Data Sources Home (src/datahome.R): one folder holding every dataset at its place (the file's HomePath column). A blank path
// whose place holds the data can be read from there; Make folders creates the empty layout. The home folder's manifest.csv, written by the Data
// Download stage, gives each dataset's status and the region it covers. The home is looked for at its top and in every regions/<name>
// folder (By region layout); the mark's tooltip names where, and the stages themselves pick the smallest region holding their zone. Editions (src/datahome.R): a dataset whose folder also holds
// editions/<tag>/ copies gets a drop-down of them; the choice, confirmed, is written to lucasFireC2F_DataSourceEdition (blank Edition =
// the first edition), the only place a library's edition changes.
// =============================================================================
using System;
using System.Collections.Generic;
using System.Data;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Windows.Forms;
using SyncroSim.Core;

namespace lucasFireUI
{
    public class DataSourcesView : LucasView
    {
        protected override string SheetName { get { return "lucasFireC2F_DataSource"; } }

        private class Entry { public string Column, Category, Source, Url, Name, Note, Access, HomePath, Dataset; public TextBox Path; public Label State, Why; public ComboBox Edition; }
        private class Layer { public string Product, Name; public Label Where, State, Why; public bool Good; }
        private static readonly string[][] Layers = { new[] { "FBFM40", "Fuel model" }, new[] { "CBD", "Canopy bulk density" }, new[] { "CBH", "Canopy base height" }, new[] { "CC", "Canopy cover" }, new[] { "CH", "Canopy height" } };
        private const string LandfireColumn = "LandfireFolder", LandfirePlace = "fuels/landfire";
        private class Ed { public string Tag, Text; public override string ToString() { return Text; } }
        private const string EditionSheet = "lucasFireC2F_DataSourceEdition";
        private const string HomeColumn = "DataSourcesHome";
        private TextBox m_home; private Label m_homeState, m_zonesNote;
        private readonly List<Layer> m_layers = new List<Layer>();
        private CheckBox m_advanced; private TableLayoutPanel m_advancedTable; private bool m_advancedTouched;
        private Dictionary<int, double[]> m_boxes;
        private readonly List<Entry> m_entries = new List<Entry>();
        private readonly List<TableLayoutPanel> m_tables = new List<TableLayoutPanel>();
        private bool m_built;

        public DataSourcesView() { }

        public override void LoadDataFeed(DataFeed dataFeed)
        {
            base.LoadDataFeed(dataFeed);
            if (!m_built) { BuildPage(); m_built = true; }
            RefreshControls();
        }

        // the groups are made once the sheet's columns are known
        private void BuildPage()
        {
            this.SuspendLayout();
            var meta = new Dictionary<string, string[]>(StringComparer.OrdinalIgnoreCase);
            try
            {
                List<string[]> rows = Csv.Read(Path.Combine(LibrarySetup.PackageDirectory(), "defaults", "library", "data-sources.csv"));
                foreach (string[] r in rows.Skip(1)) if (r.Length >= 4) meta[r[0]] = r;
            }
            catch (Exception) { }
            var intro = Column();
            intro.Controls.Add(Note("The source data the stages read. Set Data Sources Home to the folder a Data Download scenario fills, and leave a path blank to read the dataset from there. To use a copy you already have, set its path instead. Green means the data is ready to use; red means it is not, with the reason underneath. Source page opens the dataset's web page; hover a name or a mark for more."));
            if (Sheet.Columns.Cast<DataSheetColumn>().Any(c => c.Name == HomeColumn)) intro.Controls.Add(HomeRow());
            m_zonesNote = Note(""); intro.Controls.Add(m_zonesNote);
            AddGroup(Group("Data Sources", intro));

            DataSheet ds = Sheet;
            var cols = ds.Columns.Cast<DataSheetColumn>().Where(c => c.IsVisible && c.Name != "DataSourceId" && c.Name != "ProjectId" && c.Name != HomeColumn && c.DataType == typeof(string)).ToList();
            foreach (DataSheetColumn c in cols)
            {
                string[] m; meta.TryGetValue(c.Name, out m);
                Func<int, string> at = k => m != null && m.Length > k && m[k].Length > 0 ? m[k] : null;
                m_entries.Add(new Entry { Column = c.Name, Category = at(1) ?? "Other", Source = at(2), Url = at(3), Name = at(4), Note = at(5), Access = at(6), HomePath = at(10), Dataset = at(11) ?? c.Name });
            }
            var order = meta.Values.Select(m => m[1]).Distinct().ToList(); order.Add("Other");
            foreach (string cat in order)
            {
                List<Entry> es = m_entries.Where(e => e.Category == cat).ToList();
                if (es.Count == 0) continue;
                // a fixed name width, and path boxes all sized together to the window (SizePaths), so every group's columns line up
                var t = new TableLayoutPanel { AutoSize = true, ColumnCount = 6 }; m_tables.Add(t);
                t.ColumnStyles.Add(new ColumnStyle(SizeType.AutoSize));
                t.ColumnStyles.Add(new ColumnStyle(SizeType.Absolute, NameWidth));
                t.ColumnStyles.Add(new ColumnStyle(SizeType.AutoSize));
                t.ColumnStyles.Add(new ColumnStyle(SizeType.AutoSize));
                t.ColumnStyles.Add(new ColumnStyle(SizeType.AutoSize));
                t.ColumnStyles.Add(new ColumnStyle(SizeType.AutoSize));
                foreach (Entry e in es) { AddEntry(t, e, cols.First(c => c.Name == e.Column).DisplayName); if (e.Column == LandfireColumn) foreach (string[] l in Layers) AddLayer(t, l[0], l[1]); }
                if (es.All(e => e.Access == "built"))
                {
                    // not source data: shown on request, or when one of them holds a path
                    var box = Column(); m_advanced = new CheckBox { Text = "Show", AutoSize = true, Margin = new Padding(3, 3, 3, 3) }; m_advancedTable = t; t.Visible = false;
                    m_advanced.CheckedChanged += (s, a) => { if (!Refreshing) m_advancedTouched = true; m_advancedTable.Visible = m_advanced.Checked; SizePaths(); };
                    box.Controls.Add(Note("These are not source data. The stages build them for you; set a path only to reuse a grid you already have."));
                    box.Controls.Add(m_advanced); box.Controls.Add(t);
                    AddGroup(Group(cat, box));
                }
                else AddGroup(Group(cat, t));
            }
            EndGroups();
            this.Resize += (s, a) => SizePaths();
            this.ResumeLayout(false);
            SizePaths();
        }

        // name column, and the room the buttons, group borders and scroll bar take beside the path box (the mark and an edition
        // drop-down are measured in SizePaths)
        private const int NameWidth = 280, Beside = 280, MinPath = 220;

        // every path box takes the width the window leaves (at least MinPath), so Browse, Download, the whole mark ("✓ home:
        // regions/<name>") and an edition drop-down stay in view; called again whenever the marks change
        private void SizePaths()
        {
            int mark = m_entries.Count == 0 ? 90 : m_entries.Max(e => e.State.PreferredSize.Width + e.State.Margin.Horizontal);
            int ed = m_entries.Any(e => e.Edition != null && e.Edition.Visible) ? 240 : 0;
            int w = Math.Max(MinPath, this.ClientSize.Width - NameWidth - Beside - mark - ed);
            foreach (Entry e in m_entries) if (e.Path != null && e.Path.Width != w) e.Path.Width = w;
            // then fit what is on screen: with display scaling the name column and buttons are wider than the constants say, so
            // the boxes give up whatever a group still runs past the page's right edge
            int over = 0;
            foreach (TableLayoutPanel t in m_tables)
            {
                if (!t.IsHandleCreated || !this.IsHandleCreated) continue;
                int left = this.PointToClient(t.PointToScreen(Point.Empty)).X;
                over = Math.Max(over, left + t.GetPreferredSize(Size.Empty).Width + SystemInformation.VerticalScrollBarWidth + 16 - this.ClientSize.Width);
            }
            if (over > 0) { w = Math.Max(MinPath, w - over); foreach (Entry e in m_entries) if (e.Path != null && e.Path.Width != w) e.Path.Width = w; }
        }
        private readonly ToolTip m_tip = new ToolTip { AutoPopDelay = 20000 };

        // the dataset's name without the detail its column name carries in brackets or after " - " (that goes in the tooltip)
        private static string ShortName(string displayName)
        {
            string s = System.Text.RegularExpressions.Regex.Replace(displayName, @"\s*\([^)]*\)", "");
            int k = s.IndexOf(" - ", StringComparison.Ordinal); if (k > 0) s = s.Substring(0, k);
            return s.Trim();
        }

        // one row per dataset: name, path, Browse, Download, found mark
        private void AddEntry(TableLayoutPanel t, Entry e, string displayName)
        {
            int r = t.RowCount; t.RowCount = r + 1; t.RowStyles.Add(new RowStyle(SizeType.AutoSize));
            // the name from data-sources.csv (else the column's display name, shortened), a grey note under it when the file gives one
            var cell = new FlowLayoutPanel { FlowDirection = FlowDirection.TopDown, WrapContents = false, AutoSize = true, Anchor = AnchorStyles.Left, Margin = new Padding(0, 3, 9, 3) };
            var name = new Label { Text = e.Name ?? ShortName(displayName), AutoSize = true, MaximumSize = new Size(NameWidth - 12, 0), Tag = Fit.Fixed, Margin = new Padding(3, 4, 3, 0) };
            cell.Controls.Add(name);
            if (e.Note != null) cell.Controls.Add(new Label { Text = e.Note, AutoSize = true, MaximumSize = new Size(NameWidth - 12, 0), Tag = Fit.Fixed, ForeColor = SystemColors.GrayText, Margin = new Padding(3, 1, 3, 0) });
            m_tip.SetToolTip(name, displayName + (e.Source != null ? Environment.NewLine + "Source: " + e.Source : "") + (e.Url != null ? Environment.NewLine + e.Url : ""));
            e.Path = new TextBox { Width = MinPath, Anchor = AnchorStyles.Left, Margin = new Padding(3, 4, 3, 3) };
            var browse = new Button { Text = "Browse...", AutoSize = true, Margin = new Padding(3, 3, 3, 3) };
            var download = new Button { Text = "Source page", AutoSize = true, Margin = new Padding(3, 3, 3, 3), Enabled = !string.IsNullOrEmpty(e.Url) && e.Access != "user" };
            if (download.Enabled)
            {
                string url = e.Url;
                m_tip.SetToolTip(download, "Opens " + (e.Source ?? url) + Environment.NewLine + url);
                download.Click += (s, a) => { try { Process.Start(url); } catch (Exception) { } };
            }
            e.State = new Label { AutoSize = true, Anchor = AnchorStyles.Left, Margin = new Padding(6, 7, 3, 3), Font = new Font(this.Font, FontStyle.Bold) };
            e.Edition = new ComboBox { DropDownStyle = ComboBoxStyle.DropDownList, Width = 230, Visible = false, Anchor = AnchorStyles.Left, Margin = new Padding(3, 4, 3, 3) };
            t.Controls.Add(cell, 0, r); t.Controls.Add(e.Path, 1, r); t.Controls.Add(browse, 2, r); t.Controls.Add(download, 3, r); t.Controls.Add(e.State, 4, r); t.Controls.Add(e.Edition, 5, r);
            e.Why = WhyLine(t);

            Entry en = e;
            e.Path.Validated += (s, a) => { if (Refreshing) return; string v = en.Path.Text.Trim(); Write(en.Column, v.Length == 0 ? (object)DBNull.Value : v); RefreshControls(); };
            browse.Click += (s, a) => Browse(en);
            e.Edition.SelectionChangeCommitted += (s, a) => ChooseEdition(en);
        }

        // the reason a mark is red, on its own line under the row's path (hidden, and taking no room, when the mark is not red)
        private static Label WhyLine(TableLayoutPanel t)
        {
            int r = t.RowCount; t.RowCount = r + 1; t.RowStyles.Add(new RowStyle(SizeType.AutoSize));
            var why = new Label { AutoSize = true, MaximumSize = new Size(640, 0), ForeColor = Color.Firebrick, Margin = new Padding(3, 0, 3, 6), Visible = false };
            t.Controls.Add(why, 1, r); t.SetColumnSpan(why, 5);
            return why;
        }

        // one LANDFIRE layer under its dataset's row: name, the vintage and where it is, a mark, the reason. Nothing to set: the layers follow the row's path or the home
        private void AddLayer(TableLayoutPanel t, string product, string name)
        {
            int r = t.RowCount; t.RowCount = r + 1; t.RowStyles.Add(new RowStyle(SizeType.AutoSize));
            var l = new Layer { Product = product, Name = name };
            t.Controls.Add(new Label { Text = string.Format("{0} ({1})", name, product), AutoSize = true, Anchor = AnchorStyles.Left, MaximumSize = new Size(NameWidth - 30, 0), Tag = Fit.Fixed, Margin = new Padding(21, 3, 3, 3) }, 0, r);
            l.Where = new Label { AutoSize = true, Anchor = AnchorStyles.Left, ForeColor = SystemColors.GrayText, Tag = Fit.Fixed, Margin = new Padding(3, 3, 3, 3) };
            t.Controls.Add(l.Where, 1, r); t.SetColumnSpan(l.Where, 3);
            l.State = new Label { AutoSize = true, Anchor = AnchorStyles.Left, Margin = new Padding(6, 3, 3, 3), Font = new Font(this.Font, FontStyle.Bold) };
            t.Controls.Add(l.State, 4, r);
            l.Why = WhyLine(t);
            m_layers.Add(l);
        }

        // the mark: green, red with its reason, or grey (a blank optional grid: neither good nor bad)
        private void Mark(Label state, Label why, bool? good, string text, string reason, string tip)
        {
            state.Text = text; state.ForeColor = good == true ? Color.ForestGreen : good == false ? Color.Firebrick : SystemColors.GrayText;
            m_tip.SetToolTip(state, tip ?? "");
            why.Text = reason ?? ""; why.Visible = good == false && !string.IsNullOrEmpty(reason);
        }

        // the fire zones and LANDFIRE vintages this project's scenarios ask for (their results are not asked again). A zone's need is the stages' own
        // (study_box): its Zone Preparation buffer, else the default. No vintage asked = 2020, as the stages
        private List<HomeCheck.Zone> ScenarioZones(HashSet<string> fuels, HashSet<string> heights)
        {
            var zones = new Dictionary<string, HomeCheck.Zone>();
            try
            {
                if (m_boxes == null) m_boxes = HomeCheck.ZoneBoxes(Path.Combine(LibrarySetup.PackageDirectory(), "defaults", "download", "fire-zone-boxes.csv"));
                DataTable fz = this.DataFeed.Project.GetDataSheet("lucasFireC2F_FireZone").GetData();
                Action<object, double> add = (id, km) =>
                {
                    if (id == null || id == DBNull.Value) return;
                    DataRow r = fz.Rows.Cast<DataRow>().FirstOrDefault(x => x.RowState != DataRowState.Deleted && Convert.ToString(x["FireZoneId"]) == Convert.ToString(id)); if (r == null) return;
                    string name = Convert.ToString(r["Name"]); HomeCheck.Zone z;
                    if (!zones.TryGetValue(name, out z)) { double[] b = null; if (r["ID"] != DBNull.Value) m_boxes.TryGetValue(Convert.ToInt32(r["ID"]), out b); z = new HomeCheck.Zone { Name = name, Box = b, GridKm = km }; zones[name] = z; }
                    z.GridKm = Math.Max(z.GridKm, km);
                };
                Func<DataRow, string, string> text = (x, k) => x.Table.Columns.Contains(k) && x[k] != DBNull.Value ? Convert.ToString(x[k]).Trim() : "";
                foreach (Scenario s in this.DataFeed.Project.Scenarios.Cast<Scenario>().Where(x => !x.IsResult))
                {
                    foreach (DataRow x in s.GetDataSheet("lucasFireC2F_DataDownloadZone").GetData().Rows.Cast<DataRow>().Where(q => q.RowState != DataRowState.Deleted)) add(x["FireZone"], HomeCheck.DefaultGridKm);
                    foreach (DataRow x in s.GetDataSheet("lucasFireC2F_ZonePrepOption").GetData().Rows.Cast<DataRow>().Where(q => q.RowState != DataRowState.Deleted))
                    {
                        string km = text(x, "BufferKm"); add(x["FireZone"], km.Length > 0 ? Convert.ToDouble(km) : HomeCheck.DefaultGridKm);
                        string v = text(x, "LandfireVintage"); if (v.Length == 0) v = "2020"; fuels.Add(v); string c = text(x, "CanopyHeightVintage"); heights.Add(c.Length > 0 ? c : v);
                    }
                    foreach (DataRow x in s.GetDataSheet("lucasFireC2F_DataDownloadOptions").GetData().Rows.Cast<DataRow>().Where(q => q.RowState != DataRowState.Deleted))
                    {
                        string vs = text(x, "LandfireVintages"); List<string> list = vs.Length > 0 ? vs.Split(',').Select(q => q.Trim()).Where(q => q.Length > 0).ToList() : new List<string> { "2020" };
                        string c = text(x, "CanopyHeightVintage"); foreach (string v in list) { fuels.Add(v); heights.Add(c.Length > 0 ? c : v); }
                    }
                }
            }
            catch (Exception) { }
            if (fuels.Count == 0) fuels.Add("2020");
            if (heights.Count == 0) heights.UnionWith(fuels);
            return zones.Values.OrderBy(z => z.Name, StringComparer.OrdinalIgnoreCase).ToList();
        }

        // one LANDFIRE layer: each vintage asked must be there, in a folder covering every zone
        private void ShowLayer(Layer l, string home, string path, List<string> folders, List<HomeCheck.Zone> zones, IEnumerable<string> vintages)
        {
            var vs = vintages.OrderBy(v => v).ToList(); var said = new List<string>(); var tips = new List<string>(); string reason = null, text = null;
            foreach (string v in vs)
            {
                if (path.Length > 0)
                {
                    string f = HomeCheck.LayerFile(path, v, l.Product);
                    if (f == null) { reason = reason ?? string.Format("No LF{0} {1} under the path set above.", v, l.Product); text = text ?? "✗ Not found"; said.Add("LF" + v); }
                    else { said.Add("LF" + v); tips.Add(f); }
                    continue;
                }
                var holders = new List<Dictionary<string, string>>(); var where = new List<string>();
                foreach (string d in folders)
                {
                    string f = HomeCheck.LayerFile(Path.Combine(d, LandfirePlace), v, l.Product); if (f == null) continue;
                    Dictionary<string, string> r; Manifest(d).TryGetValue(LandfireColumn, out r); holders.Add(r); where.Add(FolderLabel(home, d)); tips.Add(f);
                }
                said.Add("LF" + v + (where.Count == 1 && where[0] != "the top" ? ": " + where[0] : where.Count > 1 ? string.Format(": {0} folders", where.Count) : ""));
                if (holders.Count == 0) { reason = reason ?? HomeCheck.LayerWhy(home, v, l.Product); text = text ?? "✗ Not downloaded yet"; continue; }
                HomeCheck.Zone z = HomeCheck.Uncovered(zones, holders);
                if (z != null) { reason = reason ?? HomeCheck.Why(z, holders); text = text ?? "✗ Doesn't cover zone"; }
            }
            l.Where.Text = string.Join(", ", said); l.Good = reason == null;
            Mark(l.State, l.Why, l.Good, l.Good ? "✓ Good" : text, reason, string.Join(Environment.NewLine, tips));
        }

        // the folders of the home: its top, then every regions/<name> folder
        private static List<string> Folders(string home)
        {
            var f = new List<string>(); if (home.Length == 0) return f;
            f.Add(home);
            try { string r = Path.Combine(home, "regions"); if (Directory.Exists(r)) f.AddRange(Directory.GetDirectories(r).OrderBy(x => x, StringComparer.OrdinalIgnoreCase)); } catch (Exception) { }
            return f;
        }
        private static string FolderLabel(string home, string dir) { return string.Equals(Path.GetFullPath(dir).TrimEnd('\\'), Path.GetFullPath(home).TrimEnd('\\'), StringComparison.OrdinalIgnoreCase) ? "the top" : "regions/" + Path.GetFileName(dir); }

        // the editions of a dataset in the folders that hold it: editions/<tag>/ with a done manifest row for the column
        private static List<Ed> Editions(string home, string column, out string firstVersion)
        {
            var eds = new List<Ed>(); firstVersion = "";
            foreach (string f in Folders(home))
            {
                Dictionary<string, string> m; if (Manifest(f).TryGetValue(column, out m) && firstVersion.Length == 0) firstVersion = Get(m, "Version");
                string ed = Path.Combine(f, "editions"); if (!Directory.Exists(ed)) continue;
                foreach (string d in Directory.GetDirectories(ed))
                {
                    Dictionary<string, string> r; if (!Manifest(d).TryGetValue(column, out r) || Get(r, "Status") != "done") continue;
                    string tag = Path.GetFileName(d);
                    if (!eds.Any(x => x.Tag == tag)) eds.Add(new Ed { Tag = tag, Text = string.Format("{0} ({1})", Get(r, "Version"), Get(r, "Downloaded").Split('T')[0]) });
                }
            }
            return eds;
        }

        // the edition this library reads: its Data Source Editions row (blank or none = the first)
        private string EditionOf(string column)
        {
            try
            {
                DataTable dt = this.DataFeed.Project.GetDataSheet(EditionSheet).GetData();
                DataRow r = dt.Rows.Cast<DataRow>().FirstOrDefault(x => x.RowState != DataRowState.Deleted && Convert.ToString(x["DataSource"]) == column);
                return r == null || r["Edition"] == DBNull.Value ? "" : Convert.ToString(r["Edition"]);
            }
            catch (Exception) { return ""; }
        }

        // a change of edition is the user's decision: confirmed, then written (blank = back to the first)
        private void ChooseEdition(Entry e)
        {
            var pick = e.Edition.SelectedItem as Ed; if (pick == null) return;
            string now = EditionOf(e.Dataset);
            if (pick.Tag == now) return;
            string from = now.Length == 0 ? "the first edition" : "edition " + now, to = pick.Tag.Length == 0 ? "the first edition" : "edition " + pick.Tag;
            if (MessageBox.Show(this, string.Format("Read {0} for {1} instead of {2}?" + Environment.NewLine + Environment.NewLine + "This library's results were made from {2}. The next Prep run reads {0}, so its results will differ from the ones before.", to, e.Name ?? e.Column, from),
                                "Change edition", MessageBoxButtons.YesNo, MessageBoxIcon.Question) != DialogResult.Yes) { RefreshControls(); return; }
            try
            {
                DataSheet es = this.DataFeed.Project.GetDataSheet(EditionSheet); DataTable dt = es.GetData();
                DataRow r = dt.Rows.Cast<DataRow>().FirstOrDefault(x => x.RowState != DataRowState.Deleted && Convert.ToString(x["DataSource"]) == e.Dataset);
                object v = pick.Tag.Length == 0 ? (object)DBNull.Value : pick.Tag;
                if (r != null) { es.BeginModifyRows(); try { r["Edition"] = v; } finally { es.EndModifyRows(); } }
                else { es.BeginAddRows(); try { DataRow n = dt.NewRow(); n["DataSource"] = e.Dataset; n["Edition"] = v; dt.Rows.Add(n); } finally { es.EndAddRows(); } }
            }
            catch (Exception ex) { MessageBox.Show(this, "Could not save the edition: " + ex.Message, "Change edition", MessageBoxButtons.OK, MessageBoxIcon.Warning); }
            RefreshControls();
        }

        // the home folder: path, Browse, Make folders, and a mark
        private Control HomeRow()
        {
            var row = Row(); row.WrapContents = false; row.Margin = new Padding(0, 6, 0, 0);
            var cap = Caption("Data Sources Home"); m_tip.SetToolTip(cap, "One folder holding every dataset at its standard place: a blank path below reads its place here.");
            m_home = new TextBox { Width = 420, Anchor = AnchorStyles.Left, Margin = new Padding(3, 4, 3, 3) };
            var browse = new Button { Text = "Browse...", AutoSize = true };
            var make = new Button { Text = "Make folders", AutoSize = true };
            m_tip.SetToolTip(make, "Creates the empty folder layout in the home folder (nothing is moved or deleted)");
            m_homeState = new Label { AutoSize = true, Anchor = AnchorStyles.Left, Margin = new Padding(6, 7, 3, 3), Font = new Font(this.Font, FontStyle.Bold) };
            row.Controls.Add(cap); row.Controls.Add(m_home); row.Controls.Add(browse); row.Controls.Add(make); row.Controls.Add(m_homeState);
            m_home.Validated += (s, a) => { if (Refreshing) return; string v = m_home.Text.Trim(); Write(HomeColumn, v.Length == 0 ? (object)DBNull.Value : v); RefreshControls(); };
            browse.Click += (s, a) =>
            {
                using (var d = new FolderBrowserDialog { Description = "Choose the Data Sources Home folder", SelectedPath = Directory.Exists(m_home.Text.Trim()) ? m_home.Text.Trim() : "" })
                    if (d.ShowDialog(this) == DialogResult.OK) { Write(HomeColumn, d.SelectedPath.Replace('\\', '/')); RefreshControls(); }
            };
            make.Click += (s, a) =>
            {
                string h = m_home.Text.Trim();
                if (h.Length == 0) { MessageBox.Show(this, "Choose the home folder first.", "Data Sources Home"); return; }
                int made = 0;
                try
                {
                    foreach (string d in m_entries.Where(e => e.HomePath != null).Select(e => PlaceFolder(h, e.HomePath)).Concat(new[] { Path.Combine(h, "logs") }).Distinct())
                        if (!Directory.Exists(d)) { Directory.CreateDirectory(d); made++; }
                }
                catch (Exception ex) { MessageBox.Show(this, ex.Message, "Data Sources Home"); }
                RefreshControls();
                MessageBox.Show(this, made == 0 ? "Every folder was already there." : string.Format("Made {0} folders in {1}.", made, h), "Data Sources Home");
            };
            return row;
        }

        // the folder a HomePath names: itself for a folder column, the pattern's folder for a file column
        internal static string PlaceFolder(string home, string homePath)
        {
            string first = homePath.Split('|')[0];
            return Path.Combine(home, first.Contains("*") ? Path.GetDirectoryName(first) : first);
        }

        // a HomePath under the home folder, as src/datahome.R home_path: the folder, or the one file its patterns match. found = files matched
        internal static string Place(string home, string homePath, out int found)
        {
            found = 0;
            try
            {
                string[] parts = homePath.Split('|');
                if (!parts[0].Contains("*")) { string p = Path.Combine(home, parts[0]); if (Directory.Exists(p) && Directory.EnumerateFileSystemEntries(p).Any()) { found = 1; return p; } return null; }   // an empty place (Make folders) is not there
                string dir = PlaceFolder(home, homePath); if (!Directory.Exists(dir)) return null;
                var pats = new[] { Path.GetFileName(parts[0]) }.Concat(parts.Skip(1));
                // the suffix test keeps "*.tif" from matching .tiff (GetFiles' 3-letter extension rule)
                List<string> files = pats.SelectMany(pt => Directory.GetFiles(dir, pt).Where(f => !pt.StartsWith("*") || f.EndsWith(pt.Substring(1), StringComparison.OrdinalIgnoreCase))).Distinct().ToList();
                found = files.Count;
                return files.Count == 1 ? files[0].Replace('\\', '/') : null;
            }
            catch (Exception) { return null; }
        }

        // manifest.csv of the home folder: Column -> its row (header names as keys)
        private static Dictionary<string, Dictionary<string, string>> Manifest(string home)
        {
            var m = new Dictionary<string, Dictionary<string, string>>(StringComparer.OrdinalIgnoreCase);
            try
            {
                string f = Path.Combine(home, "manifest.csv"); if (home.Length == 0 || !File.Exists(f)) return m;
                List<string[]> rows = Csv.Read(f); if (rows.Count < 2) return m;
                string[] head = rows[0];
                foreach (string[] r in rows.Skip(1))
                {
                    var d = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
                    for (int i = 0; i < head.Length && i < r.Length; i++) d[head[i]] = r[i];
                    string c; if (d.TryGetValue("Column", out c)) m[c] = d;
                }
            }
            catch (Exception) { }
            return m;
        }
        private static string Get(Dictionary<string, string> d, string k) { string v; return d != null && d.TryGetValue(k, out v) ? v : ""; }

        private void Browse(Entry e)
        {
            string now = e.Path.Text.Trim();
            if (e.Column.EndsWith("Folder", StringComparison.OrdinalIgnoreCase))
            {
                using (var d = new FolderBrowserDialog { Description = "Choose the folder", SelectedPath = Directory.Exists(now) ? now : "" })
                    if (d.ShowDialog(this) == DialogResult.OK) { Write(e.Column, d.SelectedPath.Replace('\\', '/')); RefreshControls(); }
            }
            else
            {
                using (var d = new OpenFileDialog { Title = "Choose the file", CheckFileExists = true })
                {
                    try { if (now.Length > 0 && Directory.Exists(Path.GetDirectoryName(now))) { d.InitialDirectory = Path.GetDirectoryName(now); d.FileName = Path.GetFileName(now); } } catch (Exception) { }
                    if (d.ShowDialog(this) == DialogResult.OK) { Write(e.Column, d.FileName.Replace('\\', '/')); RefreshControls(); }
                }
            }
        }

        protected override void ShowData()
        {
            string home = m_home == null ? "" : Show(Value(HomeColumn)).Trim();
            if (m_home != null)
            {
                m_home.Text = home; m_tip.SetToolTip(m_home, home);
                bool hx = false; try { hx = home.Length > 0 && Directory.Exists(home); } catch (Exception) { }
                m_homeState.Text = home.Length == 0 ? "not set" : hx ? "✓" : "✗";
                m_homeState.ForeColor = home.Length == 0 ? SystemColors.GrayText : hx ? Color.ForestGreen : Color.Firebrick;
            }
            List<string> folders = Folders(home);
            var fuels = new HashSet<string>(); var heights = new HashSet<string>();
            List<HomeCheck.Zone> zones = ScenarioZones(fuels, heights);
            if (m_zonesNote != null) m_zonesNote.Text = zones.Count == 0 ? "No scenario names a fire zone yet, so green only means the data is there." : "Checked for the fire zones this project's scenarios name: " + string.Join("; ", zones.Select(z => z.Name)) + ".";
            string lfPath = Show(Value(LandfireColumn)).Trim();
            foreach (Layer l in m_layers) ShowLayer(l, home, lfPath, folders, zones, l.Product == "CH" ? heights : fuels);
            if (m_advanced != null && !m_advancedTouched)
            {
                bool any = m_entries.Any(x => x.Access == "built" && Show(Value(x.Column)).Trim().Length > 0);
                if (m_advanced.Checked != any) m_advanced.Checked = any;
                m_advancedTable.Visible = m_advanced.Checked;
            }
            foreach (Entry e in m_entries)
            {
                string v = Show(Value(e.Column)).Trim();
                e.Path.Text = v;
                if (!e.Path.Focused) { e.Path.SelectionStart = v.Length; e.Path.ScrollToCaret(); }   // a long path shows its end (the file or folder name)
                m_tip.SetToolTip(e.Path, v);
                bool exists = false;
                try { exists = v.Length > 0 && (File.Exists(v) || Directory.Exists(v)); } catch (Exception) { }
                // the folders that hold it (top first, then regions), each with its manifest row
                var at = new List<KeyValuePair<string, string>>(); var rows = new List<Dictionary<string, string>>(); int found = 0;
                if (v.Length == 0 && e.HomePath != null)
                    foreach (string f in folders)
                    {
                        int n; string p = Place(f, e.HomePath, out n); if (n > 1 && found == 0) found = n;
                        if (p == null) continue;
                        Dictionary<string, string> r; Manifest(f).TryGetValue(e.Dataset, out r); rows.Add(r);
                        at.Add(new KeyValuePair<string, string>(FolderLabel(home, f), p + (r == null ? "" : string.Format(" ({0}{1}; {2})", Get(r, "Copy") == "national" ? "national copy" : "region: " + Get(r, "Units"),
                                                                                                                      Get(r, "BufferKm").Length > 0 ? " + " + Get(r, "BufferKm") + " km" : "", Get(r, "Status")))));
                    }
                bool layersBad = e.Column == LandfireColumn && m_layers.Any(l => !l.Good);
                if (at.Count > 0)
                {
                    string tip = string.Join(Environment.NewLine, at.Select(x => x.Key + ": " + x.Value)) + (at.Count > 1 ? Environment.NewLine + "A stage reads the smallest one holding its zone." : "");
                    HomeCheck.Zone z = HomeCheck.Uncovered(zones, rows);
                    if (layersBad) Mark(e.State, e.Why, false, "✗ Incomplete", "See its layers below.", tip);
                    else if (z != null) Mark(e.State, e.Why, false, "✗ Doesn't cover zone", HomeCheck.Why(z, rows), tip);
                    else Mark(e.State, e.Why, true, "✓ Good", null, tip + (zones.Any(q => q.Box != null) ? "" : Environment.NewLine + "No scenario names a fire zone with a known box yet, so coverage was not checked."));
                }
                else if (v.Length == 0 && found > 1) Mark(e.State, e.Why, false, string.Format("✗ {0} files", found), string.Format("{0} files match in the home folder: keep one, or set the path.", found), e.HomePath);
                else if (v.Length == 0 && e.Access == "built") Mark(e.State, e.Why, null, "Built by the stage", null, "");
                else if (v.Length == 0) Mark(e.State, e.Why, false, "✗ Not downloaded yet", home.Length == 0 ? "Set Data Sources Home and run a Data Download, or set this path." : e.Column == LandfireColumn ? "See its layers below." : "Run a Data Download for this zone, or set this path.", home.Length > 0 && e.HomePath != null ? "Nothing at " + e.HomePath + " in the home folder" : "");
                else if (!exists) Mark(e.State, e.Why, false, "✗ Path not found", "This path does not exist on this machine.", v);
                else if (layersBad) Mark(e.State, e.Why, false, "✗ Incomplete", "See its layers below.", v);
                else Mark(e.State, e.Why, true, "✓ Good", null, v);
                string first = ""; List<Ed> eds = v.Length == 0 && at.Count > 0 ? Editions(home, e.Dataset, out first) : new List<Ed>();
                e.Edition.Visible = eds.Count > 0;
                if (eds.Count > 0)
                {
                    string cur = EditionOf(e.Dataset);
                    e.Edition.BeginUpdate(); e.Edition.Items.Clear();
                    e.Edition.Items.Add(new Ed { Tag = "", Text = "First edition" + (first.Length > 0 ? " (" + first + ")" : "") });
                    foreach (Ed x in eds) e.Edition.Items.Add(x);
                    e.Edition.SelectedItem = e.Edition.Items.Cast<Ed>().FirstOrDefault(x => x.Tag == cur) ?? e.Edition.Items[0]; e.Edition.EndUpdate();
                    m_tip.SetToolTip(e.Edition, "The edition this library reads. A run never changes it; choosing another asks first.");
                }
            }
            SizePaths();   // the marks just changed width
        }
    }
}
