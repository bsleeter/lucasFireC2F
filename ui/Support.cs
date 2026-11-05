// =============================================================================
// Support.cs - the package folder, and CSV reading for the pages
// =============================================================================
using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace lucasFireUI
{
    public static class LibrarySetup
    {
        public static string PackageDirectory()
        {
            string dir = Path.GetDirectoryName(typeof(LibrarySetup).Assembly.Location);
            if (!Directory.Exists(Path.Combine(dir, "defaults")))
            {
                // a shadow copy: fall back to where the assembly was loaded from
                dir = Path.GetDirectoryName(new Uri(typeof(LibrarySetup).Assembly.CodeBase).LocalPath);
            }
            if (!Directory.Exists(Path.Combine(dir, "defaults"))) throw new DirectoryNotFoundException("lucasFireC2F defaults folder not found next to " + dir);
            return dir;
        }
    }

    internal static class Csv
    {
        public static List<string[]> Read(string file)
        {
            string text = File.ReadAllText(file, Encoding.UTF8);
            var rows = new List<string[]>(); var row = new List<string>(); var f = new StringBuilder();
            bool q = false;
            for (int i = 0; i < text.Length; i++)
            {
                char ch = text[i];
                if (q)
                {
                    if (ch == '"') { if (i + 1 < text.Length && text[i + 1] == '"') { f.Append('"'); i++; } else q = false; }
                    else f.Append(ch);
                }
                else if (ch == '"') q = true;
                else if (ch == ',') { row.Add(f.ToString()); f.Clear(); }
                else if (ch == '\n' || ch == '\r')
                {
                    if (ch == '\r' && i + 1 < text.Length && text[i + 1] == '\n') i++;
                    row.Add(f.ToString()); f.Clear(); rows.Add(row.ToArray()); row = new List<string>();
                }
                else f.Append(ch);
            }
            if (f.Length > 0 || row.Count > 0) { row.Add(f.ToString()); rows.Add(row.ToArray()); }
            if (rows.Count == 0) throw new InvalidDataException(file + " is empty");
            return rows;
        }
    }
}
