
using System.Diagnostics;
using System.Reflection;
using Internal;

int[] KmpDfa(string str)
{
    var fail = new int[str.Length];
    if (str.Length <= 0)
        return fail;
    // 失败状态，保存
    fail[0] = -1;
    // 用于记录上一个字符串的失败状态
    var f = -1;
    // 预读前一个字符
    for (int i = 0; i < str.Length - 1; i++)
    {
        Debug.Assert(f < i);
        while (str[f + 1] != str[i + 1] && f != -1)
            f = fail[f];
        if (str[f + 1] == str[i + 1])
        {
            fail[i + 1] = f + 1;
            f = f + 1;
        }
        else
        {
            fail[i + 1] = -1;
        }
        // 往前循环找到能够前进的 fail 状态
        // 如果没有找到，即 f = -1，
    }
    return fail;
}

int KmpSearch(string pattern, string input)
{
    var prefix_dfa = KmpDfa(pattern);
    var state = -1;
    var i = 0;
    for (; i < input.Length && state < pattern.Length - 1;
        i++)
    {
        while (pattern[state + 1] != input[i] && state != -1)
            state = prefix_dfa[state];
        if (pattern[state + 1] == input[i])
            state++;
    }
    return state == pattern.Length - 1 ? i : -1;
}

void ShowKmp(string pattern)
{
    var dfa = KmpDfa(pattern);
    for (int i = 0; i < dfa.Length; i++)
    {
        if (dfa[i] == -1)
            continue;
        Console.WriteLine(pattern.Substring(0, dfa[i] + 1));
    }
}

string VisualizeDfa(string pattern)
{
    var dfa = KmpDfa(pattern);
    var sb = new StringBuilder();

    sb.AppendLine($"digraph {pattern} {{");
    sb.AppendLine("graph[rankdir = LR]");
    sb.AppendLine("edge[weight=10];");

    void EdgeWithLabel(int from, int to, string label)
    {
        sb.AppendLine($"{from}->{to}[label=\"{label}\"]");
    }
    void Edge(int from, int to)
    {
        sb.AppendLine($"{from}->{to}");
    }

    for (int i = 0; i < pattern.Length; i++)
    {
        EdgeWithLabel(i - 1, i, pattern[i].ToString());
    }

    sb.AppendLine("edge[weight=1, color=red];");
    for (int i = 0; i < dfa.Length; i++)
    {
        if (dfa[i] == -1)
            continue;
        Edge(i, dfa[i]);
    }

    sb.AppendLine("}");
    return sb.ToString();
}
void exportDotToSvg(string dot, string name, string format)
{
    var dot_file = $"{name}.dot";
    var bat_file = $"{name}.bat";
    var output_file = $"{name}.{format}";
    File.WriteAllText(dot_file, dot);


    var bat =
        $"dot -T{format} {dot_file} -o {output_file}";


    File.WriteAllText(bat_file, bat);
    var result = Process.Start(bat_file);
    result.WaitForExit();


    File.Delete(dot_file);
    File.Delete(bat_file);

	var start_info = new ProcessStartInfo(output_file) 
        { UseShellExecute = true };
    Process.Start(start_info);
}

ShowKmp("abaaba");

var searchResult = KmpSearch("ababa", "acabaababaa");
Console.WriteLine($"Search result at {searchResult}");

var dot = VisualizeDfa("ababa");
exportDotToSvg(dot, "ababa", "svg");
// System.Console.WriteLine(dot);
