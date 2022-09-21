#nullable enable
using System.Runtime.CompilerServices;

class TrieNode
{
    public Dictionary<char, TrieNode> Children { get; set; } = new();
    public TrieNode? Fail { get; set; } = null;
    public TrieNode() { }
    public TrieNode(params string[] words)
    {
        foreach (var word in words)
        {
            Add(word);
        }
		BuildAcAutomata();
    }
    public void Add(IEnumerable<char> word)
    {
        if (!word.Any())
            return;
        var ch = word.First();
        if (Children.TryGetValue(ch, out var child))
        {
            child.Add(word.Skip(1));
        }
        else
        {
            var new_child = new TrieNode();
            Children.Add(ch, new_child);
            new_child.Add(word.Skip(1));
        }
    }

    public TrieNode? Get(IEnumerable<char> word)
    {
        if (!word.Any())
            return this;

        var ch = word.First();
        if (Children.TryGetValue(ch, out var child))
        {
            return child.Get(word.Skip(1));
        }
        else
        {
            return null;
        }
    }

    public bool Contains(IEnumerable<char> word)
        => Get(word) is { };

    public bool HasNext(char ch)
        => Children.ContainsKey(ch);

    public void BuildAcAutomata()
    {
        var root = this;
        var current = root;
        // BFS 对所有 child 添加 fail 状态

        void AddFail(TrieNode node)
        {
            foreach (var (ch, child) in node.Children)
            {
                var fail = node.Fail;
                // 根据自己的 fail 状态，查找 fail 处是否存在同样 ch 的指向
                // 如果没有，继续往之前的 fail 状态查找
                while (fail != null && !fail.HasNext(ch))
                {
                    fail = fail.Fail;
                }
                if (fail == null)
                    fail = root;
                if (fail.HasNext(ch))
                    child.Fail = fail.Children[ch];
				else
					child.Fail = null;
            }
        }

        var queue = new Queue<TrieNode>();
        queue.Enqueue(root);
		// root 的直接子节点不需要添加失配状态，否则直接指向它们自身
        while (queue.Any())
        {
            var count = queue.Count;
            var next_queue = new Queue<TrieNode>();
            for (int i = 0; i < count; i++)
            {
                var n = queue.Dequeue();
                // same rank
                foreach (var (ch, child) in n.Children)
                {
					AddFail(child);
                    queue.Enqueue(child);
                }
            }
        }
    }

    public string ExportGraphviz()
    {
        // graph start

        var sb = new StringBuilder();

        sb.AppendLine($"digraph {{");
        sb.AppendLine("graph[rankdir = LR]");
        sb.AppendLine("edge[weight=100];");
        sb.AppendLine("node[shape=circle];");

        void EdgeWithLabel(object from, object to, object label)
        {
            sb.AppendLine($"{from}->{to}[label=\" {label}\"]");
        }

		void FailEdge(TrieNode node) {
			if (node.Fail == null || node.Fail == this)
				return;
            sb.AppendLine($"{node.GetHashCode()}->{node.Fail.GetHashCode()}[color=red, weight=1]");
        }

        void AddNode(TrieNode node)
        {
            sb.AppendLine($"{node.GetHashCode()} [label=\"\"]");
        }

        var queue = new Queue<TrieNode>();
        queue.Enqueue(this);
        AddNode(this);
        while (queue.Any())
        {
            var count = queue.Count;
            var next_queue = new Queue<TrieNode>();

			// same rank
			sb.Append("{ rank=same; ");
			foreach (var node in queue)
			{
				sb.Append($"{node.GetHashCode()}; ");
			}
			sb.AppendLine(" };");


            for (int i = 0; i < count; i++)
            {
                var n = queue.Dequeue();
                // same rank
                foreach (var (ch, child) in n.Children)
                {
                    // node with get hashcode id
                    AddNode(child);
                    FailEdge(child);
                    EdgeWithLabel(n.GetHashCode(), child.GetHashCode(), ch);
                    queue.Enqueue(child);
                }
            }
        }

        // if empty, then empty graph
        sb.AppendLine("}");
        return sb.ToString();
    }
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


    // File.Delete(dot_file);
    File.Delete(bat_file);

    var start_info = new ProcessStartInfo(output_file)
    { UseShellExecute = true };
    Process.Start(start_info);
}


var example = new TrieNode("his", "hello", "she", "i");
// Console.WriteLine(example.Contains("his"));
// Console.WriteLine(example.Contains("hello"));
// Console.WriteLine(example.Contains("she"));
// Console.WriteLine(example.Contains("example"));

var exampleGraph = example.ExportGraphviz();
exportDotToSvg(exampleGraph, "example", "svg");