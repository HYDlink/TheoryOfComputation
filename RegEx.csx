using System.ComponentModel;

public class NFA
{
    public int StateCount;
    public List<Dictionary<char, int>> Transition;

    public NFA(int stateCount)
    {
        StateCount = stateCount;
        Transition = Enumerable.Range(0, stateCount)
            .Select(_ => new Dictionary<char, int>())
            .ToList();
    }

    public Dictionary<char, int> GetTransition(int from)
        => Transition[from];

    public bool TryGetTransition(int from, char input, out int next)
        => GetTransition(from).TryGetValue(input, out next);

    public void AddTransition(int from, char input, int to)
        => GetTransition(from).Add(input, to);
}

public const char EPSILON = '\0';

public NFA RegExToNfa(string regex)
{
    var nfa = new NFA(regex.Length + 1);

    var stack = new Stack<int>();

    for (int i = 0; i < regex.Length; i++)
    {
        void AddClosure(int closurePos, int prevPos)
        {
            nfa.AddTransition(prevPos, EPSILON, closurePos);
            nfa.AddTransition(closurePos, EPSILON, prevPos);
            nfa.AddTransition(closurePos, EPSILON, closurePos + 1);
        }
        switch (regex[i])
        {
            case '(' or '|':
                stack.Push(i);
                break;
            case ')':
                {
                    var error_msg = $"right ) has no correspond left (, at index {i}";

                    if (!stack.TryPop(out var prev))
                        throw new Exception(error_msg);
                    if (regex[prev] == '|')
                    {

                    }
                    else if (regex[prev] == ')')
                    {
                        if (i + 1 < regex.Length
                        && regex[i + 1] == '*')
                        {
                            // closure
                            AddClosure(prev, i + 1);
							
                        }
                    }
                    nfa.AddTransition(i, EPSILON, i + 1);
                }
                break;
            default:
                break;
        }
    }

    return nfa;
}
