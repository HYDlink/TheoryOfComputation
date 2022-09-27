using System.Diagnostics.Contracts;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System;

class BrainFucker
{
    public const string COMMANDS = "+-><.,[]";
	public const int DEFAULT_CELL_VALUE = 0;
    public List<int> Cells = new List<int>() { DEFAULT_CELL_VALUE };
    public int CurrentCellIndex = 0;
    public int CurrentCell => Cells[CurrentCellIndex];

    public void PrintCurrentCell()
    {
        Console.WriteLine($"{CurrentCell}");
    }
    public void Parse(string input)
    {
		for (int i = 0; i < input.Length; i++)
        {
			// System.Console.WriteLine(i);
			var ch = input[i];
            if (!COMMANDS.Contains(ch))
                continue;

            switch (ch)
            {
                case '+': // Increments the value at the current cell by one.
					Cells[CurrentCellIndex]++;
                    break;
                case '-': // Decrements the value at the current cell by one.
                    Cells[CurrentCellIndex]--;
                    break;
                case '>': // Moves the data pointer to the next cell(cell on the right).
                    if (CurrentCellIndex >= Cells.Count - 1)
						Cells.Add(DEFAULT_CELL_VALUE);
					CurrentCellIndex++;
					Debug.Assert(CurrentCellIndex < Cells.Count);
                    break;
                case '<': // Moves the data pointer to the previous cell(cell on the left).
					if (CurrentCellIndex <= 0)
						throw new InvalidOperationException("Cannot move left all");
					CurrentCellIndex--;
                    break;
                case '.': // Prints the ASCII value at the current cell(i.e. 65 = 'A').
                    Console.Write($"{(char)CurrentCell}");
                    break;
                case ',': // Reads a single input character into the current cell.
					Console.Write($"Enter a character (Pos: {i}, CellPos: {CurrentCellIndex}): ");
                    var in_char = Console.ReadLine()[0];
					Cells[CurrentCellIndex] = in_char;
					// CurrentCell = inputChar;
                    break;
                case '[': // If the value at the current cell is zero, skips to the corresponding]. Otherwise, move to the next instruction.
					if (CurrentCell == 0) {
						// fix me, based on stack
						var next = input.IndexOf(']', i);
						if (next == -1)
							throw new InvalidOperationException($"Not found next ']' from index {i}");
						i = next;
					}
                    break;
                case ']': // If the value at the current cell is zero, move to the next instruction. Otherwise, move backwards in the instructions to the corresponding[ .
					if (CurrentCell != 0) {
						// fix me, based on stack
						var prev = input.Substring(0, i).LastIndexOf('[');
                        if (prev == -1)
                            throw new InvalidOperationException($"Not found prev '[' from index {i}");
						i = prev;
					}
                    break;
            }
        }
    }
}

// new BrainFucker().Parse("++++++ [ > ++++++++++ < - ] > +++++ .");
new BrainFucker().Parse(",>,< [ > [ >+ >+ << -] >> [- << + >>] <<< -] >>");