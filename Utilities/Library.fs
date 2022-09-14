namespace Utilities

open System
open System.Diagnostics
open System.IO
open DotNetGraph.Compiler

module Graph =
    let compileGraphToSvg graph name (format) =
        let compiled = DotCompiler(graph).Compile()
        let dot_file = $"{name}.dot"
        let bat_file = $"{name}.bat"
        let output_file = $"{name}.{format}"
    
        Console.WriteLine(compiled)
        File.WriteAllText(dot_file, compiled)
    
        let bat =
            $"dot -T{format} {dot_file} > {output_file}"
    
        File.WriteAllText(bat_file, bat)
        let result = Process.Start bat_file
        result.WaitForExit()
    
        File.Delete dot_file
        File.Delete bat_file
    
        Process.Start(ProcessStartInfo("irfanview.exe", output_file, UseShellExecute = true))
