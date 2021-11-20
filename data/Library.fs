namespace Advent

open System.Resources

module Data =
    let ReadFile (name: string) =
        let res = ResourceManager("Data.Data", System.Reflection.Assembly.GetExecutingAssembly())
        res.GetString(name)