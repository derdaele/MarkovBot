open System
open System.Collections.Generic

let sampleText = ""
let rand = Random()
let getNGrams = Seq.windowed
    
let transitions (ngrams : string[] seq) = 
    ngrams 
    |> Seq.map (fun x -> (Seq.take (Seq.length x - 1) x, Seq.last x))
    |> Seq.groupBy fst
    |> Seq.map (fun (a,b) -> (Seq.toArray a, Seq.map snd b))  

let rec talk (seed : string[]) (transitions : (string [] * string seq) seq) = 
    seq {
        yield Seq.head seed
        match Seq.tryFind (fst >> ((=) seed)) transitions with
        | Some(_,words) -> 
            yield! talk 
                (Array.concat [| (Array.tail seed); Array.singleton (Seq.item (Seq.length words |> rand.Next) words)|])
                transitions
        | None -> ()
    }

[<EntryPoint>]
let main argv = 
    sampleText.ToLowerInvariant().Split([| ' '; '\n'; '\r'; '\t' |], StringSplitOptions.RemoveEmptyEntries)
    |> getNGrams 3
    |> transitions
    |> talk [|"you"; "cease"|]
    |> Seq.take 50
    |> fun a -> String.Join(" ", a)
    |> printfn "%A" 
    0 // return an integer exit code
