(*
 * SharpSolver - Progetto di Programmazione e Calcolo a.a. 2018-19
 * Main.fs: console e codice main
 * (C) 2018 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)

(* Mason Simone 876672 ,Pittini Enrico 877345 ,Simionato Francesco 876279 *)

module SharpSolver.Main

open Microsoft.FSharp.Text.Lexing
open Absyn
open System
open Prelude
open Microsoft.FSharp.Text
open Impl //Importo dal modulo Impl le funzioni al suo interno

// funzioni di logging e printing
//

let hout hd fmt =
    if not <| String.IsNullOrWhiteSpace hd then
        printf "[%s]%s" hd (new String (' ', max 1 (Config.prefix_max_len - String.length hd)))
        stdout.Flush ()
    printfn fmt

let chout col hd fmt =
    let c = Console.ForegroundColor
    Console.ForegroundColor <- col
    Printf.kprintf (fun s -> hout hd "%s" s; Console.ForegroundColor <- c) fmt

let out fmt = hout "" fmt
let cout col fmt = chout col "" fmt

let norm fmt = chout ConsoleColor.Yellow "norm" fmt
let redux fmt = chout ConsoleColor.Magenta "redux" fmt
let sol fmt = chout ConsoleColor.Green "sol" fmt
let ident fmt = chout ConsoleColor.Green "ident" fmt    
let error fmt = chout ConsoleColor.Red "error" fmt   
let degree fmt = chout ConsoleColor.Blue "degree" fmt //per stampare il degree (in blu)
// interprete dei comandi e delle espressioni
//

let interpreter_loop () =
    while true do
        printf "\n%s" Config.prompt_prefix          // stampa il prompt
        stdout.Flush ()                             // per sicurezza flusha lo stdout per vedere la stampa del prompt senza end-of-line
        let input = Console.ReadLine ()             // leggi l'input scritto dall'utente
        let lexbuf = LexBuffer<_>.FromString input  // crea un lexbuffer sulla stringa di input

        // funzione locale per il pretty-printing degli errori
        let localized_error msg =
            let tabs = new string (' ', Config.prompt_prefix.Length + lexbuf.StartPos.Column)
            let cuts = new string ('^', let n = lexbuf.EndPos.Column - lexbuf.StartPos.Column in if n > 0 then n else 1)
            cout ConsoleColor.Yellow "%s%s\n" tabs cuts
            error "error at %d-%d: %s" lexbuf.StartPos.Column lexbuf.EndPos.Column msg 
        
        // blocco con trapping delle eccezioni
        try
            let line = Parser.line Lexer.tokenize lexbuf    // invoca il parser sul lexbuffer usando il lexer come tokenizzatore
            #if DEBUG
            hout "absyn" "%+A" line
            hout "pretty" "%O" line
            #endif

            // interpreta la linea in base al valore di tipo line prodotto dal parsing
            match line with
            | Cmd "help" ->
                out "%s" Config.help_text

            | Cmd ("quit" | "exit") ->
                out "%s" Config.exit_text
                exit 0

            // TODO: se volete supportare altri comandi, fatelo qui (opzionale)
            
            | Cmd s -> error "unknown command: %s" s    // i comandi non conosciuti cadono in questo caso

            // TODO: aggiungere qui sotto i pattern per i casi Expr ed Equ con relativo codice per, rispettivamente, normalizzare espressioni e risolvere equazioni

            | Expr e1 -> let p = reduce e1   //riduco l'espressione e la stampo ridotta.
                         redux "%O" p
                         let p1 = normalize p //normalizzo il polinomio e lo stampo.
                         norm "%O" p1
                         let n = normalized_polynomial_degree p1 //calcolo e stampo il grado del polinomio normalizzato.
                         degree "%O" n
            | Equ (e1,e2) -> let p1 = reduce e1 //riduco l'espressione a sinistra dell'uguale
                             let p2 = reduce e2 //riduco l'espressione a destra dell'uguale
                             redux "%O = %O" p1  p2 //le stampo
                             let p = //porto il polinomio a destra dell'uguale a sinistra (negandolo), creando così un unico grande polinomio
                                  match (p1,polynomial_negate p2) with 
                                  (Polynomial ms1,Polynomial ms2)->Polynomial (ms1@ms2)
                             let pnorm = normalize p //normalizzo il polinomio e lo stampo
                             norm "%O = 0" pnorm
                             let n = normalized_polynomial_degree pnorm //calcolo e stampo il grado del polinomio 
                             degree "%O" n
                             match n with // a seconda del grado del polinomio calcolo la soluzione in modo diverso 
                             0 -> let x = solve0 pnorm  //Il grado è 0 : il risultato è o true o false
                                  ident "%O" x
                             |1 -> let x = solve1 pnorm //Il grado è 1 : una soluzione
                                   sol "x = %O" x
                             |2-> let x = solve2 pnorm //Il grado è 2 : o nessuna, o una o due soluzioni.
                                  match x with
                                  None -> sol "Nessuna soluzione reale"
                                  |Some (x,None)-> sol "x = %O" x
                                  |Some(x1,Some x2) -> sol "x1 = %O vel x2 = %O" x1 x2
                                
                             |_->error "Equazioni risolvibili fino al secondo grado" //Il grado è > 2 : equazioni risolvibili fino a grado 2.

        // gestione delle eccezioni
        with LexYacc.ParseErrorContextException ctx ->
                let ctx = ctx :?> Parsing.ParseErrorContext<Parser.token>
                localized_error (sprintf "syntax error%s" (match ctx.CurrentToken with Some t -> sprintf " at token <%O>" t | None -> ""))

           | Lexer.LexerError msg -> localized_error msg 

           | :? NotImplementedException as e -> error "%O" e
        
           | e -> localized_error e.Message


// funzione main: il programma comincia da qui
//

[<EntryPoint>]
let main _ = 
    let code =
        try
            interpreter_loop ()                 // chiama l'interprete
            0                                   // ritorna il codice di errore 0 (nessun errore) al sistema operativo se tutto è andato liscio
        with e -> error "fatal error: %O" e; 1  // in caso di eccezione esce con codice di errore 1
    #if DEBUG
    Console.ReadKey () |> ignore                // aspetta la pressione di un tasto prima di chiudere la finestra del terminare 
    #endif
    code


