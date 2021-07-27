
(*
 * SharpSolver - Progetto di Programmazione e Calcolo a.a. 2018-19
 * Impl.fsi: implementazioni degli studenti
 * (C) 2018 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)

module SharpSolver.Impl

open Absyn
open Prelude
open System

let rationalize (x : float) : rational = 
           let rec countdigits y =if y<10 then 1 else 1+countdigits(y/10) //funzione che dato un numero ne conta le cifre
           let str_x = string(x) //conversione in stringa del valore float dato in ingresso
           let Array_split = str_x.Split [|'.'|]  //divide il valore string in 2 parti,la parte a sinistra del punto e quella a destra di esso,
                                                  //le 2 parti vengono poi inserite rispettivamente nella prima [0] e seconda [1] cella di un 
                                                  //array creato appositamente per contenerle
           let Unione_str = Array_split.[0]+Array_split.[1] //unione delle 2 stringhe
           let Num = Int32.Parse(Unione_str) //conversione in int dell'unione delle 2 stringhe,in questo modo si ha come numeratore il valore 
                                             //di ingresso privato del punto(quindi vi è stata una conversione float -> int ,tuttavia la parte 
                                             //decimale è stata mantenuta  123.456 -> 123456)
           let Denom = 10.**(float(countdigits (int(Array_split.[1])))) //Vengono contate le cifre della parte decimale del valore dato in 
                                                                        //ingresso,poi viene creato il denominatore elevando 10 al numero di cifre
                                                                        //contate
           in rational(Num,int(Denom)) //Dato numeratore e denominatore di tipo int (denominatore dopo conversione in int) ,viene creato il valore
                                       //di tipo rational


let monomial_degree (m : monomial) : int = 
                    match m with  //decostruisco il monomio con il pattern matching per ricavarne il grado
                    Monomial (_,deg)->deg


let monomial_negate (m : monomial) : monomial = 
                    match m with  //decostruisco il monomio e restituisco il monomio con il coefficente invertito
                    Monomial (coeff,deg)->Monomial(-coeff,deg)


let rec polynomial_degree (p : polynomial) : int = 
              //il grado di un polinomio è il massimo grado dei suoi monomi.
              match p with     
              Polynomial []-> 0
              |Polynomial (m1::monos) -> if monomial_degree m1 <= polynomial_degree (Polynomial monos) 
                                         then polynomial_degree (Polynomial monos)
                                         else monomial_degree m1  

                                         
let polynomial_negate (p : polynomial) : polynomial = 
                        match p with //inverto ogni monomio del polinomio
                        Polynomial monos -> Polynomial (List.map monomial_negate monos)


let normalized_polynomial_degree (np : normalized_polynomial) : int = 
                                    //il grado di un polinomio normalizzato non è altro che la lunghezza dell'array di razionali
                                    //(ovvero il numero di elementi) meno uno
                                    match np with
                                    NormalizedPolynomial rationals -> (Array.length rationals)-1

                                    
let normalize (p : polynomial) : normalized_polynomial = 
                   //funzione che data una lista di monomi, tiene nella lista solo i monomi con grado uguale a deg.
                   let rec filtra deg monos =   
                                            List.filter (fun x -> monomial_degree x = deg) monos

                   //funzione che data una lista di monomi crea una lista di liste di monomi, una per ogni grado possibile da 
                   //i al grado del polinomio. In ogni sottolista ci sono solo i monomi, della lista iniziale, con quel grado. 
                   //i verrà successivamente posto uguale a 0, quando si richiamerà la funzione.
                   let rec raggruppa_per_grado i monos = if i> polynomial_degree ( Polynomial monos ) then []
                                                         else (filtra i monos)::raggruppa_per_grado (i+1) monos

                   //funzione che somma due monomi , che si suppongono simili.                     
                   let rec somma_monomi_simili m1 m2= match (m1,m2) with 
                                                      (Monomial (coeff1,deg), Monomial (coeff2,_))->Monomial (coeff1+coeff2,deg)

                   //funzione che somma tutti i monomi di una lista di monomi. I monomi si suppongono tutti simili. 
                   //Questa funzione dunque prende una lista di monomi e ritorna un monomio.
                   let rec somma_lista_monomi_simili monos = 
                                let deg = polynomial_degree (Polynomial monos)
                                in                           match monos with
                                                             []->Monomial (0Q,deg)
                                                             |m::ms -> somma_monomi_simili m (somma_lista_monomi_simili ms)

                   //Si applica la funzione raggruppa_per_grado alla lista di monomi, con i=0. Si ottiene così una lista di 
                   //liste di monomi. Si applica ad ogni sottolista la funzione somma_lista_monomi_simili. Il risultato finale
                   //è quindi solo una lista di monomi, ordinati per grado,e ognuno dei quali è il risultato della somma dei
                   //monomi simili.
                   let rec normalizza_lista_monomi monos = List.map (somma_lista_monomi_simili) (raggruppa_per_grado 0 monos)

                   //si trasforma la lista di monomi nella lista dei razionali che sono coefficenti dei monomi.
                   let rec lista_coefficenti monos  = match monos with
                                                      []->[]
                                                      |Monomial (coeff,deg)::ms -> coeff::lista_coefficenti ms

                   //si eliminano dalla lista di razionali gli zeri più in fondo. Ad esempio , la lista [1Q;2Q,3Q;0Q;4Q;0Q;0Q;0Q] 
                   //diventa [1Q;2Q;3Q;0Q;4Q].
                   let rec elimina_zeri l = let l1 = List.rev l
                                            let rec aux ls =  match ls with
                                                              [x] when x=0Q->[x]
                                                              |x::xs when x=0Q -> aux xs
                                                              |_-> ls
                                            in List.rev (aux l1)
                                            
                   in match p with
                      Polynomial monos -> //si applicano le funzioni precedenti e si trasforma la lista di razionali in un 
                                          //array di razionali.
                                          let ms = normalizza_lista_monomi monos 
                                          in  NormalizedPolynomial( List.toArray (elimina_zeri(lista_coefficenti ms)))

                   
let derive (p : polynomial) : polynomial = 
                    //funzione che deriva un monomio
                    let  derivata_monomio m = match m with
                                              Monomial (coeff,deg) -> if deg>=1 then Monomial (coeff*rational (deg,1),deg-1)
                                                                      else Monomial (0Q,0)

                    in match p with    //derivo ogni monomio del polinomio
                       Polynomial monos -> Polynomial (List.map (derivata_monomio) monos)


let filtra_zeri p = //funzione aggiuntiva che dato un polinomio toglie i monomi con coefficente uguale a 0. La uso dentro reduce.
          match p with
          Polynomial monos -> let monomial_coeff m = //funzione che dato un monomio restituisce il suo coefficente
                                    match m with  //Decostrouisco il monomio con il pattern matching per ricavarne il coefficente.
                                    Monomial (coeff,_)->coeff
                              let ms = List.filter (fun x -> monomial_coeff x<> 0Q) monos
                              in Polynomial ms


let reduce (e : expr) : polynomial = 
            //funzione che data un'espressione ritorna una coppia con il polinomio interno all'espressione e il numero di 
            //volte che è richiesta la derivazione
            let rec aux e = match e with
                            Poly p -> (p,0)
                            |Derive e1 -> let (p,count)= aux e1
                                          in (p,count+1)

            //funzione che dato un polinomio e un intero n deriva quel polinomio n volte
            let rec derive_n_times p n = if n=0 then p
                                         else derive_n_times (derive p) (n-1)
            let (p,count) = aux e
            in if count = 0 then derive_n_times p count
               else filtra_zeri(derive_n_times p count) //tolgo gli zeri dovuti alla derivazione

                
let solve0 (np : normalized_polynomial) : bool =
                //il grado del polinomio normalizzato è 0. L'equazione è del tipo c=0. Il risultato dunque sarà o true o false.
                match np with
                NormalizedPolynomial rationals -> rationals.[0]=0Q


let solve1 (np : normalized_polynomial) : rational =
                //il grado del polinomio normalizzato è 1. L'equazione è del tipo c+bx=0. La soluzione è dunque x=-c/b.
                match np with
                NormalizedPolynomial rationals -> let c = rationals.[0]
                                                  let b = rationals.[1]
                                                  in (-c)/b


let solve2 (np : normalized_polynomial) : (float * float option) option = 
               //il grado del polinomio normalizzato è 2. L'equazione è del tipo c+bx+ax^2=0. Si calcola il delta, e in base a
               //questo si capisce se non c'è nessuna soluzione, se ce n'è solo una o se ce ne sono due. 
               //Le soluzioni (se ce ne sono) si trovano con la formula (-b+/-sqrt(delta))/2*a.
               match np with
               NormalizedPolynomial rationals -> let a=float(rationals.[2]) 
                                                 let b=float(rationals.[1])
                                                 let c=float(rationals.[0])
                                                 let delta = b*b-4.0*a*c
                                                 if delta < 0.0 then None
                                                 else if delta = 0.0 then Some(-b/(2.0*a),None)
                                                      else let x1= (-b+sqrt(delta))/(2.0*a)
                                                           let x2 = (-b-sqrt(delta))/(2.0*a)
                                                           in Some (x1,Some x2 )
                                                       



