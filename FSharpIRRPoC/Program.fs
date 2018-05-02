// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System

let vals = [-38086.1968;
0.0;
0.0;
0.0;
0.0;
0.0;
0.0;
972.2899;
387.73;
1533.49;
10142.6092;
1374.9599;
605.6499;
0.0;
2375.4998;
1296.5397;
1738.9397;
2346.7798;
0.0;
2157.8799;
0.0;
994.7998;
0.0;
1425.7399;
1248.5199;
9238.3093;
669.5099]

let pv (i : float) (n : float) =
     1.0 / Math.Pow( (1.0 + i ), n)

let npv x i n =
    x * (pv i n)

let rec valList aList (ir : float) (n : float) =
    match aList with
    | [] -> [] 
    | h::t -> npv h ir n :: valList t ir (n+1.0)

let presentValue vals (ir : float) =
    List.fold(+)0.0 <| valList vals ir 0.0

let ret1 = presentValue vals 0.12

let calcDerivative (afunc: float -> float) x delta k =
    let xNeg = (afunc (x - delta)) - k
    let xPos = (afunc (x + delta)) - k
    let derivative = (xPos - xNeg)/2.0
    printfn "derivative: %10.7f" derivative
    derivative

let eps = 0.0001  // Near zero

let Newton (afunc: float -> float) x delta =

    let mutable k = afunc x  //x is the initial guess
    let mutable xNew = x     //initialize xNew = initial guess
    let mutable n = 0        //iteration counter

    while Math.Abs( (float) k ) > eps && n < 25 do
        let derivative = calcDerivative afunc xNew delta k
        xNew <- xNew - ((k / derivative) * delta)
        k <- afunc xNew
        n <- n + 1
        ()
    xNew

[<EntryPoint>]
let main argv = 
    let delta = 0.01

    let afunc irr =
        presentValue vals irr

    let x = Newton afunc 0.1 delta

    printfn "IRR: %10.7f" x
    
    Console.ReadKey() |> ignore
    0 // return an integer exit code
    