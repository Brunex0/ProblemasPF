let createCoins n =
  let coins = Array.make n 0 in
    for i = 0 to n-1 do
      let coin = read_int () in coins.(i) <- coin
    done;
    coins
;;

let getLeastAmountOfCoinsGreedy coins currentCoin =
  let cointCount = ref 0 and noInc = ref 1 and currentCoinTemp = ref currentCoin and i = ref 0 in
    while !currentCoinTemp > 0 do
      noInc := 1;
      i := (Array.length coins)- 1;
      while !i >= 0 do
        if (!currentCoinTemp-coins.(!i))>=0 then(
          cointCount := !cointCount + 1;
          currentCoinTemp := !currentCoinTemp - coins.(!i);
          noInc := 0;
          i := ~-1
        );
        i := !i - 1
      done;
      
      if !noInc = 1 then (
        cointCount := 0;
        currentCoinTemp := 0
      )
    done;
    !cointCount
;;

let min a b = 
  if a <= b then a else b
;;

let getLeastAmountOfCoins coins currentCoin dynamicArray =
  let minValue = ref 0 in
    for i = 0 to currentCoin do
      minValue := dynamicArray.(i);
      for j = 0 to (Array.length coins) - 1 do
        if (i - coins.(j) >= 0) then 
          minValue := min (dynamicArray.(i - coins.(j)) + 1) (!minValue)
      done;
      if (!minValue = (currentCoin + 2)) then
        dynamicArray.(i) <- 0
      else
        dynamicArray.(i) <- !minValue
    done;
    dynamicArray.(currentCoin)
;;

let n = read_int() in
  let coins = createCoins n in
    for i = 0 to (2 * coins.(n - 1)) do
      let dynamicArray = Array.make (i + 1) (i + 2) in
        let coinsDynamic = (getLeastAmountOfCoins coins i dynamicArray) and coinsGreedy = (getLeastAmountOfCoinsGreedy coins i) in
          if(coinsDynamic!=coinsGreedy) then(
            Printf.printf "%d\n" i;
            exit 0
          )
    done;
    Printf.printf "YES\n"
;;
    