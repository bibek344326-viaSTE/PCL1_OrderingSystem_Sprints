type DrinkSize = Small | Medium | Large

type CoffeeType = Espresso | Latte | Cappuccino
type TeaType = GreenTea | BlackTea | HerbalTea
type JuiceType = OrangeJuice | AppleJuice | MangoJuice

type Drink =
    | Coffee of CoffeeType * DrinkSize
    | Tea of TeaType * DrinkSize
    | Juice of JuiceType * DrinkSize
    | Soda of DrinkSize
    | Milk of DrinkSize

type FoodType = Sandwich | Burger | Salad
type FruitType = Apple | Banana | Orange

type Product =
    | Drink of Drink
    | Food of FoodType
    | Fruit of FruitType

let computeDrinkPrice (drink: Drink) =
    match drink with
    | Coffee (Espresso, Small) -> 20
    | Coffee (Espresso, Medium) -> 25
    | Coffee (Espresso, Large) -> 30
    | Coffee (Latte, Small) -> 25
    | Coffee (Latte, Medium) -> 30
    | Coffee (Latte, Large) -> 35
    | Coffee (Cappuccino, Small) -> 30
    | Coffee (Cappuccino, Medium) -> 35
    | Coffee (Cappuccino, Large) -> 40
    | Tea (BlackTea, Small) -> 15
    | Tea (BlackTea, Medium) -> 20
    | Tea (BlackTea, Large) -> 25
    | Tea (GreenTea, Small) -> 20
    | Tea (GreenTea, Medium) -> 25
    | Tea (GreenTea, Large) -> 30
    | Tea (HerbalTea, Small) -> 25
    | Tea (HerbalTea, Medium) -> 30
    | Tea (HerbalTea, Large) -> 35
    | Juice (OrangeJuice, Small) -> 30
    | Juice (OrangeJuice, Medium) -> 35
    | Juice (OrangeJuice, Large) -> 40
    | Juice (AppleJuice, Small) -> 30
    | Juice (AppleJuice, Medium) -> 35
    | Juice (AppleJuice, Large) -> 40
    | Juice (MangoJuice, Small) -> 35
    | Juice (MangoJuice, Medium) -> 40
    | Juice (MangoJuice, Large) -> 45
    | Soda Small -> 15
    | Soda Medium -> 20
    | Soda Large -> 25
    | Milk Small -> 10
    | Milk Medium -> 15
    | Milk Large -> 20

let computeFoodPrice (food: FoodType) =
    match food with
    | Sandwich -> 50
    | Burger -> 70
    | Salad -> 45

let computeFruitPrice (fruit: FruitType) =
    match fruit with
    | Apple -> 10
    | Banana -> 8
    | Orange -> 12

let computeProductPrice (product: Product) =
    match product with
    | Drink d -> computeDrinkPrice d
    | Food f -> computeFoodPrice f
    | Fruit fr -> computeFruitPrice fr

let order1 = Drink (Coffee (Latte, Medium))
let order2 = Food Sandwich
let order3 = Fruit Apple

printfn "Price of order1: %d DKK" (computeProductPrice order1)
printfn "Price of order2: %d DKK" (computeProductPrice order2)
printfn "Price of order3: %d DKK" (computeProductPrice order3)