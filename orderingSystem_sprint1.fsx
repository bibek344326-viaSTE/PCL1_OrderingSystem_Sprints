// ========================================
// DOMAIN MODEL
// ========================================

// Represents available drink sizes
type DrinkSize =
    | Small
    | Medium
    | Large

// Coffee variants
type CoffeeType =
    | Espresso
    | Latte
    | Cappuccino

// Tea variants
type TeaType =
    | GreenTea
    | BlackTea
    | HerbalTea

// Juice variants
type JuiceType =
    | OrangeJuice
    | AppleJuice
    | MangoJuice

// Category of a drink
type DrinkCategory =
    | Coffee of CoffeeType
    | Tea of TeaType
    | Juice of JuiceType
    | Soda
    | Milk

// A drink consists of a category and a size
type Drink =
    { Category : DrinkCategory
      Size : DrinkSize }

// Food and fruit types
type FoodType = Sandwich | Burger | Salad
type FruitType = Apple | Banana | Orange

// A product can be a drink, food, or fruit
type Product =
    | Drink of Drink
    | Food of FoodType
    | Fruit of FruitType


// ========================================
// PRICING MODEL (Using Records)
// ========================================

// Record representing fixed prices for each size
type SizePrice =
    { Small : int
      Medium : int
      Large : int }

// Pricing tables for each drink category

let coffeePrices coffeeType =
    match coffeeType with
    | Espresso   -> { Small = 20; Medium = 25; Large = 30 }
    | Latte      -> { Small = 25; Medium = 30; Large = 35 }
    | Cappuccino -> { Small = 30; Medium = 35; Large = 40 }

let teaPrices teaType =
    match teaType with
    | BlackTea  -> { Small = 15; Medium = 20; Large = 25 }
    | GreenTea  -> { Small = 20; Medium = 25; Large = 30 }
    | HerbalTea -> { Small = 25; Medium = 30; Large = 35 }

let juicePrices juiceType =
    match juiceType with
    | OrangeJuice -> { Small = 30; Medium = 35; Large = 40 }
    | AppleJuice  -> { Small = 30; Medium = 35; Large = 40 }
    | MangoJuice  -> { Small = 35; Medium = 40; Large = 45 }

let sodaPrices  = { Small = 15; Medium = 20; Large = 25 }
let milkPrices  = { Small = 10; Medium = 15; Large = 20 }

// Extracts the correct price from a SizePrice record
let getPriceBySize size (prices : SizePrice) =
    match size with
    | Small  -> prices.Small
    | Medium -> prices.Medium
    | Large  -> prices.Large


// ========================================
// PRICE CALCULATION FUNCTIONS
// ========================================

// Computes the price of a drink based on category and size
let computeDrinkPrice (drink : Drink) =
    match drink.Category with
    | Coffee c -> coffeePrices c |> getPriceBySize drink.Size
    | Tea t    -> teaPrices t    |> getPriceBySize drink.Size
    | Juice j  -> juicePrices j  |> getPriceBySize drink.Size
    | Soda     -> getPriceBySize drink.Size sodaPrices
    | Milk     -> getPriceBySize drink.Size milkPrices

// Computes price of food items
let computeFoodPrice =
    function
    | Sandwich -> 50
    | Burger   -> 70
    | Salad    -> 45

// Computes price of fruit items
let computeFruitPrice =
    function
    | Apple  -> 10
    | Banana -> 8
    | Orange -> 12

// General function to compute the price of any product
let computeProductPrice =
    function
    | Drink d -> computeDrinkPrice d
    | Food f  -> computeFoodPrice f
    | Fruit f -> computeFruitPrice f


// ========================================
// VAT CALCULATION
// ========================================

// VAT percentage used in the system
let vatRate = 7.5

// Applies VAT to a given price
let applyVAT price =
    price + (price * vatRate / 100.0)


// ========================================
// AGENT IMPLEMENTATION (MailboxProcessor)
// ========================================

// Messages that the agent can handle
type OrderProductMsg =
    | Order of Product * quantity:int
    | LeaveComment of string

// Agent responsible for processing orders asynchronously
type GtgAgent() =

    let agent =
        MailboxProcessor<OrderProductMsg>.Start(fun inbox ->

            // Recursive loop that continuously processes messages
            let rec messageLoop () = async {

                let! msg = inbox.Receive()

                match msg with

                // Handles product orders
                | Order (product, quantity) ->

                    let basePrice =
                        float (computeProductPrice product) * float quantity

                    let totalPrice = applyVAT basePrice

                    printfn "Please pay DKK %.2f for %d %A(s). Thank you!"
                        totalPrice quantity product

                // Handles customer feedback
                | LeaveComment comment ->
                    printfn "Thank you for your feedback: %s" comment

                return! messageLoop ()
            }

            messageLoop ()
        )
    // Public method to post messages to the agent
    member _.Post message =
        agent.Post message


// ========================================
// TEST CASES
// ========================================

let agent = GtgAgent()

agent.Post (Order (Drink { Category = Coffee Latte; Size = Small }, 2))
agent.Post (Order (Food Burger, 1))
agent.Post (Order (Fruit Apple, 3))
agent.Post (LeaveComment "Great service!")