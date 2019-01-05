module Models exposing (Account, AccountSubType(..), AccountType(..), Accounts(..), Institution, Item, Product(..), Stage(..))


type alias Account =
    { name : String
    , id : String
    , type_ : AccountType
    , subType : AccountSubType
    , mask : String
    }


type Accounts
    = Accounts (List Account)


type AccountType
    = Depository
    | Credit
    | Brokerage
    | Loan
    | OtherType


type AccountSubType
    = CD
    | Checking
    | Savings
    | MoneyMarket
    | Paypal
    | Prepaid
    | CreditCard
    | Rewards
    | OtherSubType


type alias Institution =
    { name : String
    , id : String
    }


type alias Item =
    { public_token : String
    , institution : Institution
    , accounts : List Account
    }


type Product
    = Auth
    | Transactions


type Stage
    = Start
    | PickDepositoryAccount
    | LinkAnotherBank
    | Finish
