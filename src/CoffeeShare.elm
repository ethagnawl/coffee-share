module CoffeeShare where

import Dict exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html exposing (..)
import StartApp.Simple as StartApp

defaultAddress = {street = "22 Acacia Avenue",
                  apartment = "",
                  city = "New London",
                  state = "NH",
                  zipcode = "03257"}

addresses = fromList
  [
    ("Monica Geller", {street = "425 Grove Street",
                       apartment = "Apartment 20",
                       city = "New York",
                       state = "NY",
                       zipcode = "10014"})
  , ("Jerry Seinfeld", {street = "129 W 81st Street",
                        apartment = "5A",
                        city = "New York",
                        state = "NY",
                        zipcode = "10024"})
  , ("Abbie Hoffman", {street = "9 Bleecker Street",
                       apartment = "",
                       city = "New York",
                       state = "NY",
                       zipcode = "10012"})
  , ("Mark Twain", {street = "222 West 32rd Street",
                    apartment = "The Chelsea Hotel",
                    city = "New York",
                    state = "NY",
                    zipcode = "10011"})
  , ("Nikola Tesla", {street = "1032 6th Avenue",
                      apartment = "",
                      city = "New York",
                      state = "NY",
                      zipcode = "10018"})
  ]

monicaSends = fromList
  [
    ("March", "Mark Twain")
  , ("April", "Abbie Hoffman")
  , ("May", "Nikola Tesla")
  , ("June", "Jerry Seinfeld")
  ]

jerrySends = fromList
  [
    ("March", "Nikola Tesla")
  , ("April", "Monica Geller")
  , ("May", "Abbie Hoffman")
  , ("June", "Mark Twain")
  ]

abbieSends = fromList
  [
    ("March", "Jerry Seinfeld")
  , ("April", "Nikola Tesla")
  , ("May", "Mark Twain")
  , ("June", "Monica Geller")
  ]

markSends = fromList
  [
    ("March", "Abbie Hoffman")
  , ("April", "Jerry Seinfeld")
  , ("May", "Monica Geller")
  , ("June", "Nikola Tesla")
  ]

nikolaSends = fromList
  [
    ("March", "Monica Geller")
  , ("April", "Mark Twain")
  , ("May", "Jerry Seinfeld")
  , ("June", "Abbie Hoffman")
  ]

type alias Model = {
    user: String
  , action: String
  , month: String
}

model = Model "Monica" "Send" "March"

type Action name =
    UserChanged String
  | ActionChanged String
  | MonthChanged String
  | NoOp

update action model =
  case action of
    UserChanged newUser -> { model | user = newUser }
    ActionChanged newAction -> { model | action = newAction }
    MonthChanged newMonth -> { model | month = newMonth }
    NoOp -> model

view address model =
  let
    send_func = case model.user of
                  "Monica" -> monicaSends
                  "Jerry" -> jerrySends
                  "Abbie" -> abbieSends
                  "Mark" -> markSends
                  "Nikola" -> nikolaSends
                  _ -> Debug.crash "This should never happen."
    recipient = (Maybe.withDefault "" (get model.month send_func))
    recipientAddress = (Maybe.withDefault defaultAddress (get recipient addresses))
    showHasApartment = if recipientAddress.apartment == "" then "none" else "block"
    recipientAddress' = div [] [
          br [] []
        , p [] [text recipientAddress.street]
        , p [style [("display", showHasApartment)]] [text recipientAddress.apartment]
        , p [] [text (recipientAddress.city ++ ", " ++ recipientAddress.state ++ "  " ++ recipientAddress.zipcode)]
      ]
    summary = model.user ++ " sends to " ++ recipient ++ " at:"
  in
    Html.form
      []
      [
          legend [] [text "Coffee Share"]
        , div
            [class "mui-select"]
            [
              label [] [text "Select a participant"]
            , select
              [
                value model.user
              , on "change" targetValue (\newUser -> Signal.message address (UserChanged newUser))
              ]
              [
                option [] [text "Monica"]
              , option [] [text "Jerry"]
              , option [] [text "Abbie"]
              , option [] [text "Mark"]
              , option [] [text "Nikola"]
              ]
            ]

        , div
            [class "mui-select"]
            [
              label [] [text "Select an action"]
            , select
              [
                value model.user
              , on "change" targetValue (\newAction -> Signal.message address (ActionChanged newAction))
              ]
              [
                option [selected (model.action == "Send"), value "Send"] [text "Send"]
              -- , option [selected (model.action == "Receive"), value "Receive"] [text "Receive"]
              ]
            ]

        , div
            [class "mui-select"]
            [
              label [] [text "Select an month"]
            , select
              [
                value model.user
              , on "change" targetValue (\newMonth-> Signal.message address (MonthChanged newMonth))
              ]
              [
                option [] [text "March"]
              , option [] [text "April"]
              , option [] [text "May"]
              , option [] [text "June"]
              ]
            ]

        , div
          []
          [text summary]
        , recipientAddress'

      ]

main =
  StartApp.start {
    model = model
  , update = update
  , view = view
  }
