## [pre-release] jamesmacaulay/elm-graphql

A [GraphQL](http://graphql.org) library for [Elm](http://elm-lang.org), written entirely in Elm.

(Not to be confused with [jahewson/elm-graphql](https://github.com/jahewson/elm-graphql), which is a great project that takes a different approach.)

This library is still a work in progress, but the goal is to provide a really good interface for working directly with GraphQL queries and schemas in Elm.

Here's [a minimal end-to-end example](https://github.com/jamesmacaulay/elm-graphql/tree/master/example) that builds a query, sends it to a server, and decodes the response.

The interfaces provided by this library are still unstable and I expect them to change before I publish this library to the elm package repository.

### Building requests

Building up a GraphQL query with this library feels a lot like building a JSON decoder. First you define type aliases for each of the nested record types you want to construct out of the response:


```elm
import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Arg as Arg
import GraphQL.Request.Builder.Variable as Var

type alias Photo =
    { url : String
    , caption : String
    }

type alias User =
    { name : String
    , photos : List Photo }
```

Then you build a query document:

```elm
userQuery : Document Query { vars | userID : String } User
userQuery =
    let
        userIDVar =
            Var.required "userID" .userID Var.id

        photo =
            object Photo
                |> withField "url" [] string
                |> withField "caption" [] string

        user =
            object User
                |> withField "name" [] string
                |> withField "photos" [] (list photo)
        
        queryRoot =
            field "user"
                [ args [ ( "id", Arg.variable userIDVar ) ] ]
                user
    in
        queryDocument queryRoot
```

The `Document` type can represent both query and mutation documents. It lets you do two important things:
  
  * generate GraphQL request documents to send to the server, and
  * decode JSON responses from the server.

Here's what the above Document looks like when you encode it to a string to be sent to the server:

```graphql
query ($userID: ID!) {
  user(id: $userID) {
    name
    photos {
      url
      caption
    }
  }
}
```

To supply values for the variables used in the `Document`, you provide an Elm value that your variables can extract values from according to the getter functions supplied when you define the variables. In this example, the `"userID"` variable was defined as a string that's extracted from the `userID` field of some Elm record, so the following code is a valid way to create a `Request` with the required variable values supplied:

```elm
userQueryRequest : Request Query User
userQueryRequest =
    userQuery
        |> request { userID = "123" }
```

Assuming you've built a query that is valid for the server's schema, sending it to the server will result in a JSON response that can be decoded by the very same `Query User` value, using a JSON decoder that is built up along with the structure of the query. Here's what a JSON response for `userQuery` might look like:

```
{
  "data": {
    "user": {
      "name": "Lola",
      "photos": [
        {
          "url": "http://cdn.catphotos.com/lola.jpg",
          "caption": "Lola curling up on the chair"
        }
      ]
    }
  }
}
```

When it is decoded with the help of the decoder contained in `userQuery`, it becomes this:

```elm
{ name = "Lola"
, photos =
    [ { url = "http://cdn.catphotos.com/lola.jpg"
      , caption = "Lola curling up on the chair"
      }
    ]
}
```
