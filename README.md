## [early WIP] jamesmacaulay/elm-graphql

A [GraphQL](http://graphql.org) library for [Elm](http://elm-lang.org), written entirely in Elm.

(Not to be confused with [jahewson/elm-graphql](https://github.com/jahewson/elm-graphql), which is a great project that takes a different approach.)

This library is still a very early work-in-progress, but the goal is to provide a really good interface for working directly with GraphQL queries and schemas in Elm.

### Queries

With `elm-graphql`, building up a GraphQL query feels a lot like building a JSON decoder, especially if you're familiar with [elm-decode-pipeline](http://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest). First you define type aliases for each of the nested record types you want to construct out of the response:

```elm
import GraphQL.Query exposing (..)

type alias User =
    { name : String
    , photos : List Photo }

type alias Photo =
    { url : String
    , caption : String
    }
```

Then you build a query:

```elm
-- not all of this works yet...
userQuery : Query User
userQuery =
    let
        photo =
            object Photo
                |> withField "url" [] string
                |> withField "caption" [] string

        user =
            object User
                |> withField "name" [] string
                |> withField "photos" [] (list photo)
    in
        query [] (at [ "user" ] user)
```

Here's what the above query looks like when you encode it to a string to be sent to the server:

```graphql
{
  user {
    name
    photos {
      url
      caption
    }
  }
}
```

Aside from generating query strings, `Query` values let you do two other important things:
  
  * they can be validated against a GraphQL schema,
  * they can be used to decode JSON responses from the server.

In most cases you will probably want to do all of your application's query validation as part of your unit tests. This involves decoding a standard GraphQL introspection reponse into a `GraphQL.Schema.Schema` value, and using `GraphQL.Schema.validate`.

Assuming you've built a valid query for the server's schema, sending it to the server will result in a JSON response that can be decoded by the very same `Query` value. Here's what a JSON response for `userQuery` might look like:

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

When it is decoded with `userQuery`, it then becomes this:

```elm
{ name = "Lola"
, photos =
    [ { url = "http://cdn.catphotos.com/lola.jpg"
      , caption = "Lola curling up on the chair"
      }
    ]
}
```

Most of the time, though, you will probably want to use the more convenient functions in `GraphQL.Client` to manage the encoding, HTTP requesting, and decoding for you:

```elm
import GraphQL.Client as Client

userQueryTask : Task Client.Error User
userQueryTask =
    Client.performQuery userQuery myClient
```
