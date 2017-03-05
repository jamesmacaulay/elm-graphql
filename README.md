## [early WIP] jamesmacaulay/elm-graphql

A [GraphQL](http://graphql.org) library for [Elm](http://elm-lang.org), written entirely in Elm.

(Not to be confused with [jahewson/elm-graphql](https://github.com/jahewson/elm-graphql), which is a great project that takes a different approach.)

This library is still a very early work-in-progress, but the goal is to provide a really good interface for working directly with GraphQL queries and schemas in Elm.

Here's [a minimal end-to-end example](https://github.com/jamesmacaulay/elm-graphql/tree/master/example) that builds a query, sends it to a server, and decodes the response.

The interfaces provided by this library are still unstable and I expect them to change before I publish this library to the elm package repository.

### Building requests

Building up a GraphQL query with this library feels a lot like building a JSON decoder. First you define type aliases for each of the nested record types you want to construct out of the response:


```elm
import GraphQL.Request.Builder exposing (..)

type alias Photo =
    { url : String
    , caption : String
    }

type alias User =
    { name : String
    , photos : List Photo }
```

Then you build a query operation:

```elm
userQuery : Operation Query User
userQuery =
    let
        photo =
            map2 Photo
                (field "url" [] string)
                (field "caption" [] string)

        user =
            map2 User
                (field "name" [] string)
                (field "photos" [] (list photo))
    in
        query [] (field "user" [] user)
```

You can also use a pipeline style for constructing your queries, similar to [elm-decode-pipeline](http://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest). The following code is equivalent to the last example:

```elm
userQuery : Operation Query User
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
        query [] (field "user" [] user)
```

The tradeoff with the pipeline DSL is that the errors you get from the compiler can be more difficult to understand when you don't have the types lined up properly.

The `Operation` type can represent both query and mutation operations. It lets you do two important things:
  
  * generate GraphQL request documents to send to the server, and
  * decode JSON responses from the server.

Here's what the above Operation looks like when you encode it to a string to be sent to the server:

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

Assuming you've built a valid query for the server's schema, sending it to the server will result in a JSON response that can be decoded by the very same `Query User` value, using a JSON decoder that is built up along with the structure of the query. Here's what a JSON response for `userQuery` might look like:

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
