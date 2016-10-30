## elm-graphql SWAPI example

To run this example:

1. clone the [swapi-graphql](https://github.com/graphql/swapi-graphql) project
2. edit [src/cloud/main.js](https://github.com/graphql/swapi-graphql/blob/master/src/cloud/main.js) and add `app.use(express.static('src/public'));` right below `const app = express;`
3. `npm install`
4. `rm src/api/cachedData/cache.js`
5. `npm run download`
6. `npm start` and make a note of the server's localhost URL
7. cd back to this `example` directory
8. `elm-make Main.elm --output ~/path/to/swapi-graphql/src/public/elm-graphql.html`
9. open up `http://localhost:PORT/elm-graphql.html`
