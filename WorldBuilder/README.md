## Requirements

* [dotnet SDK](https://www.microsoft.com/net/download/core) 2.0 or higher
* [node.js](https://nodejs.org) 6.11 or higher
* A JS package manager: [yarn](https://yarnpkg.com) or [npm](http://npmjs.com/)

## Building and running the app

* Install JS dependencies: `npm install`
* Install F# dependencies: `dotnet restore`
* Start Fable daemon and [Webpack](https://webpack.js.org/): `npm start`
* In another terminal, run: `npm run launch`

> The first two steps are only necessary the first time or whenever the dependencies change.

The app window will be refreshed when you modify any file in the Renderer project. For production, run `npm run build` to get optimized JS code.
