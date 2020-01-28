import { buildASTSchema } from 'graphql'
import gql from 'graphql-tag'

import { plugin, validate } from './index'

const fullConfig = {
  apiModule: 'Example.GraphQL.API',
  scalarsModule: 'Example.GraphQL.Scalars',
}

const schema = buildASTSchema(
  gql`
    enum MyEnum {
      Hello
      World
    }

    type Bar {
      id: ID!
      foo: String
    }

    type Query {
      foo: MyEnum
      bar(x: Int!): Bar
    }
  `
)

const documents = [
  {
    filePath: 'foo.graphql',
    content: gql`
      query getFoo {
        foo
      }

      query getBar($x: Int!) {
        bar(x: $x) {
          id
          foo
        }
      }
    `,
  },
]

it('validates', () => {
  expect(() => validate(schema, documents, fullConfig, '', [])).not.toThrow()
})

it('renders', () => {
  expect(plugin(schema, documents, fullConfig)).toMatchInlineSnapshot(`
    "{-# LANGUAGE DataKinds #-}
    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE QuasiQuotes #-}
    {-# LANGUAGE RecordWildCards #-}
    {-# LANGUAGE TemplateHaskell #-}
    {-# LANGUAGE TypeFamilies #-}
    {-# OPTIONS_GHC -fno-warn-unused-imports #-}

    module Example.GraphQL.API where

    import Control.Monad.IO.Class (MonadIO)
    import Data.Aeson (object, (.=))
    import Data.Aeson.Schema.TH (mkEnum)
    import Data.GraphQL
    import Data.Text (Text)

    import Example.GraphQL.Scalars

    {-----------------------------------------------------------------------------
    * MyEnum
    -----------------------------------------------------------------------------}

    mkEnum \\"MyEnum\\"
      [ \\"Hello\\"
      , \\"World\\"
      ]

    {-----------------------------------------------------------------------------
    * getFoo

    -- result :: Object GetFooSchema; throws a GraphQL exception on errors
    result <- runGetFooQuery GetFooArgs
      {
      }

    -- result :: GraphQLResult (Object GetFooSchema)
    result <- runGetFooQuerySafe GetFooArgs
      {
      }
    -----------------------------------------------------------------------------}

    type GetFooQuery = Query GetFooArgs GetFooSchema

    data GetFooArgs = GetFooArgs
      {
      }
      deriving (Show)

    type GetFooSchema = [schema|
      {
        foo: Maybe MyEnum,
      }
    |]

    instance GraphQLArgs GetFooArgs where
      fromArgs args = object
        [
        ]

    getFooQuery :: GetFooQuery
    getFooQuery = UnsafeQuery \\"getFoo\\" [query|
      query getFoo {
        foo
      }
    |]

    runGetFooQuery :: (MonadIO m, MonadQuery m)
      => GetFooArgs -> m (Object GetFooSchema)
    runGetFooQuery = runQuery getFooQuery

    runGetFooQuerySafe :: (MonadIO m, MonadQuery m)
      => GetFooArgs -> m (GraphQLResult (Object GetFooSchema))
    runGetFooQuerySafe = runQuerySafe getFooQuery
    {-----------------------------------------------------------------------------
    * getBar

    -- result :: Object GetBarSchema; throws a GraphQL exception on errors
    result <- runGetBarQuery GetBarArgs
      { _x = ...
      }

    -- result :: GraphQLResult (Object GetBarSchema)
    result <- runGetBarQuerySafe GetBarArgs
      { _x = ...
      }
    -----------------------------------------------------------------------------}

    type GetBarQuery = Query GetBarArgs GetBarSchema

    data GetBarArgs = GetBarArgs
      { _x :: Int
      }
      deriving (Show)

    type GetBarSchema = [schema|
      {
        bar: Maybe {
          id: Text,
          foo: Maybe Text,
        },
      }
    |]

    instance GraphQLArgs GetBarArgs where
      fromArgs args = object
        [ \\"x\\" .= _x args
        ]

    getBarQuery :: GetBarQuery
    getBarQuery = UnsafeQuery \\"getBar\\" [query|
      query getBar($x: Int!) {
        bar(x: $x) {
          id
          foo
        }
      }
    |]

    runGetBarQuery :: (MonadIO m, MonadQuery m)
      => GetBarArgs -> m (Object GetBarSchema)
    runGetBarQuery = runQuery getBarQuery

    runGetBarQuerySafe :: (MonadIO m, MonadQuery m)
      => GetBarArgs -> m (GraphQLResult (Object GetBarSchema))
    runGetBarQuerySafe = runQuerySafe getBarQuery
    "
  `)
})

it('renders without apiModule in config', () => {
  const config = { ...fullConfig }
  delete config.apiModule

  const renderedWithOutputFile = plugin(schema, documents, config, {
    outputFile: 'path/to/MyModule/Api.hs',
  })
  const renderedWithConfig = plugin(schema, documents, {
    ...config,
    apiModule: 'MyModule.Api',
  })

  expect(renderedWithOutputFile).toEqual(renderedWithConfig)
})