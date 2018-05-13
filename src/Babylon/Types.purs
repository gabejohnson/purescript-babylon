module Babylon.Types
       ( Node(..)
       , Node'
       , ForInStatement'
       , Function'
       , ObjectProperty'
       , ObjectMember
       , BinaryExpression'
       , CallExpression'
       , Class
       , ModuleSpecifier
       , SourceLocation
       , Position
       , LogicalOperator(..)
       , AssignmentOperator(..)
       , BinaryOperator(..)
       , UpdateOperator(..)
       , UnaryOperator(..)
       , MethodKind(..)
       , SourceType(..)
       , VariableKind(..)
       , parseExpression
       , parseExpression'
       , parse
       , parse'
       ) where

import Prelude

import Data.Foreign (F, Foreign, ForeignError(..), fail, toForeign)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Record (insert)
import Simple.JSON (class ReadForeign, class WriteForeign, read', write, writeJSON)
import Type.Data.Symbol (SProxy(..))
import Type.Prelude (class RowLacks)


instance writeForeignNode :: WriteForeign Node where
  writeImpl n =
    let insertType
          :: forall r
           . RowLacks "type" r
          => String
          -> { | r }
          -> { type :: String | r }
        insertType = insert (SProxy :: SProxy "type")
    in do
    case n of
      Identifier               r -> write $ insertType "Identifier" r
      PrivateName              r -> write $ insertType "PrivateName" r
      RegExpLiteral            r -> write $ insertType "RegExpLiteral" r
      NullLiteral              r -> write $ insertType "NullLiteral" r
      StringLiteral            r -> write $ insertType "StringLiteral" r
      BooleanLiteral           r -> write $ insertType "BooleanLiteral" r
      NumericLiteral           r -> write $ insertType "NumericLiteral" r
      File                     r -> write $ insertType "File" r
      Program                  r -> write $ insertType "Program" r
      ExpressionStatement      r -> write $ insertType "ExpressionStatement" r
      BlockStatement           r -> write $ insertType "BlockStatement" r
      EmptyStatement           r -> write $ insertType "EmptyStatement" r
      DebuggerStatement        r -> write $ insertType "DebuggerStatement" r
      WithStatement            r -> write $ insertType "WithStatement" r
      ReturnStatement          r -> write $ insertType "ReturnStatement" r
      LabeledStatement         r -> write $ insertType "LabeledStatement" r
      BreakStatement           r -> write $ insertType "BreakStatement" r
      ContinueStatement        r -> write $ insertType "ContinueStatement" r
      IfStatement              r -> write $ insertType "IfStatement" r
      SwitchStatement          r -> write $ insertType "SwitchStatement" r
      SwitchCase               r -> write $ insertType "SwitchCase" r
      ThrowStatement           r -> write $ insertType "ThrowStatement" r
      TryStatement             r -> write $ insertType "TryStatement" r
      CatchClause              r -> write $ insertType "CatchClause" r
      WhileStatement           r -> write $ insertType "WhileStatement" r
      DoWhileStatement         r -> write $ insertType "DoWhileStatement" r
      ForStatement             r -> write $ insertType "ForStatement" r
      ForInStatement           r -> write $ insertType "ForInStatement" r
      ForOfStatement           r -> write $ insertType "ForOfStatement" r
      FunctionDeclaration      r -> write $ insertType "FunctionDeclaration" r
      VariableDeclaration      r -> write $ insertType "VariableDeclaration" r
      VariableDeclarator       r -> write $ insertType "VariableDeclarator" r
      Decorator                r -> write $ insertType "Decorator" r
      Directive                r -> write $ insertType "Directive" r
      DirectiveLiteral         r -> write $ insertType "DirectiveLiteral" r
      Super                    r -> write $ insertType "Super" r
      Import                   r -> write $ insertType "Import" r
      ThisExpression           r -> write $ insertType "ThisExpression" r
      ArrowFunctionExpression  r -> write $ insertType "ArrowFunctionExpression" r
      YieldExpression          r -> write $ insertType "YieldExpression" r
      AwaitExpression          r -> write $ insertType "AwaitExpression" r
      ArrayExpression          r -> write $ insertType "ArrayExpression" r
      ObjectExpression         r -> write $ insertType "ObjectExpression" r
      ObjectProperty           r -> write $ insertType "ObjectProperty" r
      ObjectMethod             r -> write $ insertType "ObjectMethod" r
      FunctionExpression       r -> write $ insertType "FunctionExpression" r
      UnaryExpression          r -> write $ insertType "UnaryExpression" r
      UpdateExpression         r -> write $ insertType "UpdateExpression" r
      BinaryExpression         r -> write $ insertType "BinaryExpression" r
      AssignmentExpression     r -> write $ insertType "AssignmentExpression" r
      LogicalExpression        r -> write $ insertType "LogicalExpression" r
      SpreadElement            r -> write $ insertType "SpreadElement" r
      MemberExpression         r -> write $ insertType "MemberExpression" r
      BindExpression           r -> write $ insertType "BindExpression" r
      ConditionalExpression    r -> write $ insertType "ConditionalExpression" r
      CallExpression           r -> write $ insertType "CallExpression" r
      -- "NewExpression"            -> NewExpression            <$> read' f
      -- "SequenceExpression"       -> SequenceExpression       <$> read' f
      -- "DoExpression"             -> DoExpression             <$> read' f
      -- "TemplateLiteral"          -> TemplateLiteral          <$> read' f
      -- "TaggedTemplateLiteral"    -> TaggedTemplateLiteral    <$> read' f
      -- "TemplateElement"          -> TemplateElement          <$> read' f
      -- "AssignmentProperty"       -> AssignmentProperty       <$> read' f
      -- "ObjectPattern"            -> ObjectPattern            <$> read' f
      -- "ArrayPattern"             -> ArrayPattern             <$> read' f
      -- "RestElement"              -> RestElement              <$> read' f
      -- "AssignmentPattern"        -> AssignmentPattern        <$> read' f
      -- "ClassBody"                -> ClassBody                <$> read' f
      -- "ClassMethod"              -> ClassMethod              <$> read' f
      -- "ClassPrivateMethod"       -> ClassPrivateMethod       <$> read' f
      -- "ClassProperty"            -> ClassProperty            <$> read' f
      -- "ClassPrivateProperty"     -> ClassPrivateProperty     <$> read' f
      -- "ClassDeclaration"         -> ClassDeclaration         <$> read' f
      -- "ClassExpression"          -> ClassExpression          <$> read' f
      -- "MetaProperty"             -> MetaProperty             <$> read' f
      -- "ImportDeclaration"        -> ImportDeclaration        <$> read' f
      -- "ImportDefaultSpecifier"   -> ImportDefaultSpecifier   <$> read' f
      -- "ImportNamespaceSpecifier" -> ImportNamespaceSpecifier <$> read' f
      -- "ExportNamedDeclaration"   -> ExportNamedDeclaration   <$> read' f
      -- "ExportSpecifier"          -> ExportSpecifier          <$> read' f
      -- "OptFunctionDeclaration"   -> OptFunctionDeclaration   <$> read' f
      -- "OptClassDeclaration"      -> OptClassDeclaration      <$> read' f
      -- "ExportDefaultDeclaration" -> ExportDefaultDeclaration <$> read' f
      ExportAllDeclaration     r -> write $ insertType "ExportAllDeclaration" r
      _                          -> write { type: "Unknown" }

instance readForeignNode :: ReadForeign Node where
  readImpl f = do
    r :: {type :: String} <- read' f
    case r."type" of
      "Identifier"               -> Identifier               <$> read' f
      "PrivateName"              -> PrivateName              <$> read' f
      "RegExpLiteral"            -> RegExpLiteral            <$> read' f
      "NullLiteral"              -> NullLiteral              <$> read' f
      "StringLiteral"            -> StringLiteral            <$> read' f
      "BooleanLiteral"           -> BooleanLiteral           <$> read' f
      "NumericLiteral"           -> NumericLiteral           <$> read' f
      "File"                     -> File                     <$> read' f
      "Program"                  -> Program                  <$> read' f
      "ExpressionStatement"      -> ExpressionStatement      <$> read' f
      "BlockStatement"           -> BlockStatement           <$> read' f
      "EmptyStatement"           -> EmptyStatement           <$> read' f
      "DebuggerStatement"        -> DebuggerStatement        <$> read' f
      "WithStatement"            -> WithStatement            <$> read' f
      "ReturnStatement"          -> ReturnStatement          <$> read' f
      "LabeledStatement"         -> LabeledStatement         <$> read' f
      "BreakStatement"           -> BreakStatement           <$> read' f
      "ContinueStatement"        -> ContinueStatement        <$> read' f
      "IfStatement"              -> IfStatement              <$> read' f
      "SwitchStatement"          -> SwitchStatement          <$> read' f
      "SwitchCase"               -> SwitchCase               <$> read' f
      "ThrowStatement"           -> ThrowStatement           <$> read' f
      "TryStatement"             -> TryStatement             <$> read' f
      "CatchClause"              -> CatchClause              <$> read' f
      "WhileStatement"           -> WhileStatement           <$> read' f
      "DoWhileStatement"         -> DoWhileStatement         <$> read' f
      "ForStatement"             -> ForStatement             <$> read' f
      "ForInStatement"           -> ForInStatement           <$> read' f
      "ForOfStatement"           -> ForOfStatement           <$> read' f
      "FunctionDeclaration"      -> FunctionDeclaration      <$> read' f
      "VariableDeclaration"      -> VariableDeclaration      <$> read' f
      "VariableDeclarator"       -> VariableDeclarator       <$> read' f
      "Decorator"                -> Decorator                <$> read' f
      "Directive"                -> Directive                <$> read' f
      "DirectiveLiteral"         -> DirectiveLiteral         <$> read' f
      "Super"                    -> Super                    <$> read' f
      "Import"                   -> Import                   <$> read' f
      "ThisExpression"           -> ThisExpression           <$> read' f
      "ArrowFunctionExpression"  -> ArrowFunctionExpression  <$> read' f
      "YieldExpression"          -> YieldExpression          <$> read' f
      "AwaitExpression"          -> AwaitExpression          <$> read' f
      "ArrayExpression"          -> ArrayExpression          <$> read' f
      "ObjectExpression"         -> ObjectExpression         <$> read' f
      "ObjectProperty"           -> ObjectProperty           <$> read' f
      "ObjectMethod"             -> ObjectMethod             <$> read' f
      "FunctionExpression"       -> FunctionExpression       <$> read' f
      "UnaryExpression"          -> UnaryExpression          <$> read' f
      "UpdateExpression"         -> UpdateExpression         <$> read' f
      "BinaryExpression"         -> BinaryExpression         <$> read' f
      "AssignmentExpression"     -> AssignmentExpression     <$> read' f
      "LogicalExpression"        -> LogicalExpression        <$> read' f
      "SpreadElement"            -> SpreadElement            <$> read' f
      "MemberExpression"         -> MemberExpression         <$> read' f
      "BindExpression"           -> BindExpression           <$> read' f
      "ConditionalExpression"    -> ConditionalExpression    <$> read' f
      "CallExpression"           -> CallExpression           <$> read' f
      "NewExpression"            -> NewExpression            <$> read' f
      "SequenceExpression"       -> SequenceExpression       <$> read' f
      "DoExpression"             -> DoExpression             <$> read' f
      "TemplateLiteral"          -> TemplateLiteral          <$> read' f
      "TaggedTemplateLiteral"    -> TaggedTemplateLiteral    <$> read' f
      "TemplateElement"          -> TemplateElement          <$> read' f
      "AssignmentProperty"       -> AssignmentProperty       <$> read' f
      "ObjectPattern"            -> ObjectPattern            <$> read' f
      "ArrayPattern"             -> ArrayPattern             <$> read' f
      "RestElement"              -> RestElement              <$> read' f
      "AssignmentPattern"        -> AssignmentPattern        <$> read' f
      "ClassBody"                -> ClassBody                <$> read' f
      "ClassMethod"              -> ClassMethod              <$> read' f
      "ClassPrivateMethod"       -> ClassPrivateMethod       <$> read' f
      "ClassProperty"            -> ClassProperty            <$> read' f
      "ClassPrivateProperty"     -> ClassPrivateProperty     <$> read' f
      "ClassDeclaration"         -> ClassDeclaration         <$> read' f
      "ClassExpression"          -> ClassExpression          <$> read' f
      "MetaProperty"             -> MetaProperty             <$> read' f
      "ImportDeclaration"        -> ImportDeclaration        <$> read' f
      "ImportDefaultSpecifier"   -> ImportDefaultSpecifier   <$> read' f
      "ImportNamespaceSpecifier" -> ImportNamespaceSpecifier <$> read' f
      "ExportNamedDeclaration"   -> ExportNamedDeclaration   <$> read' f
      "ExportSpecifier"          -> ExportSpecifier          <$> read' f
      "OptFunctionDeclaration"   -> OptFunctionDeclaration   <$> read' f
      "OptClassDeclaration"      -> OptClassDeclaration      <$> read' f
      "ExportDefaultDeclaration" -> ExportDefaultDeclaration <$> read' f
      "ExportAllDeclaration"     -> ExportAllDeclaration     <$> read' f
      _                          -> fail $ ForeignError "Doesn't match any known node type"

data Node = Identifier  (Node' ( name :: String ))
          | PrivateName (Node' ( id :: Node {- Identifier -}))

            -- Literals
          | RegExpLiteral  (Node' ( pattern :: String
                                  , flags :: String
                                  )
                           )
          | NullLiteral    (Node' ())
          | StringLiteral  (Node' ( value :: String ))
          | BooleanLiteral (Node' ( value :: Boolean ))
          | NumericLiteral (Node' ( value :: Number ))

            -- Program
          | File    (Node' ( program :: Node          {- Program -}))
          | Program (Node' ( sourceType :: SourceType
                           , body :: Array Node       -- Statement | ModuleDeclaration
                           , directives :: Array Node -- Directive
                           )
                    )

            -- Statements
          | ExpressionStatement (Node' ( expression :: Node       {- Expression -}))
          | BlockStatement      (Node' ( body :: Array Node       -- Statement
                                       , directives :: Array Node -- Directive
                                       )
                                )
          | EmptyStatement      (Node' ())
          | DebuggerStatement   (Node' ())
          | WithStatement       (Node' ( object :: Node           -- Expression
                                       , body :: Node             -- Statement
                                       )
                                )

            -- Control Flow
          | ReturnStatement   (Node' ( argument :: Maybe Node {- Expression -}))
          | LabeledStatement  (Node' ( label :: Node          -- Identitifier
                                     , body :: Node           -- Statement
                                     )
                              )

          | BreakStatement    (Node' ( label :: Maybe Node    {- Identitifier -}))
          | ContinueStatement (Node' ( label :: Maybe Node    {- Identifier -}))

            -- Choice
          | IfStatement     (Node' ( test :: Node             -- Expression
                                   , consequent :: Node       -- Statement
                                   , alternate :: Maybe Node  -- Statement
                                   )
                            )
          | SwitchStatement (Node' ( discriminant :: Node     -- Expression
                                   , cases :: Array Node      -- SwitchCase
                                   )
                            )
          | SwitchCase      (Node' ( test :: Maybe Node       -- Expression
                                   , consequent :: Array Node -- Statement
                                   )
                            )

            -- Exceptions
          | ThrowStatement (Node' ( argument :: Node        {- Expression -}))
          | TryStatement   (Node' ( block :: Node           -- BlockStatement
                                  , handler :: Maybe Node   -- CatchClause
                                  , finalizer :: Maybe Node -- BlockStatement
                                  )
                           )
          | CatchClause    (Node' ( param :: Node           -- Pattern
                                  , body :: Node            -- BlockStatement
                                  )
                           )

            -- Loops
          | WhileStatement   (Node' ( test :: Node         -- Expression
                                    , body :: Node         -- Statement
                                    )
                             )
          | DoWhileStatement (Node' ( body :: Node         -- Statement
                                    , test :: Node         -- Expression
                                    )
                             )
          | ForStatement     (Node' ( init :: Maybe Node   -- VariableDeclaration | Expression
                                    , test :: Maybe Node   -- Expression
                                    , update :: Maybe Node -- Expression
                                    , body :: Node         -- Statement
                                    )
                             )
          | ForInStatement   (ForInStatement' ())
          | ForOfStatement   (ForInStatement' ( await :: Boolean ))

            -- Declarations
          | FunctionDeclaration (Node' (Function' ( id :: Node )))
          | VariableDeclaration (Node' ( declarations :: Array Node -- VariableDeclarator
                                       , kind :: VariableKind
                                       )
                                )
          | VariableDeclarator  (Node' ( id :: Node                 -- Identifier | Pattern
                                       , init :: Maybe Node         -- Expression
                                       )
                                )

            -- Misc
          | Decorator        (Node' ( expression :: Node {- Expression -}))
          | Directive        (Node' ( value :: Node      {- DirectiveLiteral -} ))
          | DirectiveLiteral (Node' ( value :: String ))

            -- Expressions
          | Super                   (Node' ())
          | Import                  (Node' ())
          | ThisExpression          (Node' ())
          | ArrowFunctionExpression (Node' (Function' ( id :: Maybe Node    {- Identifier -})))
          | YieldExpression         (Node' ( argument :: Maybe Node         -- Expression
                                           , delegate :: Boolean
                                           )
                                    )
          | AwaitExpression         (Node' ( argument :: Maybe Node         {- Expression -}))
          | ArrayExpression         (Node' ( elements :: Array (Maybe Node) {- Expression | SpreadElement -}))
          | ObjectExpression        (Node' ( properties :: Array Node       {- [ObjectProperty | ObjectMethod | SpreadElement] -}))
          | ObjectProperty          ObjectProperty'
          | ObjectMethod            (ObjectMember (Function' ( kind :: MethodKind )))
          | FunctionExpression      (Node' (Function' ( id :: Maybe Node     {- Identifier -})))
          | UnaryExpression         (Node' ( operator :: UnaryOperator
                                           , prefix :: Boolean
                                           , argument :: Node               -- Expression
                                           )
                                    )
          | UpdateExpression        (Node' ( operator :: UpdateOperator
                                           , argument :: Node               -- Expression
                                           , prefix :: Boolean
                                           )
                                    )

            -- Binary Operations
          | BinaryExpression      (BinaryExpression' ( operator :: BinaryOperator ))
          | AssignmentExpression  (BinaryExpression' ( operator :: AssignmentOperator ))
          | LogicalExpression     (BinaryExpression' ( operator :: LogicalOperator ))
          | SpreadElement         (Node' ( argument :: Node          {- Expression -}))
          | MemberExpression      (Node' ( object :: Node            -- Expression | Pattern
                                         , property :: Node          -- Expression
                                         , computed :: Boolean
                                         , optional :: Maybe Boolean
                                         )
                                  )
          | BindExpression        (Node' ( object :: Maybe Node      -- Expression
                                         , callee :: Node            -- Expression
                                         )
                                  )
          | ConditionalExpression (Node' ( test :: Node              -- Expression
                                         , alternate :: Node         -- Expression
                                         , consequent :: Node        -- Expression
                                         )
                                  )
          | CallExpression        CallExpression'
          | NewExpression         CallExpression'
          | SequenceExpression    (Node' ( expressions :: Array Node {- Expression -}))
          | DoExpression          (Node' ( body :: Node              {- BlockStatement -}))

            -- Template Literals
          | TemplateLiteral       (Node' ( quasis :: Array Node      -- TemplateElement
                                         , expressions :: Array Node -- Expression
                                         )
                                  )
          | TaggedTemplateLiteral (Node' ( tag :: Node               -- Expression
                                         , quasi :: Node             -- TemplateLiteral
                                         )
                                  )
          | TemplateElement       (Node' ( tail :: Boolean
                                         , value :: { cooked :: Maybe String
                                                    , raw :: String
                                                    }
                                         )
                                  )

            -- Patterns
          | AssignmentProperty ObjectProperty'
          | ObjectPattern      (Node' ( properties :: Array Node       {- AssignmentProperty | RestElement -}))
          | ArrayPattern       (Node' ( elements :: Array (Maybe Node) {- Pattern -}))
          | RestElement        (Node' ( argument :: Node               {- Pattern -}))
          | AssignmentPattern  (Node' ( left :: Node                   -- Pattern
                                      , right :: Node                  -- Pattern
                                      )
                               )

            -- Classes
          | ClassBody            (Node' ( body :: Array Node                 {- ClassMethod | ClassPrivateMethod | ClassProperty | ClassPrivateProperty -}))

          | ClassMethod          (Node' (Function' ( key :: Node             -- Expression
                                                   , kind :: MethodKind
                                                   , computed :: Boolean
                                                   , static :: Boolean
                                                   , decorators :: Maybe (Array Node) -- Decorator
                                                   )
                                        )
                                 )
          | ClassPrivateMethod   (Node' (Function' ( key :: Node             -- PrivateName
                                                   , kind :: MethodKind
                                                   , static :: Boolean
                                                   , decorators :: Maybe (Array Node) -- Decorator
                                                   )
                                        )
                                 )
          | ClassProperty        (Node' ( key :: Node                        -- Expression
                                        , value :: Node                      -- Expression
                                        , static :: Boolean
                                        , computed :: Boolean
                                        )
                                 )
          | ClassPrivateProperty (Node' ( key :: Node                        -- PrivateName
                                        , value :: Node                      -- Expression
                                        , static :: Boolean
                                        )
                                 )
          | ClassDeclaration     (Class ( id :: Node ))
          | ClassExpression      (Class ( id :: Maybe Node                   {- Identifier -}))
          | MetaProperty         (Node' ( meta :: Node                       -- Identifier
                                        , property :: Node                   -- Identifier
                                        )
                                 )

            -- Modules
            -- Imports
          | ImportDeclaration        (Node' ( source :: Node           -- Literal
                                            , specifiers :: Array Node -- ImportSpecifier | ImportDefaultSpecifier | ImportNamespaceSpecifier
                                            )
                                     )
          | ImportSpecifier          (ModuleSpecifier ( imported :: Node {- Identifier -}))
          | ImportDefaultSpecifier   (ModuleSpecifier ())
          | ImportNamespaceSpecifier (ModuleSpecifier ())

            -- Exports
          | ExportNamedDeclaration   (Node' ( declaration :: Maybe Node       -- Declaration
                                            , specifiers :: Array Node        -- ExportSpecifier
                                            , source :: Maybe Node            -- Literal
                                            )
                                     )
          | ExportSpecifier          (ModuleSpecifier ( exported :: Node      {- Identifier -}))
          | OptFunctionDeclaration   (Node' (Function' ( id :: Maybe Node {- Identifier -})))
          | OptClassDeclaration      (Class ( id :: Maybe Node    {- Identifier -}))
          | ExportDefaultDeclaration (Node' (declaration :: Node              {- OptFunctionDeclaration | OptClassDeclaration | Expression -}))
          | ExportAllDeclaration     (Node' ( source :: Node                  {- Literal -}))

instance showNode :: Show Node where
  show = writeJSON

type Position = { line :: Int
                , column :: Int
                }

type SourceLocation = { source :: Maybe String
                      , start :: Position
                      , end :: Position
                      }



type Node' r             = { loc :: Maybe SourceLocation | r }

type Function' r         = ( params :: Array Node -- Pattern
                           , generator :: Boolean
                           , body :: Node         -- BlockStatement | Expression
                           , async :: Boolean
                           | r
                           )

type Class r             = Node' ( superClass :: Maybe Node -- Expression
                                 , body :: Node             -- ClassBody
                                 , decorators :: Maybe (Array Node) -- Decorator
                                 | r
                                 )

type CallExpression'     = Node' ( callee :: Node          -- Expression | Super | Import
                                 , arguments :: Array Node -- Expression | SpreadElement
                                 , optional :: Maybe Boolean
                                 )

type BinaryExpression' r = Node' ( left :: Node  -- Expression
                                 , right :: Node -- Expression
                                 | r
                                 )

type ObjectProperty'     = ObjectMember ( shorthand :: Boolean
                                        , value :: Node -- Expression
                                        )

type ObjectMember r      = Node' ( key :: Node              -- Expression
                                 , computed :: Boolean
                                 , decorators :: Maybe (Array Node) -- Decorator
                                 | r
                                 )

type ForInStatement' r   = Node' ( left :: Node       -- VariableDeclaration | Expression
                                 , right :: Node      -- Expression
                                 , body :: Node       -- Statements
                                 | r
                                 )

type ModuleSpecifier r   = Node' ( local :: Node | r )



data LogicalOperator = Or | And

derive instance eqLogicalOperator :: Eq LogicalOperator
instance showLogicalOperator :: Show LogicalOperator where
  show Or  = "||"
  show And = "&&"

instance readForeignLogicalOperator :: ReadForeign LogicalOperator where
  readImpl f = do
    v <- read' f
    case v of
      "||"    -> pure Or
      "&&"    -> pure And
      _       -> fail $ ForeignError "Unsupported logical operator"

defaultWriteImpl :: forall a. Show a => a -> Foreign
defaultWriteImpl = toForeign <<< show

instance writeForeignOperator :: WriteForeign LogicalOperator where
  writeImpl = defaultWriteImpl

data AssignmentOperator = AssignmentOperator (Maybe BinaryOperator)

instance readForeignAssignmentOperator :: ReadForeign AssignmentOperator where
  readImpl f = do
    v <- read' f
    case v of
      "="    -> pure $ AssignmentOperator Nothing
      "<<="  -> assign LeftShift
      ">>="  -> assign RightShift
      ">>>=" -> assign ZeroFillRightShift
      "+="   -> assign Addition
      "-="   -> assign Subtraction
      "*="   -> assign Multiplication
      "/="   -> assign Division
      "%="   -> assign Remainder
      "**="  -> assign Exponentiation
      "|="   -> assign BitwiseOr
      "^="   -> assign BitwiseXor
      "&="   -> assign BitwiseAnd
      _      -> fail $ ForeignError "Unsupported assignment operator"
      where
        assign = pure <<< AssignmentOperator <<< Just

instance writeForeignAssignmentOperator :: WriteForeign AssignmentOperator where
  writeImpl = defaultWriteImpl

derive instance eqAssignmentOperator :: Eq AssignmentOperator
instance showAssignmentOperator :: Show AssignmentOperator where
  show (AssignmentOperator b) = case b of
    Nothing -> "="
    Just b' -> case b' of
      Equals              -> "Unsupported Operator"
      NotEquals           -> "Unsupported Operator"
      Identical           -> "Unsupported Operator"
      NotIdentical        -> "Unsupported Operator"
      LessThan            -> "Unsupported Operator"
      LessThanOrEquals    -> "Unsupported Operator"
      GreaterThan         -> "Unsupported Operator"
      GreaterThanOrEquals -> "Unsupported Operator"
      In                  -> "Unsupported Operator"
      Instanceof          -> "Unsupported Operator"
      Pipe                -> "Unsupported Operator"
      binop               -> show binop <> "="


data BinaryOperator = Equals
                    | NotEquals
                    | Identical
                    | NotIdentical
                    | LessThan
                    | LessThanOrEquals
                    | GreaterThan
                    | GreaterThanOrEquals
                    | LeftShift
                    | RightShift
                    | ZeroFillRightShift
                    | Addition
                    | Subtraction
                    | Multiplication
                    | Division
                    | Remainder
                    | Exponentiation
                    | BitwiseOr
                    | BitwiseXor
                    | BitwiseAnd
                    | In
                    | Instanceof
                    | Pipe

instance readForeignBinaryOperator :: ReadForeign BinaryOperator where
  readImpl f = do
    v <- read' f
    case v of
      "=="         -> pure Equals
      "!="         -> pure NotEquals
      "==="        -> pure Identical
      "!=="        -> pure NotIdentical
      "<"          -> pure LessThan
      "<="         -> pure LessThanOrEquals
      ">"          -> pure GreaterThan
      ">="         -> pure GreaterThanOrEquals
      "<<"         -> pure LeftShift
      ">>"         -> pure RightShift
      ">>>"        -> pure ZeroFillRightShift
      "+"          -> pure Addition
      "-"          -> pure Subtraction
      "*"          -> pure Multiplication
      "/"          -> pure Division
      "%"          -> pure Remainder
      "**"         -> pure Exponentiation
      "|"          -> pure BitwiseOr
      "^"          -> pure BitwiseXor
      "&"          -> pure BitwiseAnd
      "in"         -> pure In
      "instanceof" -> pure Instanceof
      "|>"         -> pure Pipe
      _            -> fail $ ForeignError "Unsupported binary operator"

instance writeForeignBinaryOperator :: WriteForeign BinaryOperator where
  writeImpl = defaultWriteImpl

derive instance eqBinaryOperator :: Eq BinaryOperator
instance showBinaryOperator :: Show BinaryOperator where
  show = case _ of
    Equals              -> "=="
    NotEquals           -> "!="
    Identical           -> "==="
    NotIdentical        -> "!=="
    LessThan            -> "<"
    LessThanOrEquals    -> "<="
    GreaterThan         -> ">"
    GreaterThanOrEquals -> ">="
    LeftShift           -> "<<"
    RightShift          -> ">>"
    ZeroFillRightShift  -> ">>>"
    Addition            -> "+"
    Subtraction         -> "-"
    Multiplication      -> "*"
    Division            -> "/"
    Remainder           -> "%"
    Exponentiation      -> "**"
    BitwiseOr           -> "|"
    BitwiseXor          -> "^"
    BitwiseAnd          -> "&"
    In                  -> "in"
    Instanceof          -> "instanceof"
    Pipe                -> "|>"

data UpdateOperator = Increment | Decrement

instance readForeignUpdateOperator :: ReadForeign UpdateOperator where
  readImpl f = do
    v <- read' f
    case v of
      "++" -> pure Increment
      "--" -> pure Decrement
      _    -> fail $ ForeignError "Unsupported update operator"

instance writeForeignUpdateOperator :: WriteForeign UpdateOperator where
  writeImpl = defaultWriteImpl

derive instance eqUpdateOperator :: Eq UpdateOperator
instance showUpdateOperator :: Show UpdateOperator where
  show Increment = "++"
  show Decrement = "--"

data UnaryOperator = Minus
                   | Plus
                   | Not
                   | BitwiseNot
                   | Typeof
                   | Void
                   | Delete
                   | Throw

derive instance eqUnaryOperator :: Eq UnaryOperator
instance showUnaryOperator :: Show UnaryOperator where
  show = case _ of
    Minus      -> "-"
    Plus       -> "+"
    Not        -> "!"
    BitwiseNot -> "~"
    Typeof     -> "typeof"
    Void       -> "void"
    Delete     -> "delete"
    Throw      -> "throw"

instance readForeignUnaryOperator :: ReadForeign UnaryOperator where
  readImpl f = do
    v <- read' f
    case v of
      "-"      -> pure Minus
      "+"      -> pure Plus
      "!"      -> pure Not
      "~"      -> pure BitwiseNot
      "typeof" -> pure Typeof
      "void"   -> pure Void
      "delete" -> pure Delete
      "throw"  -> pure Throw
      _        -> fail $ ForeignError "Unsupported unary operator"

instance writeForeignUnaryOperator :: WriteForeign UnaryOperator where
  writeImpl = defaultWriteImpl

data MethodKind = Constructor | Get | Set | Method

instance readForeignMethodKind :: ReadForeign MethodKind where
  readImpl f = do
    v <- read' f
    case v of
      "constructor" -> pure Constructor
      "get"         -> pure Get
      "set"         -> pure Set
      "method"      -> pure Method
      _             -> fail $ ForeignError "Unsupported method kind"

instance writeForeignMethodKind :: WriteForeign MethodKind where
  writeImpl = defaultWriteImpl

derive instance genericMethodKind :: Generic MethodKind _
instance showMethodKind :: Show MethodKind where
  show = genericShow

data SourceType = Script | Module

instance readForeignSourceType :: ReadForeign SourceType where
  readImpl f = do
    v <- read' f
    case v of
      "script" -> pure Script
      "module" -> pure Module
      _        -> fail $ ForeignError "Unsupported source type"

instance writeForeignSourceType :: WriteForeign SourceType where
  writeImpl  = defaultWriteImpl

derive instance genericSourceType :: Generic SourceType _
instance showSourceType :: Show SourceType where
  show = genericShow

data VariableKind = Var | Let | Const
derive instance eqVariableKind :: Eq VariableKind

instance readForeignVariableKind :: ReadForeign VariableKind where
  readImpl f = do
    v <- read' f
    case v of
      "var"   -> pure Var
      "let"   -> pure Let
      "const" -> pure Const
      _       -> fail $ ForeignError "Unsupported variable kind"

instance writeForeignVariableKind :: WriteForeign VariableKind where
  writeImpl = defaultWriteImpl

derive instance genericVariableKind :: Generic VariableKind _
instance showVariableKind :: Show VariableKind where
  show = genericShow

foreign import _parse :: forall r. Fn2 String { | r } Foreign

parse :: forall r. String -> { | r } -> F Node
parse s = read' <<< runFn2 _parse s

parse' :: String -> F Node
parse' s = parse s { sourceType: "unambiguous" }

foreign import _parseExpression :: forall r. Fn2 String { | r } Foreign

parseExpression :: forall r. String -> { | r } -> F Node
parseExpression s = read' <<< runFn2 _parseExpression s

parseExpression' :: String -> F Node
parseExpression' s = parseExpression s {}
