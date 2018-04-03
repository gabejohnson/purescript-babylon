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

import Data.Foreign (F, Foreign, ForeignError(..), fail)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(Just, Nothing))
import Simple.JSON (class ReadForeign, read')


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
          | ObjectExpression        (Node' ( properties :: Array Node       {- ObjectProperty | ObjectMethod | SpreadElemnt] -}))
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
                                                   , decorators :: Array Node -- Decorator
                                                   )
                                        )
                                 )
          | ClassPrivateMethod   (Node' (Function' ( key :: Node             -- PrivateName
                                                   , kind :: MethodKind
                                                   , static :: Boolean
                                                   , decorators :: Array Node -- Decorator
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
  show = case _ of
    Identifier { name }                      -> "(Identifier " <> name <> ")"
    PrivateName { id }                       -> "(PrivateName " <> show id <> ")"
    RegExpLiteral { pattern, flags }         -> "(RegExpLiteral /" <> pattern <> "/" <> flags <> ")"
    NullLiteral _                            -> "NullLiteral"
    StringLiteral { value }                  -> "(StringLiteral " <> value <> ")"
    BooleanLiteral { value }                 -> "(BooleanLiteral " <> show value <> ")"
    NumericLiteral { value }                 -> "(NumericLiteral " <> show value <> ")"
    File { program }                         -> "File " <> show program
    Program { body }                         -> "(Program " <> show body <> ")"
    ExpressionStatement { expression }       -> "(Expression " <> show expression <> ")"
    BlockStatement { body }                  -> "(BlockStatement " <> show body <> ")"
    EmptyStatement _                         -> "EmptyStatement"
    DebuggerStatement _                      -> "DebuggerStatement"
    WithStatement { object, body }           -> "(WithStatement " <> show object
                                                <> ", " <> show body
                                                <> ")"
    ReturnStatement { argument }             -> "(ReturnStatement " <> show argument <> ")"
    LabeledStatement { label, body }         -> "(LabeledStatement " <> show label
                                                <> ", " <> show body
                                                <> ")"
    BreakStatement { label }                 -> "(BreakStatement " <> show label <> ")"
    ContinueStatement { label }              -> "(ContinueStatement " <> show label <> ")"
    IfStatement { test
                , consequent
                , alternate
                }                            -> "(IfStatement " <> show test
                                                <> ", " <> show consequent
                                                <> ", " <> show  alternate
                                                <> ")"
    SwitchStatement { discriminant, cases }  -> "(SwitchStatement " <> show discriminant
                                                <> ", " <> show cases
                                                <> ")"
    SwitchCase { test, consequent }          -> "(SwitchCase " <> show test
                                                <> ", " <> show consequent
                                                <> ")"
    ThrowStatement { argument }              -> "(ThrowStatement " <> show argument <> ")"
    TryStatement { block
                 , handler
                 , finalizer
                 }                           -> "(TryStatement " <> show block
                                                <> ", " <> show handler
                                                <> ", " <> show finalizer
                                                <> ")"
    CatchClause { param, body }              -> "(CatchClause " <> show param
                                                <> ", " <> show body
                                                <> ")"
    WhileStatement { test, body }            -> "(WhileStatement " <> show test
                                                <> ", " <> show body
                                                <> ")"
    DoWhileStatement { body, test }          -> "(DoWhileStatement " <> show body
                                                <> ", " <> show test
                                                <> ")"
    ForStatement { init
                 , test
                 , update
                 , body
                 }                           -> "(ForStatement " <> show init
                                                <> ", " <> show test
                                                <> ", " <> show update
                                                <> ", " <> show body
                                                <> ")"
    ForInStatement { left, right, body }     -> "(ForInStatement " <> show left
                                                <> ", " <> show right
                                                <> ", " <> show body
                                                <> ")"
    ForOfStatement { left
                   , right
                   , body
                   , await
                   }                         -> "(ForOfStatement " <> show left
                                                <> ", " <> show right
                                                <> ", " <> show body
                                                <> ", " <> show await
                                                <> ")"
    FunctionDeclaration { id
                        , params
                        , body
                        , generator
                        , async
                        }                    -> "(FunctionDeclaration " <> show id
                                                <> ", " <> show params
                                                <> ", " <> show body
                                                <> ", " <> show generator
                                                <> ", " <> show async
                                                <> ")"
    VariableDeclaration { declarations
                        , kind
                        }                    -> "(VariableDeclaration " <> show declarations
                                                <> ", " <> show kind
                                                <> ")"
    VariableDeclarator { id, init }          -> "(VariableDeclarator " <> show id
                                                <> ", " <> show init
                                                <> ")"
    Decorator { expression }                 -> "(Decorator " <> show expression <> ")"
    Directive { value }                      -> "(Directive " <> show value <> ")"
    DirectiveLiteral { value }               -> "(DirectiveLiteral " <> show value <> ")"
    Super _                                  -> "Super"
    Import _                                 -> "Import"
    ThisExpression _                         -> "ThisExpression"
    ArrowFunctionExpression { id
                            , params
                            , body
                            , generator
                            , async
                            }                -> "(ArrowFunctionExpression " <> show id
                                                <> ", " <> show params
                                                <> ", " <> show body
                                                <> ", " <> show generator
                                                <> ", " <> show async
                                                <> ")"
    YieldExpression { argument, delegate }   -> "(YieldExpression " <> show argument
                                                <> ", " <> show delegate
                                                <> ")"
    AwaitExpression { argument }             -> "(AwaitExpression " <> show argument <> ")"
    ArrayExpression { elements }             -> "(ArrayExpression " <> show elements <> ")"
    ObjectExpression { properties }          -> "(ObjectExpression " <> show properties <> ")"
    ObjectProperty { key
                   , value
                   , computed
                   , shorthand
                   , decorators
                   }                         -> "(ObjectProperty " <> show key
                                                <> ", " <> show key
                                                <> ", " <> show value
                                                <> ", " <> show computed
                                                <> ", " <> show shorthand
                                                <> ", " <> show decorators
                                                <> ")"
    ObjectMethod { key
                 , params
                 , body
                 , computed
                 , decorators
                 , generator
                 , async
                 }                           -> "(ObjectMethod " <> show key
                                                <> ", " <> show params
                                                <> ", " <> show body
                                                <> ", " <> show computed
                                                <> ", " <> show decorators
                                                <> ", " <> show generator
                                                <> ", " <> show async
                                                <> ")"
    FunctionExpression { id
                       , params
                       , body
                       , generator
                       , async
                       }                     -> "(FunctionExpression " <> show id
                                                <> ", " <> show params
                                                <> ", " <> show body
                                                <> ", " <> show generator
                                                <> ", " <> show async
                                                <> ")"
    UnaryExpression { operator
                    , prefix
                    , argument
                    }                        -> "(UnaryExpression " <> show operator
                                                <> ", " <> show prefix
                                                <> ", " <> show argument
                                                <> ")"
    UpdateExpression { operator
                     , argument
                     , prefix
                     }                       -> "(UpdateExpression " <> show operator
                                                <> ", " <> show prefix
                                                <> ", " <> show argument
                                                <> ")"
    BinaryExpression { left
                     , operator
                     , right
                     }                       -> "(BinaryExpression " <> show left
                                                <> ", " <> show operator
                                                <> ", " <> show right
                                                <> ")"
    AssignmentExpression { left
                         , operator
                         , right
                         }                   -> "(AssignmentExpression " <> show left
                                                <> ", " <> show operator
                                                <> ", " <> show right
                                                <> ")"
    LogicalExpression { left
                      , operator
                      , right
                      }                      -> "(LogicalExpression " <> show left
                                                <> ", " <> show operator
                                                <> ", " <> show right
                                                <> ")"
    SpreadElement { argument }               -> "(SpreadElement " <> show argument <> ")"
    MemberExpression { object
                     , property
                     , computed
                     , optional
                     }                       -> "(MemberExpression " <> show object
                                                <> ", " <> show property
                                                <> ", " <> show computed
                                                <> ", " <> show optional
                                                <> ")"
    BindExpression { object, callee }        -> "(BindExpression " <> show object
                                                <> ", " <> show callee
                                                <> ")"
    ConditionalExpression { test
                          , alternate
                          , consequent
                          }                  -> "(ConditionalExpression " <> show test
                                                <> ", " <> show alternate
                                                <> ", " <> show consequent
                                                <> ")"
    CallExpression { callee
                   , arguments
                   , optional
                   }                         -> "(CallExpression " <> show callee
                                                <> ", " <> show arguments
                                                <> ", " <> show optional
                                                <> ")"
    NewExpression { callee
                  , arguments
                  , optional
                  }                          -> "(NewExpression " <> show callee
                                                <> ", " <> show arguments
                                                <> ", " <> show optional
                                                <> ")"
    SequenceExpression { expressions }       -> "(SequenceExpression " <> show expressions <> ")"
    DoExpression { body }                    -> "(DoExpression " <> show body <> ")"
    TemplateLiteral { quasis, expressions }  -> "(TemplateLiteral " <> show quasis
                                                <> ", " <> show expressions
                                                <> ")"
    TaggedTemplateLiteral { tag, quasi }     -> "(TaggedTemplateLiteral " <> show tag
                                                <> ", " <> show quasi
                                                <> ")"
    TemplateElement { tail
                    , value: { cooked
                             , raw
                             }
                    }                        -> "(TemplateElement " <> show tail
                                                <> ", cooked: " <> show cooked
                                                <> ", raw: " <> show raw
                                                <> ")"
    AssignmentProperty { key
                       , value
                       , computed
                       , shorthand
                       , decorators
                       }                     -> "(AssignmentProperty " <> show key
                                                <> ", " <> show value
                                                <> ", " <> show computed
                                                <> ", " <> show shorthand
                                                <> ", " <> show decorators
                                                <> ")"
    ObjectPattern { properties }             -> "(ObjectPattern " <> show properties <> ")"
    ArrayPattern { elements }                -> "(ArrayPattern " <> show elements <> ")"
    RestElement { argument }                 -> "(RestElement " <> show argument <> ")"
    AssignmentPattern { left, right }        -> "(AssignmentPattern " <> show left
                                                <> ", " <> show right
                                                <> ")"
    ClassBody { body }                       -> "(ClassBody " <> show body <> ")"
    ClassMethod { key
                , params
                , body
                , kind
                , computed
                , static
                , decorators
                , generator
                , async
                }                            -> "(ClassMethod " <> show key
                                                <> ", " <> show params
                                                <> ", " <> show body
                                                <> ", " <> show kind
                                                <> ", " <> show computed
                                                <> ", " <> show static
                                                <> ", " <> show decorators
                                                <> ", " <> show generator
                                                <> ", " <> show async
                                                <> ")"
    ClassPrivateMethod { key
                       , params
                       , body
                       , kind
                       , static
                       , decorators
                       , generator
                       , async
                       }                     -> "(ClassMethod " <> show key
                                                <> ", " <> show params
                                                <> ", " <> show body
                                                <> ", " <> show kind
                                                <> ", " <> show static
                                                <> ", " <> show decorators
                                                <> ", " <> show generator
                                                <> ", " <> show async
                                                <> ")"
    ClassProperty { key
                  , value
                  , static
                  , computed
                  }                          -> "(ClassProperty " <> show key
                                                <> ", " <> show value
                                                <> ", " <> show static
                                                <> ", " <> show computed
                                                <> ")"
    ClassPrivateProperty { key
                         , value
                         , static
                         }                   -> "(ClassPrivateProperty " <> show key
                                                <> ", " <> show value
                                                <> ", " <> show static
                                                <> ")"
    ClassDeclaration { id
                     , superClass
                     , body
                     , decorators
                     }                       -> "(ClassDeclaration " <> show id
                                                <> ", " <> show superClass
                                                <> ", " <> show body
                                                <> ", " <> show decorators
                                                <> ")"
    ClassExpression { id
                    , superClass
                    , body
                    , decorators
                    }                        -> "(ClassExpression " <> show id
                                                <> ", " <> show superClass
                                                <> ", " <> show body
                                                <> ", " <> show decorators
                                                <> ")"
    MetaProperty { meta, property }          -> "(MetaProperty " <> show meta
                                                <> ", " <> show property
                                                <> ")"
    ImportDeclaration { source, specifiers }  -> "(ImportDeclaration " <> show source
                                                <> ", " <> show specifiers
                                                <> ")"
    ImportSpecifier { local
                    , imported
                    }                         -> "(ImportSpecifier " <> show local
                                                 <> ", " <> show imported
                                                 <> ")"
    ImportDefaultSpecifier { local }          -> "(ImportDefaultSpecifier " <> show local <> ")"
    ImportNamespaceSpecifier { local }        -> "(ImportNamespaceSpecifier " <> show local <> ")"
    ExportNamedDeclaration { declaration
                           , specifiers
                           , source
                           }                 -> "(ExportNamedDeclaration " <> show declaration
                                                <> ", " <> show specifiers
                                                <> ", " <> show source
                                                <> ")"
    ExportSpecifier { local, exported }      -> "(ExportSpecifier " <> show local
                                                <> ", " <> show exported
                                                <> ")"
    OptFunctionDeclaration { id
                           , params
                           , body
                           , generator
                           , async
                           }                 -> "(OptFunctionDeclaration " <> show id
                                                <> ", " <> show params
                                                <> ", " <> show body
                                                <> ", " <> show generator
                                                <> ", " <> show async
                                                <> ")"
    OptClassDeclaration { id
                        , superClass
                        , body
                        , decorators
                        }                    -> "(OptClassDeclaration " <> show id
                                                <> ", " <> show superClass
                                                <> ", " <> show body
                                                <> ", " <> show decorators
                                                <> ")"
    ExportDefaultDeclaration { declaration } -> "(ExportDefaultDeclaration " <> show declaration <> ")"
    ExportAllDeclaration { source }          -> "(ExportAllDeclaration " <> show source <> ")"

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
                                 , decorators :: Array Node -- Decorator
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

derive instance genericSourceType :: Generic SourceType _
instance showSourceType :: Show SourceType where
  show = genericShow

data VariableKind = Var | Let | Const

instance readForeignVariableKind :: ReadForeign VariableKind where
  readImpl f = do
    v <- read' f
    case v of
      "var"   -> pure Var
      "let"   -> pure Let
      "const" -> pure Const
      _       -> fail $ ForeignError "Unsupported variable kind"

derive instance genericVariableKind :: Generic VariableKind _
instance showVariableKind :: Show VariableKind where
  show = genericShow

foreign import _parse :: Fn2 String (Array String) Foreign

parse :: String -> Array String -> F Node
parse s = read' <<< runFn2 _parse s

parse' :: String -> F Node
parse' s = parse s []

foreign import _parseExpression :: Fn2 String (Array String) Foreign

parseExpression :: String -> Array String -> F Node
parseExpression s = read' <<< runFn2 _parseExpression s

parseExpression' :: String -> F Node
parseExpression' s = parseExpression s []
