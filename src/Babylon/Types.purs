module Babylon.Types
       ( Node(..)
       , Node'
       , ForInStatement'
       , FunctionDeclaration'
       , Function'
       , ObjectProperty'
       , ObjectMember
       , BinaryExpression'
       , CallExpression'
       , ClassDeclaration'
       , Class
       , ImportSpecifier
       , ModuleSpecifier
       , SourceLocation
       , Position
       , LogicalOperator
       , AssignmentOperator
       , BinaryOperator
       , UpdateOperator
       , UnaryOperator(..)
       , MethodKind
       , SourceType
       , VariableKind
       , parseExpression
       ) where

import Prelude

import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(Just, Nothing))


-- data Statement n = ExpressionStatement n
--                  | BlockStatement n
--                  | EmptyStatement n
--                  | DebuggerStatement n
--                  | WithStatement n
--                  | ReturnStatement n
--                  | LabeledStatement n
--                  | BreakStatement n
--                  | ContinueStatement n
--                  | IfStatement n
--                  | SwitchStatement n
--                  | SwitchCase n
--                  | ThrowStatement n
--                  | TryStatement n
--                  | CatchClause n
--                  | WhileStatement n
--                  | DoWhileStatement n
--                  | ForStatement n
--                  | ForInStatement n
--                  | ForOfStatement n
--                  | FunctionDeclaration n
--                  | VariableDeclaration n
--                  | VariableDeclarator n
--                  | Decorator n
--                  | Directive n
--                  | DirectiveLiteral n

-- data Expression n = Super n
--                   | Import n
--                   | ThisExpression n
--                   | ArrowFunctionExpression n
--                   | YieldExpression n
--                   | AwaitExpression n
--                   | ArrayExpression n
--                   | ObjectExpression n
--                   | ObjectProperty n
--                   | ObjectMethod n
--                   | FunctionExpression n
--                   | UnaryExpression n
--                   | UpdateExpression n
--                   | BinaryExpression n
--                   | AssignmentExpression n
--                   | LogicalExpression n
--                   | SpreadElement n
--                   | MemberExpression n
--                   | BindExpression n
--                   | ConditionalExpression n
--                   | CallExpression n
--                   | NewExpression n
--                   | SequenceExpression n
--                   | DoExpression n
--                   | TemplateLiteral n
--                   | TaggedTemplateLiteral n
--                   | TemplateElement n

-- data Literal n = RegExpLiteral n
--                | NullLiteral n
--                | StringLiteral  n
--                | BooleanLiteral n
--                | NumericLiteral n

{-
interface Node {
  type: string;
  loc: SourceLocation | null;
}

interface SourceLocation {
  source: string | null;
  start: Position;
  end: Position;
}

interface Position {
  line: number; // >= 1
  column: number; // >= 0
}

interface Identifier <: Expression, Pattern {
  type: "Identifier";
  name: string;
}


-}

data Node = Identifier  (Node' ( name :: String ))
          | PrivateName (Node' ( id :: Node {- Identifier -}))
            -- Literals
          | RegExpLiteral (Node' ( pattern :: String
                                 , flags :: String
                                 )
                          )
          | NullLiteral    (Node' ())
          | StringLiteral  (Node' ( value :: String ))
          | BooleanLiteral (Node' ( value :: Boolean ))
          | NumericLiteral (Node' ( value :: Number ))
            -- Program
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
          | FunctionDeclaration (FunctionDeclaration' ( id :: Node ))
          | VariableDeclaration (Node' ( declarations :: Array Node -- VariableDeclarator
                                       , kind :: VariableKind
                                       )
                                )
          | VariableDeclarator  (Node' ( id :: Node                 -- Pattern
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
          | ClassDeclaration     (ClassDeclaration' ())
          | ClassExpression      (Class ( id :: Maybe Node                   {- Identifier -}))
          | MetaProperty         (Node' ( meta :: Node                       -- Identifier
                                        , property :: Node                   -- Identifier
                                        )
                                 )
            -- Modules
            -- Imports
          | ImportDeclaration        (Node' ( local :: Node            -- Identifier
                                            , specifiers :: Array Node -- ImportSpecifier | ImportDefaultSpecifier | ImportNamespaceSpecifier
                                            )
                                     )
          | ImportDefaultSpecifier   ImportSpecifier
          | ImportNamespaceSpecifier ImportSpecifier
            -- Exports
          | ExportNamedDeclaration   (Node' ( declaration :: Maybe Node       -- Declaration
                                            , specifiers :: Array Node        -- ExportSpecifier
                                            , source :: Maybe Node            -- Literal
                                            )
                                     )
          | ExportSpecifier          (ModuleSpecifier ( exported :: Node      {- Identifier -}))
          | OptFunctionDeclaration   (FunctionDeclaration' ( id :: Maybe Node {- Identifier -}))
          | OptClassDeclaration      (ClassDeclaration' ( id :: Maybe Node    {- Identifier -}))
          | ExportDefaultDeclaration (Node' (declaration :: Node              {- OptFunctionDeclaration | OptClassDeclaration | Expression -}))
          | ExportAllDeclaration     (Node' ( source :: Node                  {- Literal -}))

instance showNode :: Show Node where
  show = case _ of
    Identifier { name }                      ->
      "(Identifier " <> name <> ")"
    PrivateName { id }                       ->
      "(PrivateName " <> show id <> ")"
    RegExpLiteral { pattern, flags }         ->
      "(RegExpLiteral /" <> pattern <> "/" <> flags <> ")"
    NullLiteral _                            ->
      "NullLiteral"
    StringLiteral { value }                  ->
      "(StringLiteral " <> value <> ")"
    BooleanLiteral { value }                 ->
      "(BooleanLiteral " <> show value <> ")"
    NumericLiteral { value }                 ->
      "(NumericLiteral " <> show value <> ")"
    Program { body }                         ->
      "Program " <> show body
    ExpressionStatement { expression }       ->
      "(Expression " <> show expression <> ")"
    BlockStatement { body }                  ->
      "(BlockStatement " <> show body <> ")"
    EmptyStatement _                         ->
      "EmptyStatement"
    DebuggerStatement _                      ->
      "DebuggerStatement"
    WithStatement { object, body }           ->
      "(WithStatement " <> show object <> ", " <> show body <> ")"
    ReturnStatement { argument }             ->
      "(ReturnStatement " <> show argument <> ")"
    LabeledStatement { label, body }         ->
      "(LabeledStatement " <> show label <> ", " <> show body <> ")"
    BreakStatement { label }                 ->
      "(BreakStatement " <> show label <> ")"
    ContinueStatement { label }              ->
      "(ContinueStatement " <> show label <> ")"
    IfStatement { test
                , consequent
                , alternate
                }                            ->
      "(IfStatement " <> show test
      <> ", " <> show consequent
      <> ", " <> show  alternate
      <> ")"
    SwitchStatement { discriminant, cases }  ->
      "(SwitchStatement " <> show discriminant <> ", " <> show cases <> ")"
    SwitchCase { test, consequent }          ->
      "(SwitchCase " <> show test <> ", " <> show consequent <> ")"
    ThrowStatement { argument }              ->
      "(ThrowStatement " <> show argument <> ")"
    TryStatement { block
                 , handler
                 , finalizer
                 }                           ->
      "(TryStatement " <> show block
      <> ", " <> show handler
      <> ", " <> show finalizer
      <> ")"
    CatchClause { param, body }              ->
      "(CatchClause " <> show param
      <> ", " <> show body
      <> ")"
    WhileStatement { test, body }            ->
      "(WhileStatement " <> show test
      <> ", " <> show body
      <> ")"
    DoWhileStatement { body, test }          ->
      "(DoWhileStatement " <> show body
      <> ", " <> show test
      <> ")"
    ForStatement { init
                 , test
                 , update
                 , body
                 }                           ->
      "(ForStatement " <> show init
      <> ", " <> show test
      <> ", " <> show update
      <> ", " <> show body
      <> ")"
    ForInStatement { left, right, body }     ->
      "(ForInStatement " <> show left
      <> ", " <> show right
      <> ", " <> show body
      <> ")"
    ForOfStatement { left
                   , right
                   , body
                   , await
                   }                         ->
      "(ForOfStatement " <> show left
      <> ", " <> show right
      <> ", " <> show body
      <> ", " <> show await
      <> ")"
    FunctionDeclaration { id
                        , params
                        , body
                        , generator
                        , async
                        }                    ->
      "(FunctionDeclaration " <> show id
      <> ", " <> show params
      <> ", " <> show body
      <> ", " <> show generator
      <> ", " <> show async
      <> ")"
    VariableDeclaration { declarations
                        , kind
                        }                    ->
      "(VariableDeclaration " <> show declarations
      <> ", " <> show kind
      <> ")"
    VariableDeclarator { id, init }          ->
      "(VariableDeclarator " <> show id
      <> ", " <> show init
      <> ")"
    Decorator { expression }                 ->
      "(Decorator " <> show expression <> ")"
    Directive { value }                      ->
      "(Directive " <> show value <> ")"
    DirectiveLiteral { value }               ->
      "(DirectiveLiteral " <> show value <> ")"
    Super _                                  ->
      "Super"
    Import _                                 ->
      "Import"
    ThisExpression _                         ->
      "ThisExpression"
    ArrowFunctionExpression { id
                            , params
                            , body
                            , generator
                            , async
                            }                ->
      "(ArrowFunctionExpression " <> show id
      <> ", " <> show params
      <> ", " <> show body
      <> ", " <> show generator
      <> ", " <> show async
    YieldExpression { argument, delegate }   ->
      "(YieldExpression " <> show argument
      <> ", " <> show delegate
      <> ")"
    AwaitExpression { argument }             ->
      "(AwaitExpression " <> show argument <> ")"
    ArrayExpression { elements }             ->
      "(ArrayExpression " <> show elements <> ")"
    ObjectExpression { properties }          ->
      "(ObjectExpression " <> show properties <> ")"
    ObjectProperty { key
                   , value
                   , computed
                   , shorthand
                   , decorators
                   }                         ->
      "(ObjectProperty " <> show key
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
                 }                           ->
      "(ObjectMethod " <> show key
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
                       }                     ->
      "(FunctionExpression " <> show id
      <> ", " <> show params
      <> ", " <> show body
      <> ", " <> show generator
      <> ", " <> show async
      <> ")"
    UnaryExpression { operator
                    , prefix
                    , argument
                    }                        ->
      "(UnaryExpression " <> show operator
      <> ", " <> show prefix
      <> ", " <> show argument
      <> ")"
    UpdateExpression { operator
                     , argument
                     , prefix
                     }                       ->
      "(UpdateExpression " <> show operator
      <> ", " <> show prefix
      <> ", " <> show argument
      <> ")"
    BinaryExpression { left
                     , operator
                     , right
                     }                       ->
      "(BinaryExpression " <> show left
      <> ", " <> show operator
      <> ", " <> show right
      <> ")"
    AssignmentExpression { left
                         , operator
                         , right
                         }                   ->
      "(AssignmentExpression " <> show left
      <> ", " <> show operator
      <> ", " <> show right
      <> ")"
    LogicalExpression { left
                      , operator
                      , right
                      }                      ->
      "(LogicalExpression " <> show left
      <> ", " <> show operator
      <> ", " <> show right
      <> ")"
    SpreadElement { argument }               ->
      "(SpreadElement " <> show argument <> ")"
    MemberExpression { object
                     , property
                     , computed
                     , optional
                     }                       ->
      "(MemberExpression " <> show object
      <> ", " <> show property
      <> ", " <> show computed
      <> ", " <> show optional
      <> ")"
    BindExpression { object, callee }        ->
      "(BindExpression " <> show object
      <> ", " <> show callee
      <> ")"
    ConditionalExpression { test
                          , alternate
                          , consequent
                          }                  ->
      "(ConditionalExpression " <> show test
      <> ", " <> show alternate
      <> ", " <> show consequent
      <> ")"
    CallExpression { callee
                   , arguments
                   , optional
                   }                         ->
      "(CallExpression " <> show callee
      <> ", " <> show arguments
      <> ", " <> show optional
      <> ")"
    NewExpression { callee
                  , arguments
                  , optional
                  }                          ->
      "(NewExpression " <> show callee
      <> ", " <> show arguments
      <> ", " <> show optional
      <> ")"
    SequenceExpression { expressions }       ->
      "(SequenceExpression " <> show expressions <> ")"
    DoExpression { body }                    ->
      "(DoExpression " <> show body <> ")"
    TemplateLiteral { quasis, expressions }  ->
      "(TemplateLiteral " <> show quasis
      <> ", " <> show expressions
      <> ")"
    TaggedTemplateLiteral { tag, quasi }     ->
      "(TaggedTemplateLiteral " <> show tag
      <> ", " <> show quasi
      <> ")"
    TemplateElement { tail
                    , value: { cooked
                             , raw
                             }
                    }                        ->
      "(TemplateElement " <> show tail
      <> ", cooked: " <> show cooked
      <> ", raw: " <> show raw
      <> ")"
    AssignmentProperty { key
                       , value
                       , computed
                       , shorthand
                       , decorators
                       }                     ->
      "(AssignmentProperty " <> show key
      <> ", " <> show value
      <> ", " <> show computed
      <> ", " <> show shorthand
      <> ", " <> show decorators
      <> ")"
    ObjectPattern { properties }             ->
      "(ObjectPattern " <> show properties <> ")"
    ArrayPattern { elements }                ->
      "(ArrayPattern " <> show elements <> ")"
    RestElement { argument }                 ->
      "(RestElement " <> show argument <> ")"
    AssignmentPattern { left, right }        ->
      "(AssignmentPattern " <> show left
      <> ", " <> show right
      <> ")"
    ClassBody { body }                       ->
      "(ClassBody " <> show body <> ")"
    ClassMethod { key
                , params
                , body
                , kind
                , computed
                , static
                , decorators
                , generator
                , async
                }                            ->
      "(ClassMethod " <> show key
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
                       }                     ->
      "(ClassMethod " <> show key
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
                  }                          ->
      "(ClassProperty " <> show key
      <> ", " <> show value
      <> ", " <> show static
      <> ", " <> show computed
      <> ")"
    ClassPrivateProperty { key
                         , value
                         , static
                         }                   ->
      "(ClassPrivateProperty " <> show key
      <> ", " <> show value
      <> ", " <> show static
      <> ")"
    ClassDeclaration { id
                     , superClass
                     , body
                     , decorators
                     }                       ->
      "(ClassDeclaration " <> show id
      <> ", " <> show superClass
      <> ", " <> show body
      <> ", " <> show decorators
      <> ")"
    ClassExpression { id
                    , superClass
                    , body
                    , decorators
                    }                        ->
      "(ClassExpression " <> show id
      <> ", " <> show superClass
      <> ", " <> show body
      <> ", " <> show decorators
      <> ")"
    MetaProperty { meta, property }          ->
      "(MetaProperty " <> show meta
      <> ", " <> show property
      <> ")"
    ImportDeclaration { local, specifiers }  ->
      "(ImportDeclaration " <> show local
      <> ", " <> show specifiers
      <> ")"
    ImportDefaultSpecifier { local
                           , imported
                           }                 ->
      "(ImportDefaultSpecifier " <> show local
      <> ", " <> show imported
      <> ")"
    ImportNamespaceSpecifier { local
                             , imported
                             }               ->
      "(ImportNamespaceSpecifier " <> show local
      <> ", " <> show imported
      <> ")"
    ExportNamedDeclaration { declaration
                           , specifiers
                           , source
                           }                 ->
      "(ExportNamedDeclaration " <> show declaration
      <> ", " <> show specifiers
      <> ", " <> show source
      <> ")"
    ExportSpecifier { local, exported }      ->
      "(ExportSpecifier " <> show local
      <> ", " <> show exported
      <> ")"
    OptFunctionDeclaration { id
                           , params
                           , body
                           , generator
                           , async
                           }                 ->
      "(OptFunctionDeclaration " <> show id
      <> ", " <> show params
      <> ", " <> show body
      <> ", " <> show generator
      <> ", " <> show async
      <> ")"
    OptClassDeclaration { id
                        , superClass
                        , body
                        , decorators
                        }                    ->
      "(OptClassDeclaration " <> show id
      <> ", " <> show superClass
      <> ", " <> show body
      <> ", " <> show decorators
      <> ")"
    ExportDefaultDeclaration { declaration } ->
      "(ExportDefaultDeclaration " <> show declaration <> ")"
    ExportAllDeclaration { source }          ->
      "(ExportAllDeclaration " <> show source <> ")"

type Position = { line :: Int
                , column :: Int
                }

type SourceLocation = { source :: Maybe String
                      , start :: Position
                      , end :: Position
                      }

type Node' r = { loc :: Maybe SourceLocation | r }

type ClassDeclaration' r = Class ( id :: Node {- Identifier -} | r)

type FunctionDeclaration' r = Node' (Function' ( id :: Node {- Identifier -} | r))

type Class r = Node' ( id :: Node               -- Identifier
                     , superClass :: Maybe Node -- Expression
                     , body :: Node             -- ClassBody
                     , decorators :: Array Node -- Decorator
                     | r
                     )


type CallExpression' = Node' ( callee :: Node          -- Expression | Super | Import
                             , arguments :: Array Node -- Expression | SpreadElement
                             , optional :: Maybe Boolean
                             )

type BinaryExpression' r = Node' ( left :: Node  -- Expression
                                 , right :: Node -- Expression
                                 | r
                                 )

type ObjectProperty' = ObjectMember ( shorthand :: Boolean
                                    , value :: Node -- Expression
                                    )

type ObjectMember r = Node' ( key :: Node              -- Expression
                            , computed :: Boolean
                            , decorators :: Array Node -- Decorator
                            | r
                            )

type ForInStatement' r = Node' ( left :: Node       -- VariableDeclaration | Expression
                               , right :: Node      -- Expression
                               , body :: Node       -- Statements
                               | r
                               )

type Function' r = ( params :: Array Node -- Pattern
                   , generator :: Boolean
                   , body :: Node         -- BlockStatement | Expression
                   , async :: Boolean
                   | r
                   )

type ImportSpecifier = ModuleSpecifier ( imported :: Node {- Identifier -})

type ModuleSpecifier r = Node' ( local :: Node | r )

data LogicalOperator = Or | And

derive instance eqLogicalOperator :: Eq LogicalOperator
instance showLogicalOperator :: Show LogicalOperator where
  show Or  = "||"
  show And = "&&"

data AssignmentOperator = AssignmentOperator (Maybe BinaryOperator)

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

data MethodKind = Constructor | Get | Set | Method

derive instance genericMethodKind :: Generic MethodKind _
instance showMethodKind :: Show MethodKind where
  show = genericShow

data SourceType = Script | Module

derive instance genericSourceType :: Generic SourceType _
instance showSourceType :: Show SourceType where
  show = genericShow

data VariableKind = Var | Let | Const

derive instance genericVariableKind :: Generic VariableKind _
instance showVariableKind :: Show VariableKind where
  show = genericShow

foreign import data BabelAST :: Type

foreign import _parseExpression :: String -> BabelAST

parseExpression :: String -> Node
parseExpression src = convert $ _parseExpression src
  where
    convert ({ type: "ArrowFunctionExpression", params, body, generator, async }) =
      ArrowFunctionExpression { loc: Nothing
                              , params: convert <$> params
                              , body: convert body
                              , generator
                              , async
                              , id: Nothing
                              }
    convert ({ type: "BlockStatement", body, directives }) =
      BlockStatement { loc: Nothing
                     , body: convert <$> body
                     , directives: convert <$> directives
                     }

    convert ({ type: "Identifier", name }) =
      Identifier { loc: Nothing
                 , name
                 }
    convert ({ type: "BooleanLiteral", value }) =
      BooleanLiteral { loc: Nothing
                     , value: value
                     }
