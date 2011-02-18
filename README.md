# twine

Twine is a simple templating engine for haskell that allows developers to reveal just enough information and logic in their templates to get things done.

## The Templating Language

Templates are made up of strings of text and template blocks.  There are five kinds of template blocks, each one starts with a bracket (`{`) and an symbol which indicates the type of block it is (`+`,`{`,`@`,`?`,`|`).  The types of template block are as follows.

### The generic 'slot' block

'Slots' are simply a way of evaluating an expression in a template and having it rendered as a string in ones template, for example.

    Hello user {{user.name}}.
    
The above template contains the text "Hello user" and slot, when the template is evaluated the expression `user.name` is run and the result is rendered into the template.  Expressions themselves are covered below.

### The 'iterator' block

'Iterator' blocks allow one to iterate over a object such as a list, referencing each item in the list further inside the template blockm.  

    Selected Users:
    <ul>
      {@|user <- users|
        <li>{{user.name}} - {{user.age}}</li>
      @}
    </ul>
    
The example above could be a snippet from an html page.  `users` may return a list, the head of the block (`|user <- users|`) indicates each item in the list is brought into scope one by one, bound to variable `user`, for each item in the list the body code block is run and rendered into the template.

### The 'conditional' block

The conditional block works exactly like an 'if' statement does in most programming languages (there is no else statement though).

    {?|gt?(user.age, 21)| 
       Hello user: {{user.name}} 
    ?}
    
In the example above the expression `gt?(user.age, 21)` is evaluated, if the statement is true the following code block is rendered to the template, otherwise it is skipped.  Any statement that return a boolean value can be evaluated in the conditional clause of this block (is the example above `gt?` is a built-in function, more on that below).

### The 'include' block

The include block simply allows one to include other template files in ones template.  The include is done after parsing but before evaluation of the template (so conditional blocks will not effect the include blocks).  Files are included relative to the current template file or current working directory.  See the example below.

    {+ header.tmpl +}
    Hello World
    {+ footer.tmpl +}
    
In the above example the files `header.tmpl` and `footer.tmpl` would be included in the template above before it is evaluated.

### The 'assignment' block

Assignment blocks allow you to assign a new name to some expression, you probably won't use them much but they are occationally handy help simplify certain aspects of templating.  See the example.

    {| numbers = users.enum |}
    
The above example would assign `users.enum` to the name `number`.

## Expressions

Expressions are used in template blocks to accesses the various objects exposed in the templates.  The expression language is very simple; every expression is simply a method call on an object (builtin functions are method calls on the global object).  Every value revealed to the template language is an object that can respond to signals (method calls); for example if we where to reveal a list to twine it is accesible as an object which responds to method calls such as `.length` or `.item(2)`.  There are a number of simple builtin objects available in twine, including the global object which has some useful methods for working with objects, these builtins are listed below in the reference documentation (coming soon).  There are also two types of literals that can currently be used in templates, string literal like "Hello World" and integers such as 1, 2, 3 and 4.

Here a are a few examples of expressions

    {{ users.length }} returns the length the list 'users'
    
    {{ users.item(2).age }} return the age the user at the index        position 2 in the list users.

    {{ not(gt?(users.length, 3)) }} determines if the users list is of length greater then 3 and then negates the boolean returned.


## Interfacing with Haskell

In order to present useful data to a template one must construct a context to evaluate the template in.  This can be done easier with the `ContextWriter` monad which is a simply Writer monad that builds a context.  You can run the `ContextWriter` monad with the `makeContext` function as show in the example below.

    context <- makeContext $ do
      "title"  =: "Test Page"
      "author" =: "James"
      "users"  =: ["Tom", "Pete", "Dave"]
      
    evalTemplate "test.tmpl" context >>= print

The above example would bind three "objects" into our template, then the context is used when evaluating the template "test.tmpl".  Its that simple.

In the example above we bind two String type values and a value of the type [String] into our template but, we can bind any sort of type container into out template so long as it is part of the `ContextBinding` typeclass as shown in the following example...

    data User = User {
        getName :: String,
        getAge  :: Int,
        getOccupation :: String
    }
    
    class (Monad m) => ContextBinding m User where
        binding "name" = return . bind . getName
        binding "age"  = return . bind . getAge
        binding "job"  = return . bind . getOccupation
        makeString _ = return "<User>"
        
In the above example we have a custom type `User` which we make part of the `ContextBinding` typeclass.  When a property is reference by our template for example `{{user.name}}` the `binding` function is called with the property name and the object to lookup the property on, so `user.name` would cause the iterpretor to called `binding "name" user`.  Whatever the binding function returns is what is returned to out template.  When the interpretor is ready to ready to render the template then the `makeString` function is called on the object.  We can now use anything of the type `User` in our context builder as shown below...

    let user = User "Peter" 24 "Monkey Salesman"
    context <- makeContext $ do
        "user" =: user
    evalTemplate "test.tmpl" context
    
Thats all there is to it!
