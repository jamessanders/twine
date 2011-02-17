# twine

Simple templates for simple people.

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

