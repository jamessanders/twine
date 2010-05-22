rstemplate
----------

Simple templates for simple people.


Examples:

    This is a template, this is a {{slot}}
    
Need to loop over a list? You can do it like so:
    
    {@|i <- mylist| 
     I'm inside a loop block, this will be repeated for each item in
     the list, oh and to reference the current item I can do: {{i}}
    @}

The syntax `|i <- mylist|` could be read, "i" in "mylist".    

You can reference the items in an associative list with dot notation as seen below:

    {@|user <- users| Inside the loop I can reference the name attribute like so:
     {{user.name}}, I can reference items in the outer scope like normal: {{slot}}.
    @}
    
While rstemplates are meant to be logic-less it is sometimes useful to have simple logic statements inside templates.
Rstemplate allow this with an conditional block as show below.

    {?|slot| {{slot}} exist! ?}

Very simple expressions are allowed in many places as shown in the examples below.

    {?|(not slot)| there is nothing to show ?}

    {?|(eq? (head mylist) "anything")| The head of mylist is "anything" ?}

    Hello my name is {{(capitalize name)}}.



Full Example
------------

Pretend for this example that we have loaded a json object that looks like the
following and that we have hooked it into out template context.  

    { users: [{ name: "Dave", age: 25, occupation: "plumber" }      
             ,{ name: "Mike", age: 32, occupation: "salesman" }     
             ,{ name: "Pete", age: 14, occupation: "sailor" }     
             ,{ name: "Greg", age: 29, occupation: "programmer"}] } 

Keep in mind one would of course need to parse the json into Haskell and create an instance
of the `toContext` typeclass in order for this to work.     
     
Now we could display the above with the following template.

    {@|user <- users|                             
      {?|(lt? user.age 30)|                       
        Name: {{(capitalize user.name)}}          
        Age:  {{user.age}}                        
        Occupation: {{(upper user.occupation }}   
      ?}                                          
      {?|(not (lt? user.age 30))|                 
        User {{user.name}} is over 30.            
      ?}                                          
     @}                                           
     
     
