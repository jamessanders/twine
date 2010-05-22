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
    
While rstemplates are meant to be logicless it is sometimes useful to have simple logic statements inside the template.
Rstemplate allow this with an conditional block as show below.

    {?|slot| {{slot}} exist! ?}

Very simple expressions are allowed in most places where slot names can be used as shown in the examples below.

    {?|(not slot)| there is nothing to show ?}

    {?|(eq? (head mylist) "anything")| The head of mylist is "anything" ?}

    Hello my name is {{(capitalize name)}}.

