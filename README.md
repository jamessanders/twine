rstemplate
----------

Simple templates for simple people.


Examples:

    This is a template, this is a slot {{slot1}}
    
Need to loop over a list? You can do it like so:
    
    {@|l <- mylist| 
     I'm inside a loop block, this will be repeated for each item in
     the list, oh and to reference the current item I can do: {{l}}
     I can spit out a number like so: {{#}} 
    @}
    
If the list is list of associative list you can reference the items like so:

    {@|l <- mylist| Inside the loop I can reference the name attribute like so:
     {{l.name}}, I can reference items in the outer scope like normal: {{slot}}.
    @}
    
The syntax `|l <- mylist|` could be read, "l" in "mylist".
    
Sometimes you only want to output a block of text if a context item exists you can do so like this:  
    {?|slot1| This block will only be output if slot1 exits?}
    

    
    
    
