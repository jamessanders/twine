rstemplate
----------

Simple templates for simple people.


Example:

    This is a template, this is a slot {{slot1}}
    
    Need to loop over a list? You can do it like so:
    
    {@|mylist| I'm inside a loop block, this will be repeated for each item in
    the list, oh and to reference the current item I can do: {{_}}
    I can spit out a number like so: {{#}} @}
    
    If the list is list of associative list you can reference the items like
    so:
    {@|mylist| Inside the loop I can reference the name attribute like so:
    {{mylist.name}}, I can reference items in the outer scope like normal:
    {{slot}}.@}
    
    Sometimes you only want to output a block of text if a context item exists
    you can do so like this:  {?|slot1| This block will only be output if
    slot1 exits?}
    

    
    
    
