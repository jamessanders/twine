<h1>Pets by {{author.fname}} {{author.lname}}</h1>
<h2>{{length pets}} total pets</h2>
<table>
  <!--Loop over all our pets -->
  {@|p <- pets| 
          {?|(not (eq? p.species "Fish"))| <!--Don't display fish-->
          <tr>
            <td>{{p.name}}</td>
            <td>{{p.species}}</td>
            <td>{{p.age}} years</td>
          </tr>
          ?}@}
</table>
