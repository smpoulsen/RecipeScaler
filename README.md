#Recipe Scaler

###2014-05-08
A program to scale the measurements for ingredients (specifically for baking).

Measurements are expected to be precise weights in metric units.

Takes two arguments: the starting recipe, and a filename to save the scaled version to.
e.g. 

```Bash
$> Main recipe_in.txt recipe_out.txt
```

Initial recipe example (example_recipe.txt):

```
--Recipe
Lemon Poundcake
--Serving conversion
4 -> 2
--Ingredients
7 g salt
680 g cake flour
142 g cornstarch
21 g baking powder
567 g butter, soft
680 g sugar
28 g lemon zest
907 g eggs, room temperature
```

Blank lines and lines starting with "--" are ignored.
The program looks for the line "x -> y" and uses that to calculate the scaling factor.

Example recipe after conversion:

```
--Recipe
Lemon Poundcake
--Scaled by
0.5
--Ingredients
3.5 g salt
340.0 g cake flour
71.0 g cornstarch
10.5 g baking powder
283.5 g butter, soft
340.0 g sugar
14.0 g lemon zest
453.5 g eggs, room temperature
```