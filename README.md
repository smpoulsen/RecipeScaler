#RecipeScaler

Scale cooking ingredient measurements.

###Installation:
Installable via 
```Bash
$ cabal install
```

###Use:
Measurements are expected to be either weights in metric units, or <a href=https://en.wikipedia.org/wiki/Cooking_weights_and_measures#United_States_measures>US cutomary units</a>.

Takes two arguments: the starting recipe, and a filename to save the scaled version to.
e.g. 

```Bash
$ RecipeScaler recipe_in.txt recipe_out.txt
```

In the input, blank lines and lines starting with "--" are ignored.
The line "x -> y" is used to calculate the scaling factor.

Input example (example_recipe.txt):

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

Output example (recipe after scaling ingredients):

```
--Recipe
Lemon Poundcake
--Servings:
Scaled original by 0.5 to get 2.0 serving(s).
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

####To-Do:
- Graceful error handling.
- Unit conversions for US measures.