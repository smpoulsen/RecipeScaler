# Recipe scaler

Recipe Scaler exists to provide a quick, easy way to scale recipes by their serving size.
It can currently scale using any type of measurement; in the future it will additionally be able to convert units.RecipeScaler

Scale cooking ingredient measurements.

### Installation:
Installable via 
```Bash
$ cabal install
```
This will start it running on localhost:3000

### Use:
Fill in the Recipe Name, initial servings, desired servings, and the recipe following the examples in the placeholder text. Clicking Submit will scale the amounts needed to reach the desired serving size.

Sample recipe:
```
--Recipe
Lemon Poundcake
--Initial Servings
4 
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

#### To-Do:
- Graceful error handling.
- Unit conversions for US measures.

