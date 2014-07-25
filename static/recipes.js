$(document).ready(function(){
    $("#scaled_recipe").hide();

    $.ajaxSetup({
        beforeSend: function(){
        },
        complete: function(){
        }
    });

    $("#submit_recipe").click(function(){
        var $recipe = new Object();
        var $cleanIngredients = $("#ingredients").val().split("\n").filter(function(d) {return d != ""})
        $recipe['recipeName'] = $("#recipeName").val();
        $recipe['currentServings'] = parseFloat($("#current").val());
        $recipe['desiredServings'] = parseFloat($("#desired").val());
        $recipe['ingredients'] = $.map($cleanIngredients, function(d) { 
            var splits = d.split(" "); 
            return {"amount":parseFloat(splits[0]), "unit":splits[1], "ingredient":splits.slice(2).join(" ")}; 
        });
        var $stringy = JSON.stringify($recipe);

        //alert($stringy);

        $.ajax({
        type: "POST",
        contentType: "application/json; charset=utf-8",
        url: "/recipe_scale",
        data: $stringy,
        success: function(data){
            res = JSON.parse(data)
            //alert(JSON.stringify(data));
            $("#scaled_recipe").empty();
            $("#scaled_recipe").show();
            $("#scaled_recipe").append("<h3>" + res.recipeName + "</h3>");
            if (res.currentServings > 1) {
                var cPlural = "servings"
            } else {
                var cPlural = "serving"
            };
            $("#scaled_recipe").append("<h4>Scaled from " + res.currentServings + " " + cPlural + " to " + res.desiredServings + ".</h4>");
            $.map(res.ingredients, function(d) {
                $("#scaled_recipe").append("<p> " + d.amount + " " + d.unit + " " + d.item + "</p>");
            });
        }
        });
        return false;
    });

    $("#clear_recipe").click(function(){
        $("#scaled_recipe").empty();
        $("#recipeName").val("");
        $("#current").val("");
        $("#desired").val("");
        $("#ingredients").val("");

        return false;
    });

});