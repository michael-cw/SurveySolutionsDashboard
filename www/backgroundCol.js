shinyjs.backgroundCol = function(params) {
  var defaultParams = {
    id : "sampSizeFinal",
    col : "red",
    fon : "20px",
    fonw : "bold",
    fonc : "center",
    bos : "solid",
    boss : "3px"
  };
  params = shinyjs.getParams(params, defaultParams);

  var el = $("#" + params.id);
  el.css("border-color", params.col);
  el.css("font-size", params.fon);
  el.css("font-weight", params.fonw);
  el.css("border-style", params.bos);
  el.css("border-width", params.boss);
  el.css("text-align", params.fonc);
}


shinyjs.backgroundColBox = function(params) {
  var defaultParams = {
    id : "box-header",
    col : "#0D47A1",
    fon : "20px",
    fonw : "bold",
    fonc : "center",
    bos : "solid",
    boss : "3px"
  };
  params = shinyjs.getParams(params, defaultParams);

  var el = $("#" + params.id);
  el.css("border-color", params.col);
  el.css("font-size", params.fon);
  el.css("font-weight", params.fonw);
  el.css("border-style", params.bos);
  el.css("border-width", params.boss);
  el.css("text-align", params.fonc);
}