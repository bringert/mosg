
function toggle(id) {
  var oldDisplay = document.getElementById(id).style.display;
  var newDisplay = oldDisplay == "none" ? "table-row" : "none";
  document.getElementById(id).style.display = newDisplay;
}
