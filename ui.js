
function toggle(widget,id) {
  var n = document.getElementById(id);
  var oldDisplay = n.style.display;
  var show = !oldDisplay || oldDisplay == "none";
  // HACK: there should be a nicer way to do this
  defaultDisplay = n.tagName == "TR" ? "table-row" : "block";
  n.style.display = show ? defaultDisplay : "none";
  widget.firstChild.data = show ? "-" : "+";
  return false;
}
