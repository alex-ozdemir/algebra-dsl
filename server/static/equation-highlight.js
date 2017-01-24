MathJax.Hub.Config({
    showMathMenu: false,
    ShowMathMenuMSIE: false,
    showProcessingMessages: false,
    messagStyle: "none",
    //"HTML-CSS": {
    //    EqnChunkDelay: 0,
    //},
    //ignoreMMLattributes: {
    //    'onclick': false,
    //},
});

var lastClickedID = null;
var curHighlighted = null;

function mathTreeNodeNodeAbove(cur, topLevel) {
  while(!cur.classList.contains('mathTreeNode')) {
    if (cur === topLevel) {
      return null;
    }
    cur = cur.parentNode;
  }
  return cur;
}

window.onload = function() {
  document.getElementById("formula").addEventListener("click", function(event) {
    // Remove any leftover MathML
    var maths = document.getElementsByTagName('math');
    while(maths.length > 0) {
      maths.item(0).remove();
    }

    var maths = document.getElementsByClassName('mathTreeNode');
    for (var i=0; i<maths.length; ++i) {
      maths[i].style.border = 'none';
    }

    var reset = false;
    var closestAbove = mathTreeNodeNodeAbove(
        event.target, document.getElementById('formula'));
    if (closestAbove === null) {
      reset = true;
    } else {
      var elementToHighlight = null;
      if (closestAbove.id === lastClickedID) {
        elementToHighlight = mathTreeNodeNodeAbove(
          curHighlighted.parentNode,
          document.getElementById('formula'));
      } else {
        elementToHighlight = closestAbove;
      }
      if (elementToHighlight === null || elementToHighlight.tagName === 'math') {
        reset = true;
      } else {
        elementToHighlight.style.border = 'solid';
        elementToHighlight.style.borderWidth = '1px';
        elementToHighlight.style.borderColor = 'green';

        document.getElementById("jstext").innerHTML = elementToHighlight.id;
        lastClickedID = closestAbove.id;
        curHighlighted = elementToHighlight;
      }
    }
    if (reset) {
      document.getElementById("jstext").innerHTML = ''
      lastClickedID = null;
      curHighlighted = null;
    }
  }, false);
};


var socket = new WebSocket("ws://127.0.0.1:2794", "rust-websocket");
socket.onmessage = function (event) {
  document.getElementById('formula').innerHTML = event.data;
  MathJax.Hub.Queue(["Typeset", MathJax.Hub, "formula"]);
};
function sendJSText() {
  socket.send(document.getElementById("jstext").innerHTML);
}
