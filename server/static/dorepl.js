MathJax.Hub.Config({
    showMathMenu: false,
    ShowMathMenuMSIE: false,
    showProcessingMessages: false,
    messagStyle: "none",
    //menuSettings: {
    //    inTabOrder: false,
    //},
    //"HTML-CSS": {
    //    EqnChunkDelay: 0,
    //},
    //ignoreMMLattributes: {
    //    'onclick': false,
    //},
});

window.onload = function() {
    createCM();
}

var currentCM = null;
function createCM() {
    if (currentCM) {
        currentCM.setOption('readOnly', true);
    }

    currentCM = CodeMirror(document.getElementById('repl'), {
        autofocus: true,
        extraKeys: {
            'Enter': sendToServer,
        },
    });
    //myCodeMirror.setSize(null, 1);

    //myCodeMirror.on('keyup', function(instance, e) {
    //    if (e.code == 'Enter') {
    //        sendToServer(instance);
    //    }
    //});
}

var socket = new WebSocket("ws://127.0.0.1:2794", "rust-websocket");

function sendToServer(cmBox) {
    socket.send(cmBox.getValue());
}

socket.onmessage = function (event) {
    var data = event.data;
    var parts = data.split('@');

    var div = document.createElement('div');
    div.id = 'formula'+parts[0];
    div.innerHTML = parts[1];
    div.className += ' disable-highlight';
    document.getElementById('repl').appendChild(div);

    MathJax.Hub.Queue(["Typeset", MathJax.Hub, div.id]);
    MathJax.Hub.Queue([finishTypesetting, div.id]);

    createCM();
};

function insertIntoCurrrentEquation(elementToInsert) {


    var literaltext = '#(' + elementToInsert.getAttribute('mathTreeNode') + ')';

    var toInsert = document.createElement('span');
    toInsert.className = 'mjx-math mjx-chtml';
    toInsert.appendChild(elementToInsert.cloneNode(true));

    if (!currentCM.somethingSelected()) {
        var place = currentCM.getCursor();

        currentCM.replaceRange(literaltext, place);

        var place2 = currentCM.getCursor();
        console.log(place);
        console.log(place2);
        var x = currentCM.markText(place, place2, { replacedWith: toInsert });
        currentCM.setSelection(place, place2);
    } else {
        var selections = currentCM.listSelections();
        console.log(selections);

        currentCM.replaceSelection(literaltext, 'around');
        var anchor = selections[0].anchor;
        var head = selections[0].head;
        var beg;
        var end;
        if (anchor.line < head.line || anchor.line == head.line && anchor.ch < head.ch) {
            beg = anchor;
            end = head;
        } else {
            beg = head;
            end = anchor;
        }
        currentCM.markText(beg, end, { replacedWith: toInsert });
    }
}

function removeSelection() {
    if (currentCM.somethingSelected()) {
        currentCM.replaceSelection('');
    }
}

var lastClickedID = null;
var curHighlighted = null;

function mathTreeNodeNodeAbove(cur, topLevel) {
    while (!cur.hasAttribute('mathTreeNode')) {
        if (cur === topLevel) {
            return null;
        }
        cur = cur.parentNode;
    }
    return cur;
}

function finishTypesetting(where) {
    var element = document.getElementById(where);

    // Remove any leftover MathML
    var maths = element.getElementsByTagName('math');
    while(maths.length > 0) {
        maths.item(0).remove();
    }


    element.addEventListener("click", function(event) {
        //// Remove the other highlighted stuff
        //var maths = element.getElementsByTagName('*');
        //for (var i=0; i<maths.length; ++i) {
        //    maths[i].removeAttribute('highlighted');
        //}

        if (curHighlighted !== null) {
            curHighlighted.removeAttribute('highlighted');
        }

        var reset = false;
        var closestAbove = mathTreeNodeNodeAbove(event.target, element);
        if (closestAbove === null) {
            reset = true;
        } else {
            var elementToHighlight = null;
            if (closestAbove.id === lastClickedID) {
                elementToHighlight = mathTreeNodeNodeAbove(
                    curHighlighted.parentNode,
                    element);
            } else {
                elementToHighlight = closestAbove;
            }
            if (elementToHighlight === null || elementToHighlight.tagName === 'math') {
                reset = true;
            } else {
                console.log('moo');
                elementToHighlight.setAttribute('highlighted', 'true');

                // TODO: Put in the formula
                insertIntoCurrrentEquation(elementToHighlight);
                lastClickedID = closestAbove.id;
                curHighlighted = elementToHighlight;
            }
        }
        if (reset) {
            removeSelection();

            lastClickedID = null;
            curHighlighted = null;
        }
    }, false);

}
